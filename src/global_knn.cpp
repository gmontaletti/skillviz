// Fused Jaccard similarity + top-k selection for .global_knn_vote().
//
// The R implementation this replaces looped per test row over that row's
// non-zero candidates, which put the ESCO-free rescue path at ~92% of the
// imputation runtime on the 24-month window (18221s of 19856s).
//
// A first attempt kept the R structure -- Matrix::tcrossprod() to build the
// intersection, then a C++ top-k over it -- and came out SLOWER than the R
// original. Profiling showed why: on a 3000 x 112k slice the product carries
// 221M non-zeros (~73.5k candidates per test row), so materialising it cost
// 5.2s and scanning it cost more than the R loop saved.
//
// This version never builds the product. It walks an inverted index instead:
// for each test row, for each of its skills, bump an accumulator over the train
// rows carrying that skill. Only the touched entries are scored. Memory is one
// reused int array of n_train, and the intersection is never allocated.
//
// Only the SEARCH is here. The vote stays in R: its tie-break depends on
// grouping order and it touches at most k elements, so moving it would risk
// changing predictions for no gain.

#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// Selection semantics reproduced exactly from the R reference, quirks included,
// because they feed the vote's tie-break:
//
//   n <= k        -> all candidates, in ASCENDING train index order
//   otherwise     -> cut = k-th largest similarity;
//                    sel = every candidate with sim >= cut;
//                    |sel| == k -> those k, in ASCENDING train index order
//                    |sel| >  k -> top k by (sim desc, index asc)
//
// The set selected is the same either way; only the ORDER differs, and that
// asymmetry decides which CP4 wins a tied vote downstream.
//
// tp/ti  : train matrix in CSC, n_train x n_vocab -- p indexes SKILLS, i gives
//          the train rows carrying each skill. That is the inverted index.
// qp/qi  : transposed test matrix, n_vocab x n_test -- p indexes TEST ROWS, i
//          gives the skills each test row carries.
//
// [[Rcpp::export]]
List global_topk(IntegerVector tp,
                 IntegerVector ti,
                 IntegerVector qp,
                 IntegerVector qi,
                 NumericVector rs_test,
                 NumericVector rs_train,
                 LogicalVector cp4_ok,
                 int n_train,
                 int k) {
  const int n_test = qp.size() - 1;

  const int* TP = INTEGER(tp);
  const int* TI = INTEGER(ti);
  const int* QP = INTEGER(qp);
  const int* QI = INTEGER(qi);
  const double* RST = REAL(rs_test);
  const double* RSTR = REAL(rs_train);

  // Raw copy of the validity mask: LogicalVector element access is checked and
  // this is read once per touched candidate.
  std::vector<char> ok(n_train);
  for (int i = 0; i < n_train; ++i) ok[i] = (cp4_ok[i] == TRUE) ? 1 : 0;

  std::vector<int> acc(n_train, 0);   // intersection counts, reused
  std::vector<int> touched;           // train rows hit by this test row
  std::vector<int> cand;              // surviving candidate train rows
  std::vector<double> sim;            // their similarities
  std::vector<double> buf;            // reused scratch for nth_element
  std::vector<int> sel;               // selected positions into cand/sim

  std::vector<int> o_col, o_idx;
  std::vector<double> o_sim;
  o_col.reserve((size_t)n_test * (size_t)k);
  o_idx.reserve((size_t)n_test * (size_t)k);
  o_sim.reserve((size_t)n_test * (size_t)k);

  for (int j = 0; j < n_test; ++j) {
    touched.clear();

    // Accumulate intersections over the inverted index.
    for (int s = QP[j]; s < QP[j + 1]; ++s) {
      const int skill = QI[s];
      for (int t = TP[skill]; t < TP[skill + 1]; ++t) {
        const int r = TI[t];
        if (acc[r]++ == 0) touched.push_back(r);
      }
    }
    if (touched.empty()) continue;

    // Score, dropping zero-similarity and NA-CP4 neighbours. Reset as we go so
    // acc returns to all-zero without a second sweep.
    const double a = RST[j];
    cand.clear();
    sim.clear();
    for (size_t t = 0; t < touched.size(); ++t) {
      const int r = touched[t];
      const double inter = (double)acc[r];
      acc[r] = 0;
      if (!ok[r]) continue;
      const double uni = a + RSTR[r] - inter;
      const double s = (uni > 0.0) ? inter / uni : 0.0;
      if (s <= 0.0) continue;
      cand.push_back(r);
      sim.push_back(s);
    }

    const int n = (int)sim.size();
    if (n == 0) continue;

    sel.clear();
    bool order_by_index;

    if (n <= k) {
      for (int t = 0; t < n; ++t) sel.push_back(t);
      order_by_index = true;
    } else {
      buf.assign(sim.begin(), sim.end());
      std::nth_element(buf.begin(), buf.begin() + (n - k), buf.end());
      const double cut = buf[n - k];
      for (int t = 0; t < n; ++t) {
        if (sim[t] >= cut) sel.push_back(t);
      }
      if ((int)sel.size() > k) {
        // Top k by (sim desc, index asc).
        std::partial_sort(
          sel.begin(), sel.begin() + k, sel.end(),
          [&sim, &cand](int p, int q) {
            if (sim[p] != sim[q]) return sim[p] > sim[q];
            return cand[p] < cand[q];
          });
        sel.resize(k);
        order_by_index = false;
      } else {
        order_by_index = true;  // exactly k met the cut
      }
    }

    // `touched` is in first-encounter order, not index order, so the ascending
    // cases must be sorted explicitly -- but only over the <= k selected
    // elements, which is cheap.
    if (order_by_index) {
      std::sort(sel.begin(), sel.end(),
                [&cand](int p, int q) { return cand[p] < cand[q]; });
    }

    for (size_t t = 0; t < sel.size(); ++t) {
      const int p = sel[t];
      o_col.push_back(j + 1);
      o_idx.push_back(cand[p] + 1);
      o_sim.push_back(sim[p]);
    }
  }

  return List::create(_["col"] = wrap(o_col),
                      _["idx"] = wrap(o_idx),
                      _["sim"] = wrap(o_sim));
}
