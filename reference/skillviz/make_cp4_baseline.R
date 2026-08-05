# make_cp4_baseline.R — freeze predict_cp4_knn()'s output before refactoring ---
#
# CLAUDE.md requires that any change to a function consumed by downstream
# pipelines be backed by an output comparison against a known baseline. The CP5
# work touches R/crosswalk.R, so this script captures what predict_cp4_knn()
# returns TODAY, over a wide argument surface, and freezes it as a fixture.
# test-crosswalk.R then asserts expect_identical() against it.
#
# Run this with the UNMODIFIED package loaded, before any edit:
#   Rscript reference/skillviz/make_cp4_baseline.R
#
# expect_identical(), not expect_equal(): `confidence` is a ratio of doubles and
# the point of the fixture is bit-identity, not approximate agreement.

library(data.table)
devtools::load_all(".", quiet = TRUE)

OUT <- "tests/testthat/fixtures/cp4_baseline.rds"
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)

# 1. Fixtures -----

# 1a. The rescue fixture from test-crosswalk.R:330, verbatim. Small, but it is
# the one input already known to exercise every branch of the cascade.
.rescue_fixture <- function() {
  postings <- data.table::data.table(
    general_id = as.character(1:12),
    idesco_level_4 = c(rep(c(1000L, 2000L), each = 4), rep(NA_integer_, 4)),
    cp2021_id_level_4 = c(
      "1.1.1.1",
      "1.1.1.2",
      NA,
      NA,
      "2.2.2.1",
      "2.2.2.2",
      NA,
      NA,
      "1.1.1.1",
      NA,
      NA,
      NA
    ),
    idsector = c(rep("C", 4), rep("F", 4), "C", "C", "F", "F")
  )
  skills <- data.table::data.table(
    general_id = as.character(c(
      1,
      1,
      2,
      2,
      3,
      3,
      4,
      4,
      5,
      5,
      6,
      6,
      7,
      7,
      8,
      8,
      9,
      9,
      10,
      10,
      11,
      11
    )),
    escoskill_level_3 = c(
      "s1",
      "s2",
      "s2",
      "s3",
      "s1",
      "s2",
      "s2",
      "s3",
      "s4",
      "s5",
      "s5",
      "s6",
      "s4",
      "s5",
      "s5",
      "s6",
      "s1",
      "s2",
      "s1",
      "s2",
      "s4",
      "s5"
    )
  )
  list(postings = postings, skills = skills)
}

# 1b. A larger seeded fixture. The hand-written ones are too small to move the
# tie-breaks that the refactor must preserve: grouping order in the vote, the
# which.max() first-maximum rule, and the deterministic max_train stride. This
# one has enough rows per ESCO group to exercise all three.
.big_fixture <- function(seed = 7L) {
  set.seed(seed)
  n <- 900L
  n_esco <- 24L
  esco <- sample(seq_len(n_esco) * 100L, n, replace = TRUE)
  # 1-6 CP4 codes per ESCO group, so single_candidate and multi-candidate
  # groups both occur.
  n_cand <- setNames(
    sample(1:6, n_esco, replace = TRUE),
    seq_len(n_esco) * 100L
  )
  cp4 <- vapply(
    seq_len(n),
    function(i) {
      g <- esco[i]
      sprintf(
        "%d.%d.%d.%d",
        g %/% 1000L + 1L,
        1L,
        1L,
        sample(seq_len(n_cand[[as.character(g)]]), 1L)
      )
    },
    character(1)
  )
  # 30% unlabeled, plus 8% with no ESCO code at all (the rescue population),
  # plus a handful of labeled rows carrying the empty string rather than NA --
  # itaposts emits both and the split must treat them alike.
  lab <- runif(n)
  cp4[lab < 0.30] <- NA_character_
  cp4[lab >= 0.30 & lab < 0.33] <- ""
  esco[runif(n) < 0.08] <- NA_integer_

  postings <- data.table::data.table(
    general_id = as.character(seq_len(n)),
    idesco_level_4 = esco,
    cp2021_id_level_4 = cp4,
    # An unknown idsector arrives as the empty string, never NA
    # (R/crosswalk.R:980-985), so the fixture must contain both.
    idsector = sample(
      c("A", "B", "C", "", NA_character_),
      n,
      replace = TRUE,
      prob = c(0.3, 0.3, 0.25, 0.1, 0.05)
    )
  )

  # 1-9 skills per posting from a 40-term vocabulary; a few postings carry none.
  sk <- rbindlist(lapply(seq_len(n), function(i) {
    m <- sample(0:9, 1L)
    if (m == 0L) {
      return(NULL)
    }
    data.table::data.table(
      general_id = as.character(i),
      escoskill_level_3 = sample(sprintf("s%02d", 1:40), m)
    )
  }))
  list(postings = postings, skills = unique(sk))
}

fixtures <- list(rescue = .rescue_fixture(), big = .big_fixture())

# 2. Argument surface -----
# Every combination the existing suite exercises, plus max_train values that
# straddle the group sizes in the big fixture so the stride actually fires.
grid <- data.table::CJ(
  fixture = names(fixtures),
  k = c(3L, 7L),
  sector_boost = c(1.0, 5.0),
  rescue_no_match = c(FALSE, TRUE),
  max_train = c(5L, 50000L),
  sorted = FALSE
)

# 3. Capture -----
baseline <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i]
  f <- fixtures[[g$fixture]]
  # Warnings are part of the contract (max_train warns); capture them too.
  w <- character(0)
  res <- withCallingHandlers(
    predict_cp4_knn(
      f$postings,
      f$skills,
      k = g$k,
      sector_boost = g$sector_boost,
      max_train = g$max_train,
      rescue_no_match = g$rescue_no_match,
      verbose = FALSE
    ),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  data.table::setorder(res, general_id)
  baseline[[i]] <- list(args = as.list(g), result = res, warnings = w)
}

# The fixtures travel with the baseline so the test can replay the exact same
# inputs without re-deriving them -- a fixture that drifted from the one used
# here would turn a real regression into a passing test.
saveRDS(
  list(
    package_version = as.character(utils::packageVersion("skillviz")),
    created = Sys.time(),
    fixtures = fixtures,
    grid = grid,
    baseline = baseline
  ),
  OUT
)

cat(sprintf("captured %d cases -> %s\n", length(baseline), OUT))
cat(sprintf(
  "  methods seen: %s\n",
  paste(
    sort(unique(unlist(lapply(baseline, function(b) b$result$method)))),
    collapse = ", "
  )
))
cat(sprintf(
  "  rows per case: min %d, max %d\n",
  min(vapply(baseline, function(b) nrow(b$result), integer(1))),
  max(vapply(baseline, function(b) nrow(b$result), integer(1)))
))
