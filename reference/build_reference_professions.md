# Reference professions of one classification in another

For every code of an official universe of the starting classification
(`from_col`), names a reference profession of the arrival classification
(`to_col`): the most frequent arrival code among the postings that carry
both codes when there are at least `n_min` of them, otherwise the
arrival code with the most similar skill profile, otherwise none. One
direction per call: swap `from_col` and `to_col` for the other.

## Usage

``` r
build_reference_professions(
  postings,
  skills = NULL,
  from_col,
  to_col,
  universe_from,
  universe_to,
  n_min,
  min_postings = 30L,
  strata_col = NULL,
  id_col = "general_id",
  skill_col = "idescoskill_level_3",
  from_na = c("", "Unclassifiable"),
  to_na = c("", "Unclassifiable"),
  weighting = c("idf", "none"),
  profiles_from = NULL,
  profiles_to = NULL
)
```

## Arguments

- postings:

  A `data.frame` with one row per posting and the columns `from_col`,
  `to_col`, `strata_col` (when used) and, when the profiles are not
  supplied, `id_col`. Only codes observed on the postings are used: the
  function never imputes a code.

- skills:

  A `data.frame` of posting skills (columns `id_col` and `skill_col`),
  used only when `profiles_from` or `profiles_to` is `NULL`.

- from_col, to_col:

  Names of the starting and arrival code columns.

- universe_from, universe_to:

  `data.frame`s of the official codes of the two classifications, with
  column `code` and optional `label` and `parent`. The output has one
  row per code of `universe_from`; similarity references are chosen
  among the codes of `universe_to`.

- n_min:

  Minimum number of postings with both codes for the frequency rule.
  Calibrate it with
  [`calibrate_reference_threshold()`](https://gmontaletti.github.io/skillviz/reference/calibrate_reference_threshold.md).

- min_postings:

  Minimum number of postings with at least one skill for a profile to be
  usable. Default 30.

- strata_col:

  Optional stratum column (typically `"source"`), passed to
  [`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md)
  to flag modes that depend on which postings were coded
  (`modal_stable`).

- id_col, skill_col:

  Posting id and skill column names.

- from_na, to_na:

  Values of the two code columns that mean "no code". Default
  `c("", "Unclassifiable")`.

- weighting:

  Profile weighting, passed to
  [`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md).

- profiles_from, profiles_to:

  Optional precomputed profile matrices from
  [`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md)
  for the two classifications, on the same skill weighting. When `NULL`
  they are computed from `postings` and `skills` with an IDF shared by
  both, computed on all postings of `postings`.

## Value

A `data.table`, one row per code of `universe_from`, sorted by code,
with columns:

- code, label, parent:

  The starting code, its label and parent.

- method:

  `"frequenza"`, `"similarità"` or `"nessuno"`.

- reason:

  Why the method was chosen: `"annunci_in_comune"` (frequency),
  `"annunci_in_comune_sotto_soglia"` or `"nessun_annuncio_in_comune"`
  (similarity), `"nessun_annuncio"`, `"profilo_insufficiente"` or
  `"nessun_candidato"` (none).

- ref_code, ref_label, ref_parent:

  The reference profession.

- ref_in_universe:

  Whether `ref_code` belongs to `universe_to` (a frequency reference may
  be a code observed on postings but absent from the universe).

- n_postings:

  Postings with the starting code.

- n_cocoded:

  Postings with the starting code and an arrival code.

- code_modal, modal_share, n_eff, modal_tied, modal_reliable,
  modal_stable:

  From
  [`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md):
  the most frequent arrival code, its share, the inverse-Simpson number
  of candidates, whether the mode is tied (resolved on the code), and
  the reliability and stability flags.

- n_profile:

  Postings with at least one skill behind the starting code's profile.

- cosine_ref:

  Cosine between the profiles of the code and of its reference, when
  both exist.

- sim_code, sim_cosine, sim2_code, sim2_cosine, sim_tied:

  Best and second-best arrival code by cosine, for every code with a
  usable profile, whatever the method.

Attributes `n_min` and `min_postings` record the thresholds.

## Details

**Frequency.** The mode is computed by
[`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md)
with `from_col` as restrictor and `to_col` as target, after the missing
markers of both columns are set to `NA`: ties are resolved on the code
and flagged in `modal_tied`. The frequency reference is the most typical
arrival profession of the code, not an equivalence: about one posting in
four with an ESCO level-5 code carries a CP5 code other than the mode.

**Similarity.** Profiles are coverage shares weighted by IDF
([`skill_profiles()`](https://gmontaletti.github.io/skillviz/reference/skill_profiles.md));
the reference is the arrival code of the universe with a usable profile
and the largest cosine. When the two profiles are built on overlapping
postings, the cosine between a code and the arrival code that codes the
same postings is inflated.

## Examples

``` r
postings <- data.table::data.table(
  general_id = 1:8,
  cp = c(rep("1.1.1.1.0", 4), rep("2.2.2.2.0", 3), ""),
  esco = c("1000.1", "1000.1", "1000.1", "1000.2",
           "2000.1", "2000.1", "Unclassifiable", "2000.2")
)
skills <- data.table::data.table(
  general_id = rep(1:8, each = 2),
  idescoskill_level_3 = c("a", "b", "a", "b", "a", "c", "a", "b",
                          "d", "e", "d", "e", "d", "f", "d", "e")
)
ucp <- data.table::data.table(
  code = c("1.1.1.1.0", "2.2.2.2.0", "3.3.3.3.0"),
  parent = c("1.1.1.1", "2.2.2.2", "3.3.3.3")
)
uesco <- data.table::data.table(
  code = c("1000.1", "1000.2", "2000.1", "2000.2"),
  parent = c("1000", "1000", "2000", "2000")
)
build_reference_professions(
  postings, skills, "cp", "esco", ucp, uesco,
  n_min = 3, min_postings = 1
)
#>         code  label  parent     method                         reason ref_code
#>       <char> <char>  <char>     <char>                         <char>   <char>
#> 1: 1.1.1.1.0   <NA> 1.1.1.1  frequenza              annunci_in_comune   1000.1
#> 2: 2.2.2.2.0   <NA> 2.2.2.2 similarità annunci_in_comune_sotto_soglia   2000.1
#> 3: 3.3.3.3.0   <NA> 3.3.3.3    nessuno                nessun_annuncio     <NA>
#>    ref_label ref_parent ref_in_universe n_postings n_cocoded code_modal
#>       <char>     <char>          <lgcl>      <int>     <int>     <char>
#> 1:      <NA>       1000            TRUE          4         4     1000.1
#> 2:      <NA>       2000            TRUE          3         2     2000.1
#> 3:      <NA>       <NA>           FALSE          0         0       <NA>
#>    modal_share n_eff modal_tied modal_reliable modal_stable n_profile
#>          <num> <num>     <lgcl>         <lgcl>       <lgcl>     <int>
#> 1:        0.75   1.6      FALSE          FALSE           NA         4
#> 2:        1.00   1.0      FALSE          FALSE           NA         3
#> 3:          NA    NA      FALSE          FALSE           NA         0
#>    cosine_ref sim_code sim_cosine sim2_code sim2_cosine sim_tied
#>         <num>   <char>      <num>    <char>       <num>   <lgcl>
#> 1:  0.9869458   1000.1  0.9869458    1000.2   0.8805097    FALSE
#> 2:  0.7926717   2000.1  0.7926717    2000.2   0.7926717     TRUE
#> 3:         NA     <NA>         NA      <NA>          NA       NA
```
