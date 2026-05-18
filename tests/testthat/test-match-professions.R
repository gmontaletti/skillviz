# Tests for match_professions.R -----

# 1. fixture sintetico -----

# Crea un piccolo dataset di postings + skills_long con 3 professioni ESCO L4
# e 8 skill ESCO L3, ciascuna professione con un profilo distinto.
# - P_A: specializzata su {s1, s2, s3} + occasionale s7
# - P_B: specializzata su {s4, s5}
# - P_C: generalista su {s1, s4, s6}
# Restituisce una list con postings, skills_long e skill_labels.
.make_match_fixture <- function() {
  n_per_prof <- 40L

  build_prof <- function(prof_id, start_gid, heavy, occasional = character()) {
    gids <- seq.int(start_gid, length.out = n_per_prof)
    postings <- data.table::data.table(
      general_id = gids,
      idesco_level_4 = prof_id
    )
    # ogni annuncio porta tutte le skill "heavy"
    skills_heavy <- data.table::CJ(
      general_id = gids,
      idescoskill_level_3 = heavy
    )
    # le skill occasionali compaiono solo nella prima metà degli annunci
    if (length(occasional) > 0L) {
      skills_occ <- data.table::CJ(
        general_id = gids[seq_len(n_per_prof %/% 4L)],
        idescoskill_level_3 = occasional
      )
      skills <- data.table::rbindlist(list(skills_heavy, skills_occ))
    } else {
      skills <- skills_heavy
    }
    list(postings = postings, skills = skills)
  }

  a <- build_prof(
    "P_A",
    start_gid = 1000L,
    heavy = c("s1", "s2", "s3"),
    occasional = "s7"
  )
  b <- build_prof("P_B", start_gid = 2000L, heavy = c("s4", "s5"))
  c <- build_prof("P_C", start_gid = 3000L, heavy = c("s1", "s4", "s6"))

  postings <- data.table::rbindlist(list(a$postings, b$postings, c$postings))
  skills_long <- data.table::rbindlist(list(a$skills, b$skills, c$skills))

  skill_labels <- data.table::data.table(
    idescoskill_level_3 = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"),
    escoskill_level_3 = c(
      "Skill One",
      "Skill Two",
      "Skill Three",
      "Skill Four",
      "Skill Five",
      "Skill Six",
      "Skill Seven",
      "Skill Eight"
    )
  )

  list(
    postings = postings,
    skills_long = skills_long,
    skill_labels = skill_labels
  )
}


# 2. identità: la query coincide col profilo di P_A -----

test_that("la professione corretta è prima sotto la base coverage", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage"
  )

  expect_s3_class(res$ranking, "data.table")
  expect_equal(res$ranking[1L, idesco_level_4], "P_A")
})

test_that("la professione corretta è prima sotto la base tfidf", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "tfidf"
  )

  expect_equal(res$ranking[1L, idesco_level_4], "P_A")
})

test_that("la professione corretta è prima sotto la base rca", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "rca"
  )

  expect_equal(res$ranking[1L, idesco_level_4], "P_A")
})


# 3. invariante somma: sum(contribution) == distance per professione -----

test_that("la somma dei contributi per professione coincide con la distanza", {
  fx <- .make_match_fixture()

  for (b in c("coverage", "tfidf", "rca")) {
    res <- match_professions(
      skills = c("s1", "s2", "s3"),
      postings = fx$postings,
      skills_long = fx$skills_long,
      basis = b
    )

    sums <- res$per_skill[,
      list(sum_contrib = sum(contribution)),
      by = idesco_level_4
    ]
    merged <- merge(
      sums,
      res$ranking[, list(idesco_level_4, distance)],
      by = "idesco_level_4"
    )
    expect_equal(
      merged$sum_contrib,
      merged$distance,
      tolerance = 1e-9,
      info = paste0("basis = ", b)
    )
  }
})


# 4. non negatività dei contributi -----

test_that("tutti i contributi per_skill sono non negativi sotto ogni base", {
  fx <- .make_match_fixture()

  for (b in c("coverage", "tfidf", "rca")) {
    res <- match_professions(
      skills = c("s1", "s2", "s3"),
      postings = fx$postings,
      skills_long = fx$skills_long,
      basis = b
    )
    expect_gte(min(res$per_skill$contribution), 0)
  }
})


# 5. semantica del flag declared -----

test_that("declared distingue correttamente skill dichiarate e non dichiarate", {
  fx <- .make_match_fixture()
  query <- c("s1", "s2", "s3")

  res <- match_professions(
    skills = query,
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage"
  )

  # ogni riga con declared == TRUE ha una skill che era nella query
  declared_rows <- res$per_skill[declared == TRUE]
  expect_true(all(declared_rows$idescoskill_level_3 %in% query))

  # le skill esistenti in skills_long ma fuori dalla query non sono mai declared
  non_query_skills <- setdiff(unique(fx$skills_long$idescoskill_level_3), query)
  expect_true(
    nrow(res$per_skill[
      idescoskill_level_3 %in% non_query_skills & declared == TRUE
    ]) ==
      0L
  )

  # esistono righe declared == FALSE: le top contengono professioni che
  # richiedono skill estranee alla query (es. P_C richiede s4, s6).
  expect_gt(nrow(res$per_skill[declared == FALSE]), 0L)
})


# 6. errori su input vuoto o NA -----

test_that("input vuoto produce errore con messaggio richiesto", {
  fx <- .make_match_fixture()

  expect_error(
    match_professions(
      skills = character(),
      postings = fx$postings,
      skills_long = fx$skills_long
    ),
    regexp = "almeno una"
  )
})

test_that("input di soli NA produce errore con messaggio richiesto", {
  fx <- .make_match_fixture()

  expect_error(
    match_professions(
      skills = c(NA_character_, NA_character_),
      postings = fx$postings,
      skills_long = fx$skills_long
    ),
    regexp = "almeno una"
  )
})


# 7. warning sulle skill non risolte -----

test_that("skill non presenti nell'universo generano warning e vengono ignorate", {
  fx <- .make_match_fixture()

  expect_warning(
    res <- match_professions(
      skills = c("s1", "s2", "s_missing"),
      postings = fx$postings,
      skills_long = fx$skills_long,
      basis = "coverage"
    ),
    regexp = "universo"
  )

  expect_gt(nrow(res$ranking), 0L)
  expect_gte(res$params$n_skills_unresolved, 1L)
  expect_false("s_missing" %in% res$per_skill$idescoskill_level_3)
})


# 8. filtro min_postings restituisce tabelle vuote ma tipizzate -----

test_that("min_postings impossibile restituisce schema vuoto completo", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    min_postings = 10000L
  )

  expect_equal(nrow(res$ranking), 0L)
  expect_equal(nrow(res$per_skill), 0L)

  expect_named(
    res$ranking,
    c(
      "idesco_level_4",
      "distance",
      "score",
      "hit_ratio",
      "mean_coverage",
      "n_postings",
      "n_skills_matched"
    ),
    ignore.order = TRUE
  )
  expect_named(
    res$per_skill,
    c(
      "idesco_level_4",
      "idescoskill_level_3",
      "skill_label",
      "declared",
      "profile_value",
      "contribution",
      "contribution_pct"
    ),
    ignore.order = TRUE
  )

  expect_type(res$ranking$idesco_level_4, "character")
  expect_type(res$ranking$distance, "double")
  expect_type(res$ranking$score, "double")
  expect_type(res$ranking$hit_ratio, "double")
  expect_type(res$ranking$mean_coverage, "double")
  expect_type(res$ranking$n_postings, "integer")
  expect_type(res$ranking$n_skills_matched, "integer")

  expect_type(res$per_skill$idesco_level_4, "character")
  expect_type(res$per_skill$idescoskill_level_3, "character")
  expect_type(res$per_skill$skill_label, "character")
  expect_type(res$per_skill$declared, "logical")
  expect_type(res$per_skill$profile_value, "double")
  expect_type(res$per_skill$contribution, "double")
  expect_type(res$per_skill$contribution_pct, "double")
})


# 9. passthrough delle skill_labels -----

test_that("skill_labels valorizza la colonna skill_label quando fornito", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage",
    skill_labels = fx$skill_labels
  )

  # le skill presenti nel dizionario etichette devono avere skill_label non NA
  in_labels <- res$per_skill[
    idescoskill_level_3 %in% fx$skill_labels$idescoskill_level_3
  ]
  expect_true(all(!is.na(in_labels$skill_label)))

  # coerenza punto-per-punto con la lookup
  lookup <- setNames(
    fx$skill_labels$escoskill_level_3,
    fx$skill_labels$idescoskill_level_3
  )
  expect_equal(
    in_labels$skill_label,
    unname(lookup[in_labels$idescoskill_level_3])
  )
})

test_that("senza skill_labels la colonna skill_label è interamente NA", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage"
  )

  expect_true("skill_label" %in% names(res$per_skill))
  expect_true(all(is.na(res$per_skill$skill_label)))
  expect_type(res$per_skill$skill_label, "character")
})


# 10. top_n viene rispettato -----

test_that("top_n limita ranking e per_skill alle sole professioni richieste", {
  fx <- .make_match_fixture()

  res <- match_professions(
    skills = c("s1", "s2", "s3"),
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage",
    top_n = 1L
  )

  expect_equal(nrow(res$ranking), 1L)
  expect_equal(
    unique(res$per_skill$idesco_level_4),
    res$ranking$idesco_level_4
  )
})


# 11. stabilità top-3 fra basi diverse -----

test_that("le top-3 sotto basi diverse si sovrappongono (Jaccard >= 0.5)", {
  fx <- .make_match_fixture()
  query <- c("s1", "s2", "s3")

  jaccard <- function(a, b) {
    inter <- length(intersect(a, b))
    uni <- length(union(a, b))
    if (uni == 0L) 1 else inter / uni
  }

  res_cov <- match_professions(
    skills = query,
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage"
  )
  res_tfidf <- match_professions(
    skills = query,
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "tfidf"
  )
  res_rca <- match_professions(
    skills = query,
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "rca"
  )

  top3 <- function(r) head(r$ranking$idesco_level_4, 3L)

  expect_gte(jaccard(top3(res_cov), top3(res_tfidf)), 0.5)
  expect_gte(jaccard(top3(res_cov), top3(res_rca)), 0.5)
})


# 12. round-trip prepare + score equivalente al wrapper -----

test_that("prepare + score riproduce bit-equal il wrapper match_professions()", {
  fx <- .make_match_fixture()
  query <- c("s1", "s2", "s3")

  for (b in c("coverage", "tfidf", "rca")) {
    res_wrapper <- match_professions(
      skills = query,
      postings = fx$postings,
      skills_long = fx$skills_long,
      basis = b,
      top_n = 10L
    )

    prep <- match_professions_prepare(
      postings = fx$postings,
      skills_long = fx$skills_long,
      basis = b
    )
    expect_true(
      inherits(prep, "match_professions_prep"),
      info = paste0("basis = ", b)
    )

    res_split <- match_professions_score(
      prep,
      skills = query,
      top_n = 10L
    )

    expect_equal(
      res_wrapper$ranking,
      res_split$ranking,
      tolerance = 1e-12,
      info = paste0("basis = ", b)
    )
    expect_equal(
      res_wrapper$per_skill,
      res_split$per_skill,
      tolerance = 1e-12,
      info = paste0("basis = ", b)
    )
    expect_identical(res_wrapper$basis, res_split$basis)
    expect_identical(res_wrapper$query, res_split$query)
  }
})


# 13. pesi della query: re-ranking e invarianti -----

test_that("weights ri-pesano la query e preservano l'invariante di somma", {
  fx <- .make_match_fixture()
  query <- c("s1", "s4", "s7")

  prep <- match_professions_prepare(
    postings = fx$postings,
    skills_long = fx$skills_long,
    basis = "coverage"
  )

  # riferimento uniforme
  res_u <- match_professions_score(prep, skills = query)

  # variante pesata: spinge massa sulla skill di nicchia s4
  res_w <- match_professions_score(
    prep,
    skills = query,
    weights = c(0.3, 3.0, 0.3)
  )

  # il ranking complessivo deve cambiare in qualche modo (ordine o distanze)
  expect_false(identical(res_u$ranking, res_w$ranking))

  # invariante somma per ciascuna professione, anche sotto pesi
  for (p in res_w$ranking$idesco_level_4) {
    dist_p <- res_w$ranking[idesco_level_4 == p, distance]
    sum_c <- sum(res_w$per_skill[idesco_level_4 == p, contribution])
    expect_equal(sum_c, dist_p, tolerance = 1e-9)
  }

  # hit_ratio e mean_coverage devono restare in [0, 1]
  expect_true(all(res_w$ranking$hit_ratio >= 0 & res_w$ranking$hit_ratio <= 1))
  expect_true(
    all(res_w$ranking$mean_coverage >= 0 & res_w$ranking$mean_coverage <= 1)
  )

  # pesi uniformi equivalgono a NULL
  res_uw <- match_professions_score(
    prep,
    skills = query,
    weights = c(1, 1, 1)
  )
  expect_equal(res_u$ranking, res_uw$ranking, tolerance = 1e-12)

  # validazione pesi: negativi
  expect_error(
    match_professions_score(
      prep,
      skills = c("s1", "s4"),
      weights = c(-1, 1)
    ),
    regexp = "non negativo"
  )

  # validazione pesi: tutti zero
  expect_error(
    match_professions_score(
      prep,
      skills = c("s1", "s4"),
      weights = c(0, 0)
    ),
    regexp = "identicamente zero"
  )

  # validazione pesi: lunghezza errata
  expect_error(
    match_professions_score(
      prep,
      skills = c("s1", "s4", "s7"),
      weights = c(1, 1)
    )
  )

  # validazione prep: classe errata
  expect_error(
    match_professions_score(prep = list(), skills = c("s1")),
    regexp = "match_professions_prep"
  )
})
