# Profession matching from a declared skill set -----

# 1. match_professions -----

#' Classifica le professioni ESCO L4 più affini a un insieme di competenze
#'
#' A partire da un vettore di identificativi ESCO L3 dichiarati dall'utente,
#' costruisce il profilo empirico di ogni professione ESCO L4 (a partire
#' dalle offerte di lavoro) e ne calcola la distanza dal profilo desiderato.
#' Le professioni vengono ordinate per distanza crescente e per ogni
#' professione viene restituita la decomposizione del contributo di ciascuna
#' competenza alla distanza totale.
#'
#' La distanza usata è la distanza coseno (`1 - cos(v_p, q)`), equivalente
#' a metà della distanza euclidea quadrata fra vettori L2-normalizzati.
#' Questa scelta è l'unica metrica della famiglia che fornisce una
#' decomposizione additiva pulita per singola competenza:
#' \deqn{1 - \cos(v_p, q) = \frac{1}{2} \sum_s ( \tilde v_{p,s} - \tilde q_s )^2.}
#'
#' Il profilo `v_p` di ogni professione è costruito secondo la base scelta:
#' \describe{
#'   \item{`coverage`}{`n_{p,s} / N_p`, probabilità che un annuncio di `p`
#'     richieda `s`. Interpretabile come quota di copertura.}
#'   \item{`tfidf`}{`tf_{p,s} * idf_s` con `tf_{p,s} = n_{p,s} / sum_s n_{p,s}`
#'     calcolata internamente alla professione, e `idf_s = log(P / df_s)`
#'     dove `df_s` è il numero di professioni che richiedono `s` (dopo
#'     filtro `min_postings`) e `P` il numero totale di professioni
#'     superstiti. Pesa di più le competenze discriminanti.}
#'   \item{`rca`}{indice di Balassa, delegato a [compute_balassa_index()].
#'     Misura la specializzazione: `>1` indica sovra-rappresentazione.}
#' }
#'
#' Il vettore di query `q` è l'indicatore non pesato delle competenze
#' dichiarate (1 per quelle presenti nell'universo, 0 altrove). Sia `q`
#' che le colonne di `M` vengono L2-normalizzati prima del calcolo.
#'
#' @param skills Vettore character di identificativi ESCO L3
#'   (`idescoskill_level_3`). Eventuali `NA` e duplicati vengono rimossi.
#' @param postings `data.table` con almeno le colonne `general_id` e
#'   `idesco_level_4`. Tipicamente ottenuto da
#'   `itaposts::oja_postings(con) |> dplyr::collect()`.
#' @param skills_long `data.table` in formato lungo con almeno le colonne
#'   `general_id` e `idescoskill_level_3`. Una riga per coppia
#'   (annuncio, competenza). Tipicamente ottenuto da
#'   `itaposts::oja_skills(con) |> dplyr::collect()`.
#' @param basis Base del profilo professionale: `"coverage"` (default),
#'   `"tfidf"` oppure `"rca"`.
#' @param top_n Numero intero di professioni da restituire in `ranking`
#'   e per le quali emettere la decomposizione `per_skill`. Default `20L`.
#' @param min_postings Numero minimo di annunci per professione necessari
#'   per ritenere stabile il profilo. Le professioni sotto soglia vengono
#'   scartate prima del calcolo. Default `30L`.
#' @param skill_labels `data.table` opzionale con almeno le colonne
#'   `idescoskill_level_3` e `escoskill_level_3` (etichetta leggibile).
#'   Quando fornito, la tabella `per_skill` ottiene la colonna
#'   `skill_label`. Default `NULL`. Tipicamente ottenuto da
#'   `itaposts::oja_skill_dim(con) |> dplyr::collect()`.
#'
#' @return Una `list` con i seguenti elementi:
#'   \describe{
#'     \item{`ranking`}{`data.table` con una riga per professione (al più
#'       `top_n`), ordinata per `distance` crescente. Colonne:
#'       `idesco_level_4`, `distance` (in `[0, 1]`), `score = 1 - distance`,
#'       `hit_ratio` (frazione delle competenze dichiarate che la
#'       professione richiede almeno una volta), `mean_coverage` (media di
#'       `coverage_{p,s}` sulle competenze dichiarate; `NA` se
#'       `basis != "coverage"`), `n_postings`, `n_skills_matched`.}
#'     \item{`per_skill`}{`data.table` in formato lungo con una riga per
#'       ciascuna coppia (professione top-N, competenza con contributo
#'       non nullo). La lista delle righe include sia le competenze
#'       dichiarate sia quelle richieste dalla professione ma non
#'       dichiarate, in modo che valga l'identità additiva
#'       `sum(contribution) == distance` per ogni professione. Colonne:
#'       `idesco_level_4`, `idescoskill_level_3`, `skill_label`
#'       (`NA` se `skill_labels` non fornito), `declared` (logico, `TRUE`
#'       se la competenza è nella query), `profile_value` (valore grezzo
#'       di `v_p[s]` nella base scelta, pre-normalizzazione; `NA` se la
#'       competenza non è nell'universo della professione),
#'       `contribution` (addendo della distanza), `contribution_pct`
#'       (`contribution / distance`; `NA` se `distance == 0`).}
#'     \item{`basis`}{La base usata, per introspezione.}
#'     \item{`query`}{Il vettore `skills` deduplicato.}
#'     \item{`params`}{Lista con `top_n`, `min_postings`,
#'       `n_professions_considered`, `n_skills_in_universe`,
#'       `n_skills_resolved`, `n_skills_unresolved`.}
#'   }
#'
#'   Nota sull'invariante: la somma di `contribution` su tutte le righe di
#'   `per_skill` per una data professione coincide con `distance` perché
#'   sono incluse anche le competenze richieste dalla professione ma non
#'   dichiarate. Filtrando su `declared == TRUE` la somma sarà invece
#'   inferiore a `distance`, e il divario rappresenta la massa che la
#'   professione richiede al di fuori della query.
#'
#' @seealso [compute_profession_distance()] per la distanza fra
#'   professioni, [compute_balassa_index()] per l'indice di Balassa usato
#'   dalla base `rca`, [build_skill_prof_sparse()] per la matrice
#'   competenze x professioni.
#'
#' @examples
#' \dontrun{
#' con  <- itaposts::oja_connect()
#' on.exit(itaposts::oja_disconnect(con), add = TRUE)
#'
#' post <- itaposts::oja_postings(con)  |> dplyr::collect() |> data.table::setDT()
#' skil <- itaposts::oja_skills(con)    |> dplyr::collect() |> data.table::setDT()
#' labs <- itaposts::oja_skill_dim(con) |> dplyr::collect() |> data.table::setDT()
#'
#' res <- skillviz::match_professions(
#'   skills       = c("S1.4.1", "S4.8.1", "S1.13.2"),
#'   postings     = post,
#'   skills_long  = skil,
#'   basis        = "coverage",
#'   top_n        = 10,
#'   skill_labels = labs
#' )
#'
#' res$ranking
#' res$per_skill[idesco_level_4 == res$ranking$idesco_level_4[1L]]
#' }
#'
#' @export
match_professions <- function(
  skills,
  postings,
  skills_long,
  basis = c("coverage", "tfidf", "rca"),
  top_n = 20L,
  min_postings = 30L,
  skill_labels = NULL
) {
  # 1. validazione input -----
  basis <- match.arg(basis)

  if (!is.character(skills)) {
    stop(
      "match_professions: 'skills' deve essere un vettore character di ID ESCO L3.",
      call. = FALSE
    )
  }

  skills <- unique(skills[!is.na(skills)])
  if (length(skills) == 0L) {
    stop(
      "match_professions: \u00e8 necessario fornire almeno una skill ESCO L3.",
      call. = FALSE
    )
  }

  if (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1L) {
    stop(
      "match_professions: 'top_n' deve essere un intero positivo.",
      call. = FALSE
    )
  }
  top_n <- as.integer(top_n)

  if (
    !is.numeric(min_postings) || length(min_postings) != 1L || min_postings < 0L
  ) {
    stop(
      "match_professions: 'min_postings' deve essere un intero non negativo.",
      call. = FALSE
    )
  }
  min_postings <- as.integer(min_postings)

  check_columns(
    postings,
    c("general_id", "idesco_level_4"),
    caller = "match_professions"
  )
  check_columns(
    skills_long,
    c("general_id", "idescoskill_level_3"),
    caller = "match_professions"
  )

  if (!data.table::is.data.table(postings)) {
    postings <- data.table::as.data.table(postings)
  }
  if (!data.table::is.data.table(skills_long)) {
    skills_long <- data.table::as.data.table(skills_long)
  }

  if (!is.null(skill_labels)) {
    check_columns(
      skill_labels,
      c("idescoskill_level_3", "escoskill_level_3"),
      caller = "match_professions"
    )
    if (!data.table::is.data.table(skill_labels)) {
      skill_labels <- data.table::as.data.table(skill_labels)
    }
  }

  # Le viste lazy DuckDB raccolte via collect() arrivano con i nomi originali;
  # lavoriamo solo sulle colonne necessarie per non duplicare grandi tabelle.
  post_slim <- unique(
    postings[
      !is.na(idesco_level_4) & !is.na(general_id),
      .(general_id, idesco_level_4)
    ]
  )
  skills_slim <- skills_long[
    !is.na(idescoskill_level_3) & !is.na(general_id),
    .(general_id, idescoskill_level_3)
  ]

  # 2. filtro professioni sotto soglia -----
  n_per_prof <- post_slim[, .(N_p = .N), by = idesco_level_4]
  keep_profs <- n_per_prof[N_p >= min_postings, idesco_level_4]
  n_professions_considered <- length(keep_profs)

  if (n_professions_considered == 0L) {
    return(
      .empty_match_result(
        skills = skills,
        basis = basis,
        top_n = top_n,
        min_postings = min_postings,
        n_professions_considered = 0L,
        n_skills_in_universe = 0L,
        n_skills_resolved = 0L,
        n_skills_unresolved = length(skills),
        has_labels = !is.null(skill_labels)
      )
    )
  }

  post_slim <- post_slim[idesco_level_4 %chin% keep_profs]
  n_per_prof <- n_per_prof[idesco_level_4 %chin% keep_profs]

  # 3. coppie (professione, competenza) per annuncio -----
  ps <- merge(
    post_slim,
    skills_slim,
    by = "general_id",
    allow.cartesian = TRUE
  )

  if (nrow(ps) == 0L) {
    return(
      .empty_match_result(
        skills = skills,
        basis = basis,
        top_n = top_n,
        min_postings = min_postings,
        n_professions_considered = n_professions_considered,
        n_skills_in_universe = 0L,
        n_skills_resolved = 0L,
        n_skills_unresolved = length(skills),
        has_labels = !is.null(skill_labels)
      )
    )
  }

  # 4. conteggi (p, s) e (p) su annunci distinti -----
  # n_ps usa uniqueN(general_id) per non gonfiare se l'input avesse duplicati;
  # in pratica skills_long ha già una riga per (general_id, skill).
  counts <- ps[,
    .(n_ps = data.table::uniqueN(general_id)),
    by = .(idesco_level_4, idescoskill_level_3)
  ]
  counts <- merge(counts, n_per_prof, by = "idesco_level_4")

  # 5. costruzione del profilo nella base scelta -----
  if (basis == "coverage") {
    counts[, value := n_ps / N_p]
  } else if (basis == "tfidf") {
    counts[, sum_n_p := sum(n_ps), by = idesco_level_4]
    counts[, tf := n_ps / sum_n_p]
    df_per_skill <- counts[,
      .(df_s = data.table::uniqueN(idesco_level_4)),
      by = idescoskill_level_3
    ]
    P_total <- data.table::uniqueN(counts$idesco_level_4)
    df_per_skill[, idf := log(P_total / df_s)]
    counts <- merge(
      counts,
      df_per_skill[, .(idescoskill_level_3, idf)],
      by = "idescoskill_level_3"
    )
    counts[, value := tf * idf]
  } else {
    # basis == "rca": deleghiamo a compute_balassa_index() che richiede i nomi
    # "preferredLabel" + "escoskill_level_3". Rinominiamo solo localmente.
    bal_in <- ps[, .(
      preferredLabel = idesco_level_4,
      escoskill_level_3 = idescoskill_level_3
    )]
    bal <- compute_balassa_index(bal_in)
    counts <- bal[, .(
      idesco_level_4 = preferredLabel,
      idescoskill_level_3 = escoskill_level_3,
      value = balassa_index
    )]
    counts <- merge(counts, n_per_prof, by = "idesco_level_4")
  }

  # coverage_ps serve sempre per mean_coverage e hit_ratio. Per "coverage"
  # coincide con value; per "tfidf" è derivabile dai conteggi già in counts;
  # per "rca" passiamo per compute_balassa_index() che scarta n_ps, quindi
  # ricostruiamo il conteggio dalle coppie originali.
  if (basis == "coverage") {
    counts[, coverage_ps := value]
  } else if (basis == "tfidf") {
    counts[, coverage_ps := n_ps / N_p]
  } else {
    cov_aux <- ps[,
      .(n_ps_aux = data.table::uniqueN(general_id)),
      by = .(idesco_level_4, idescoskill_level_3)
    ]
    counts <- merge(
      counts,
      cov_aux,
      by = c("idesco_level_4", "idescoskill_level_3"),
      all.x = TRUE
    )
    counts[, coverage_ps := n_ps_aux / N_p]
    counts[, n_ps_aux := NULL]
  }

  # 6. matrice sparsa M (skill x professione) -----
  # build_skill_prof_sparse() pretende il nome "escoskill_level_3"; rinominiamo
  # solo per la chiamata, senza toccare il resto della pipeline.
  prof_input <- counts[, .(
    escoskill_level_3 = idescoskill_level_3,
    idesco_level_4,
    value = value
  )]
  prof_input <- prof_input[!is.na(value) & value != 0]

  if (nrow(prof_input) == 0L) {
    return(
      .empty_match_result(
        skills = skills,
        basis = basis,
        top_n = top_n,
        min_postings = min_postings,
        n_professions_considered = n_professions_considered,
        n_skills_in_universe = 0L,
        n_skills_resolved = 0L,
        n_skills_unresolved = length(skills),
        has_labels = !is.null(skill_labels)
      )
    )
  }

  M <- build_skill_prof_sparse(
    prof_input,
    value_col = "value",
    prof_col = "idesco_level_4"
  )

  n_skills_in_universe <- nrow(M)
  universe_skills <- rownames(M)
  universe_profs <- colnames(M)

  # 7. risoluzione delle skill della query e vettore q L2-normalizzato -----
  resolved <- skills[skills %in% universe_skills]
  unresolved <- setdiff(skills, resolved)
  n_skills_resolved <- length(resolved)
  n_skills_unresolved <- length(unresolved)

  if (n_skills_unresolved > 0L) {
    warning(
      "match_professions: ",
      n_skills_unresolved,
      " competenz",
      if (n_skills_unresolved == 1L) "a" else "e",
      " non present",
      if (n_skills_unresolved == 1L) "e" else "i",
      " nell'universo OJA e ignorat",
      if (n_skills_unresolved == 1L) "a" else "e",
      ": ",
      paste(unresolved, collapse = ", "),
      call. = FALSE
    )
  }

  if (n_skills_resolved == 0L) {
    return(
      .empty_match_result(
        skills = skills,
        basis = basis,
        top_n = top_n,
        min_postings = min_postings,
        n_professions_considered = n_professions_considered,
        n_skills_in_universe = n_skills_in_universe,
        n_skills_resolved = 0L,
        n_skills_unresolved = n_skills_unresolved,
        has_labels = !is.null(skill_labels)
      )
    )
  }

  q <- numeric(n_skills_in_universe)
  names(q) <- universe_skills
  q[resolved] <- 1
  q_norm_val <- sqrt(sum(q * q))
  q <- q / q_norm_val # uniforme 1/sqrt(|resolved|) sulle skill dichiarate

  # 8. L2-normalizzazione colonne di M, scarto colonne nulle -----
  col_norms <- sqrt(Matrix::colSums(M * M))
  keep_cols <- col_norms > 0
  if (!all(keep_cols)) {
    M <- M[, keep_cols, drop = FALSE]
    col_norms <- col_norms[keep_cols]
    universe_profs <- colnames(M)
  }

  if (ncol(M) == 0L) {
    return(
      .empty_match_result(
        skills = skills,
        basis = basis,
        top_n = top_n,
        min_postings = min_postings,
        n_professions_considered = n_professions_considered,
        n_skills_in_universe = n_skills_in_universe,
        n_skills_resolved = n_skills_resolved,
        n_skills_unresolved = n_skills_unresolved,
        has_labels = !is.null(skill_labels)
      )
    )
  }

  # Divisione colonna per colonna via Diagonal: resta sparsa.
  M_norm <- M %*% Matrix::Diagonal(x = 1 / col_norms)
  dimnames(M_norm) <- list(universe_skills, universe_profs)

  # 9. distanza coseno e contributo per skill -----
  # 1 - cos = 0.5 * sum_s (M_norm[s, p] - q[s])^2
  # Il sum sulle skill in `resolved` viene gestito esplicitamente; sulle
  # altre skill q vale 0, quindi il contributo è 0.5 * M_norm[s, p]^2.
  M_sq_colsum <- Matrix::colSums(M_norm * M_norm) # = 1 per costruzione
  q_resolved <- q[resolved]
  # cross-prod fra M_norm e q (zero ovunque tranne su `resolved`)
  cross_qM <- as.numeric(
    Matrix::crossprod(M_norm[resolved, , drop = FALSE], q_resolved)
  )
  distance_vec <- 0.5 * (M_sq_colsum + 1 - 2 * cross_qM)
  # protezione numerica: piccoli negativi da round-off
  distance_vec[distance_vec < 0 & distance_vec > -1e-12] <- 0

  ranking_all <- data.table::data.table(
    idesco_level_4 = universe_profs,
    distance = distance_vec
  )
  data.table::setorder(ranking_all, distance)

  top_profs <- ranking_all[
    seq_len(min(top_n, nrow(ranking_all))),
    idesco_level_4
  ]

  # 10. metriche secondarie su top_profs -----
  # hit_ratio e mean_coverage richiedono coverage_ps sulle skill dichiarate
  cov_top <- counts[
    idesco_level_4 %chin% top_profs & idescoskill_level_3 %chin% resolved,
    .(idesco_level_4, idescoskill_level_3, coverage_ps)
  ]

  hit_dt <- cov_top[,
    .(n_skills_matched = .N, sum_cov = sum(coverage_ps, na.rm = TRUE)),
    by = idesco_level_4
  ]

  ranking <- data.table::data.table(idesco_level_4 = top_profs)
  ranking <- merge(ranking, ranking_all, by = "idesco_level_4", sort = FALSE)
  ranking <- merge(
    ranking,
    n_per_prof,
    by = "idesco_level_4",
    all.x = TRUE,
    sort = FALSE
  )
  ranking <- merge(
    ranking,
    hit_dt,
    by = "idesco_level_4",
    all.x = TRUE,
    sort = FALSE
  )
  ranking[is.na(n_skills_matched), n_skills_matched := 0L]
  ranking[is.na(sum_cov), sum_cov := 0]
  ranking[, hit_ratio := n_skills_matched / n_skills_resolved]
  if (basis == "coverage") {
    ranking[, mean_coverage := sum_cov / n_skills_resolved]
  } else {
    ranking[, mean_coverage := NA_real_]
  }
  ranking[, score := 1 - distance]
  ranking <- ranking[,
    .(
      idesco_level_4,
      distance,
      score,
      hit_ratio,
      mean_coverage,
      n_postings = N_p,
      n_skills_matched
    )
  ]
  data.table::setorder(ranking, distance)

  # 11. decomposizione per_skill: tutte le skill con contributo non nullo -----
  # Per ogni professione top_n estraiamo la colonna L2-normalizzata; il
  # contributo è 0.5 * (M_norm[s, p] - q[s])^2. Restano fuori solo le skill
  # con M_norm[s, p] == 0 AND q[s] == 0, che hanno contributo nullo.
  M_norm_top <- M_norm[, top_profs, drop = FALSE]
  M_raw_top <- M[, top_profs, drop = FALSE]

  per_skill_list <- lapply(seq_len(ncol(M_norm_top)), function(j) {
    prof_j <- colnames(M_norm_top)[j]
    col_norm <- M_norm_top[, j]
    col_raw <- M_raw_top[, j]
    # union dei supporti: skill con M_norm[s, p] != 0 oppure s in resolved
    nz_idx <- which(col_norm != 0)
    decl_idx <- match(resolved, universe_skills)
    decl_idx <- decl_idx[!is.na(decl_idx)]
    idx <- sort(unique(c(nz_idx, decl_idx)))
    if (length(idx) == 0L) {
      return(NULL)
    }
    s_ids <- universe_skills[idx]
    qv <- numeric(length(idx))
    qv[s_ids %in% resolved] <- q[resolved][1] # tutti i resolved hanno lo stesso peso
    diff <- as.numeric(col_norm[idx]) - qv
    contrib <- 0.5 * diff * diff
    data.table::data.table(
      idesco_level_4 = prof_j,
      idescoskill_level_3 = s_ids,
      declared = s_ids %in% resolved,
      profile_value = as.numeric(col_raw[idx]),
      contribution = contrib
    )
  })
  per_skill <- data.table::rbindlist(
    per_skill_list,
    use.names = TRUE,
    fill = TRUE
  )

  # distance per professione per il pct
  dist_map <- ranking[, .(idesco_level_4, distance)]
  per_skill <- merge(per_skill, dist_map, by = "idesco_level_4", sort = FALSE)
  per_skill[,
    contribution_pct := data.table::fifelse(
      distance > 0,
      contribution / distance,
      NA_real_
    )
  ]
  per_skill[, distance := NULL]

  # skill_label: aggiungiamo la colonna sempre, valorizzata se skill_labels c'è
  if (!is.null(skill_labels)) {
    lab <- skill_labels[, .(
      idescoskill_level_3,
      skill_label = escoskill_level_3
    )]
    per_skill <- merge(
      per_skill,
      lab,
      by = "idescoskill_level_3",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    per_skill[, skill_label := NA_character_]
  }

  data.table::setcolorder(
    per_skill,
    c(
      "idesco_level_4",
      "idescoskill_level_3",
      "skill_label",
      "declared",
      "profile_value",
      "contribution",
      "contribution_pct"
    )
  )

  # riordina per professione (nello stesso ordine del ranking) e per contributo
  per_skill[,
    idesco_level_4 := factor(idesco_level_4, levels = ranking$idesco_level_4)
  ]
  data.table::setorder(per_skill, idesco_level_4, -contribution)
  per_skill[, idesco_level_4 := as.character(idesco_level_4)]

  # 12. assemblaggio output -----
  list(
    ranking = ranking,
    per_skill = per_skill,
    basis = basis,
    query = skills,
    params = list(
      top_n = top_n,
      min_postings = min_postings,
      n_professions_considered = n_professions_considered,
      n_skills_in_universe = n_skills_in_universe,
      n_skills_resolved = n_skills_resolved,
      n_skills_unresolved = n_skills_unresolved
    )
  )
}


# 2. .empty_match_result (helper) -----

# Restituisce la struttura completa con tabelle vuote tipizzate quando il
# calcolo non può procedere (nessuna professione superstite, nessuna skill
# risolvibile, eccetera). Evita di duplicare il blocco di return su più rami.
.empty_match_result <- function(
  skills,
  basis,
  top_n,
  min_postings,
  n_professions_considered,
  n_skills_in_universe,
  n_skills_resolved,
  n_skills_unresolved,
  has_labels
) {
  ranking <- data.table::data.table(
    idesco_level_4 = character(),
    distance = numeric(),
    score = numeric(),
    hit_ratio = numeric(),
    mean_coverage = numeric(),
    n_postings = integer(),
    n_skills_matched = integer()
  )
  per_skill <- data.table::data.table(
    idesco_level_4 = character(),
    idescoskill_level_3 = character(),
    skill_label = character(),
    declared = logical(),
    profile_value = numeric(),
    contribution = numeric(),
    contribution_pct = numeric()
  )

  list(
    ranking = ranking,
    per_skill = per_skill,
    basis = basis,
    query = skills,
    params = list(
      top_n = top_n,
      min_postings = min_postings,
      n_professions_considered = n_professions_considered,
      n_skills_in_universe = n_skills_in_universe,
      n_skills_resolved = n_skills_resolved,
      n_skills_unresolved = n_skills_unresolved
    )
  )
}
