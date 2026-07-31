# Classifica le professioni ESCO L4 piu' affini a un insieme di competenze

A partire da un vettore di identificativi ESCO L3 dichiarati
dall'utente, costruisce il profilo empirico di ogni professione ESCO L4
(a partire dalle offerte di lavoro) e ne calcola la distanza dal profilo
desiderato. Le professioni vengono ordinate per distanza crescente e per
ogni professione viene restituita la decomposizione del contributo di
ciascuna competenza alla distanza totale.

## Usage

``` r
match_professions(
  skills,
  postings,
  skills_long,
  basis = c("coverage", "tfidf", "rca"),
  top_n = 20L,
  min_postings = 30L,
  skill_labels = NULL,
  weights = NULL
)
```

## Arguments

- skills:

  Vettore character di identificativi ESCO L3 (`idescoskill_level_3`).
  Eventuali `NA` e duplicati vengono rimossi.

- postings:

  `data.table` con almeno le colonne `general_id` e `idesco_level_4`.
  Tipicamente ottenuto da `read_oja_itaposts(con)$postings`:
  [`itaposts::oja_postings()`](https://rdrr.io/pkg/itaposts/man/oja_postings.html)
  da sola non espone `idesco_level_4`.

- skills_long:

  `data.table` in formato lungo con almeno le colonne `general_id` e
  `idescoskill_level_3`. Una riga per coppia (annuncio, competenza).
  Tipicamente ottenuto da `read_oja_itaposts(con)$skills`.

- basis:

  Base del profilo professionale: `"coverage"` (default), `"tfidf"`
  oppure `"rca"`.

- top_n:

  Numero intero di professioni da restituire in `ranking` e per le quali
  emettere la decomposizione `per_skill`. Default `20L`.

- min_postings:

  Numero minimo di annunci per professione necessari per ritenere
  stabile il profilo. Le professioni sotto soglia vengono scartate prima
  del calcolo. Default `30L`.

- skill_labels:

  `data.table` opzionale con almeno le colonne `idescoskill_level_3` e
  `escoskill_level_3` (etichetta leggibile). Quando fornito, la tabella
  `per_skill` ottiene la colonna `skill_label`. Default `NULL`.
  Tipicamente ottenuto deduplicando le due colonne su
  `read_oja_itaposts(con)$skills`:
  [`itaposts::oja_skill_dim()`](https://rdrr.io/pkg/itaposts/man/oja_skill_dim.html)
  espone l'etichetta come `esco_v010200_label`, non come
  `escoskill_level_3`.

- weights:

  Vettore numerico non negativo di pesi per le competenze in `skills`.
  Se `NULL` (default) tutte le competenze ricevono peso uniforme. Quando
  fornito deve avere lunghezza pari a `length(skills)` dopo
  deduplicazione di `NA` e duplicati; vedi
  [`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
  per i dettagli.

## Value

Una `list` con i seguenti elementi:

- `ranking`:

  `data.table` con una riga per professione (al piu' `top_n`), ordinata
  per `distance` crescente. Colonne: `idesco_level_4`, `distance` (in
  `[0, 1]`), `score = 1 - distance`, `hit_ratio` (frazione delle
  competenze dichiarate che la professione richiede almeno una volta;
  nel caso pesato e' la massa quadratica delle competenze matchate
  normalizzata sulla massa totale), `mean_coverage` (media di
  `coverage_{p,s}` sulle competenze dichiarate; `NA` se
  `basis != "coverage"`), `n_postings`, `n_skills_matched`.

- `per_skill`:

  `data.table` in formato lungo con una riga per ciascuna coppia
  (professione top-N, competenza con contributo non nullo). La lista
  delle righe include sia le competenze dichiarate sia quelle richieste
  dalla professione ma non dichiarate, in modo che valga l'identita'
  additiva `sum(contribution) == distance` per ogni professione.
  Colonne: `idesco_level_4`, `idescoskill_level_3`, `skill_label` (`NA`
  se `skill_labels` non fornito), `declared` (logico, `TRUE` se la
  competenza e' nella query), `profile_value` (valore grezzo di `v_p[s]`
  nella base scelta, pre-normalizzazione; `NA` se la competenza non e'
  nell'universo della professione), `contribution` (addendo della
  distanza), `contribution_pct` (`contribution / distance`; `NA` se
  `distance == 0`).

- `basis`:

  La base usata, per introspezione.

- `query`:

  Il vettore `skills` deduplicato.

- `params`:

  Lista con `top_n`, `min_postings`, `n_professions_considered`,
  `n_skills_in_universe`, `n_skills_resolved`, `n_skills_unresolved`.

Nota sull'invariante: la somma di `contribution` su tutte le righe di
`per_skill` per una data professione coincide con `distance` perche'
sono incluse anche le competenze richieste dalla professione ma non
dichiarate. Filtrando su `declared == TRUE` la somma sara' invece
inferiore a `distance`, e il divario rappresenta la massa che la
professione richiede al di fuori della query.

## Details

Questa funzione e' un wrapper sottile su
[`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md) +
[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md).
Per uso ad alta frequenza (per esempio dashboard interattive) e'
preferibile invocare
[`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
una sola volta per data refresh e chiamare
[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
ad ogni interazione utente: la costruzione della matrice profilo e' il
costo dominante.

La distanza usata e' la distanza coseno (`1 - cos(v_p, q)`), equivalente
a meta' della distanza euclidea quadrata fra vettori L2-normalizzati.
Questa scelta e' l'unica metrica della famiglia che fornisce una
decomposizione additiva pulita per singola competenza: \$\$1 - \cos(v_p,
q) = \frac{1}{2} \sum_s ( \tilde v\_{p,s} - \tilde q_s )^2.\$\$

Il profilo `v_p` di ogni professione e' costruito secondo la base
scelta:

- `coverage`:

  `n_{p,s} / N_p`, probabilita' che un annuncio di `p` richieda `s`.
  Interpretabile come quota di copertura.

- `tfidf`:

  `tf_{p,s} * idf_s` con `tf_{p,s} = n_{p,s} / sum_s n_{p,s}` calcolata
  internamente alla professione, e `idf_s = log(P / df_s)` dove `df_s`
  e' il numero di professioni che richiedono `s` (dopo filtro
  `min_postings`) e `P` il numero totale di professioni superstiti. Pesa
  di piu' le competenze discriminanti.

- `rca`:

  indice di Balassa, delegato a
  [`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md).
  Misura la specializzazione: `>1` indica sovra-rappresentazione.

Il vettore di query `q` e' costruito a partire da `weights` (default
uniforme `1` su ogni competenza dichiarata); sia `q` che le colonne di
`M` vengono L2-normalizzati prima del calcolo.

## See also

[`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
e
[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
per il workflow "prepara una volta, scoring molte volte",
[`compute_profession_distance()`](https://gmontaletti.github.io/skillviz/reference/compute_profession_distance.md)
per la distanza fra professioni,
[`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md)
per l'indice di Balassa usato dalla base `rca`,
[`build_skill_prof_sparse()`](https://gmontaletti.github.io/skillviz/reference/build_skill_prof_sparse.md)
per la matrice competenze x professioni.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- itaposts::oja_connect()
on.exit(itaposts::oja_disconnect(con), add = TRUE)

ojv <- skillviz::read_oja_itaposts(con)

# le etichette leggibili viaggiano con le righe di skill
labs <- unique(ojv$skills[, .(idescoskill_level_3, escoskill_level_3)])

# convenience: end-to-end in una sola chiamata
res <- skillviz::match_professions(
  skills       = c("S1.4.1", "S4.8.1", "S1.13.2"),
  postings     = ojv$postings,
  skills_long  = ojv$skills,
  basis        = "coverage",
  top_n        = 10,
  skill_labels = labs
)

res$ranking
res$per_skill[idesco_level_4 == res$ranking$idesco_level_4[1L]]

# prepara una volta, scoring molte volte
prep <- skillviz::match_professions_prepare(
  ojv$postings, ojv$skills, basis = "coverage"
)
res1 <- skillviz::match_professions_score(prep, c("S1.4.1", "S4.8.1"))
res2 <- skillviz::match_professions_score(
  prep, c("S2.1.3"), weights = c(2.0)
)
} # }
```
