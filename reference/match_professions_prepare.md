# Pre-computa la matrice profilo professionale per il matching

Prepara l'oggetto riusabile contenente la matrice sparsa competenze x
professioni (`M`) e la sua versione L2-normalizzata (`M_norm`), insieme
ai conteggi e all'universo di skill e professioni. L'oggetto puo' essere
riutilizzato da
[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
su query diverse senza ripagare il costo della costruzione del profilo,
che dipende solo dalle tabelle `postings` e `skills_long`.

## Usage

``` r
match_professions_prepare(
  postings,
  skills_long,
  basis = c("coverage", "tfidf", "rca"),
  min_postings = 30L
)
```

## Arguments

- postings:

  `data.table` con almeno le colonne `general_id` e `idesco_level_4`.
  Tipicamente ottenuto da
  `itaposts::oja_postings(con) |> dplyr::collect()`.

- skills_long:

  `data.table` in formato lungo con almeno le colonne `general_id` e
  `idescoskill_level_3`. Una riga per coppia (annuncio, competenza).
  Tipicamente ottenuto da
  `itaposts::oja_skills(con) |> dplyr::collect()`.

- basis:

  Base del profilo professionale: `"coverage"` (default), `"tfidf"`
  oppure `"rca"`.

- min_postings:

  Numero minimo di annunci per professione necessari per ritenere
  stabile il profilo. Le professioni sotto soglia vengono scartate prima
  del calcolo. Default `30L`.

## Value

Una `list` con classe `"match_professions_prep"` e i seguenti elementi:

- `M`:

  `dgCMatrix` sparsa skill x professione con i valori grezzi del profilo
  nella base scelta (pre-normalizzazione).

- `M_norm`:

  `dgCMatrix` sparsa con le colonne di `M` divise per la propria norma
  L2. Le colonne con norma nulla vengono scartate.

- `col_norms`:

  Vettore numerico delle norme L2 delle colonne di `M`, con nomi pari a
  `colnames(M)`.

- `universe_skills`:

  Character con `rownames(M)`.

- `universe_profs`:

  Character con `colnames(M)` (post-scarto delle colonne a norma nulla).

- `counts`:

  `data.table` con `idesco_level_4`, `idescoskill_level_3`, `n_ps`,
  `N_p`, `value` (dipendente dalla base) e `coverage_ps` (sempre
  presente; usata da
  [`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
  per `mean_coverage` e `hit_ratio`).

- `n_per_prof`:

  `data.table` con `idesco_level_4` e `N_p` (numero di annunci per
  professione, dopo filtro `min_postings`).

- `basis`:

  La base risolta.

- `min_postings`:

  La soglia applicata.

Se nessuna professione supera `min_postings`, oppure se la giunzione
`postings`-`skills_long` e' vuota, viene restituito un oggetto con la
medesima struttura ma con `M`, `M_norm`, `counts` e `universe_*` vuoti.
[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
gestisce questo caso restituendo tabelle vuote ma tipizzate.

## Details

Per la semantica delle basi (`coverage`, `tfidf`, `rca`) si veda
[`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md).

## See also

[`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
per il calcolo della distanza a partire da un prep,
[`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
per il wrapper end-to-end.

## Examples

``` r
if (FALSE) { # \dontrun{
con  <- itaposts::oja_connect()
on.exit(itaposts::oja_disconnect(con), add = TRUE)

post <- itaposts::oja_postings(con)  |> dplyr::collect() |> data.table::setDT()
skil <- itaposts::oja_skills(con)    |> dplyr::collect() |> data.table::setDT()

# prepara una sola volta, riusa per molte query
prep <- skillviz::match_professions_prepare(
  postings = post,
  skills_long = skil,
  basis = "coverage"
)

res1 <- skillviz::match_professions_score(prep, c("S1.4.1", "S4.8.1"))
res2 <- skillviz::match_professions_score(prep, c("S2.1.3"))
} # }
```
