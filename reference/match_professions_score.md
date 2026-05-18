# Calcola la classifica delle professioni a partire da un prep

Scoring step: dato un oggetto preparato da
[`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
e un vettore di competenze dichiarate, calcola la distanza coseno fra il
profilo di ogni professione e la query, ordina le professioni per
distanza crescente e restituisce la decomposizione del contributo di
ciascuna competenza alla distanza totale per le top-N professioni.

## Usage

``` r
match_professions_score(
  prep,
  skills,
  weights = NULL,
  top_n = 20L,
  skill_labels = NULL
)
```

## Arguments

- prep:

  Oggetto di classe `"match_professions_prep"` prodotto da
  [`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md).

- skills:

  Vettore character di identificativi ESCO L3 (`idescoskill_level_3`).
  Eventuali `NA` e duplicati vengono rimossi.

- weights:

  Vettore numerico non negativo di pesi per le competenze in `skills`.
  Se `NULL` (default) tutte le competenze ricevono peso uniforme
  (comportamento storico). Se fornito deve avere lunghezza pari a
  `length(unique(skills[!is.na(skills)]))`. Se ha nomi, questi
  sovrascrivono l'allineamento posizionale e vengono usati per
  accoppiare il peso alla corrispondente competenza. Tutti i pesi devono
  essere finiti e non negativi; non possono essere tutti zero.

- top_n:

  Numero intero di professioni da restituire in `ranking` e per le quali
  emettere la decomposizione `per_skill`. Default `20L`.

- skill_labels:

  `data.table` opzionale con almeno le colonne `idescoskill_level_3` e
  `escoskill_level_3` (etichetta leggibile). Quando fornito, la tabella
  `per_skill` ottiene la colonna `skill_label`. Default `NULL`.

## Value

Una `list` con i seguenti elementi:

- `ranking`:

  `data.table` con una riga per professione (al piu' `top_n`), ordinata
  per `distance` crescente. Colonne: `idesco_level_4`, `distance` (in
  `[0, 1]`), `score = 1 - distance`, `hit_ratio` (massa L2 delle
  competenze dichiarate che la professione richiede almeno una volta,
  normalizzata sulla massa totale; coincide con la frazione di
  competenze dichiarate nel caso non pesato), `mean_coverage` (media
  pesata di `coverage_{p,s}` sulle competenze dichiarate; `NA` se
  `basis != "coverage"`), `n_postings`, `n_skills_matched`.

- `per_skill`:

  `data.table` in formato lungo (vedi
  [`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
  per la descrizione completa delle colonne).

- `basis`:

  La base usata, per introspezione.

- `query`:

  Il vettore `skills` deduplicato.

- `params`:

  Lista con `top_n`, `min_postings`, `n_professions_considered`,
  `n_skills_in_universe`, `n_skills_resolved`, `n_skills_unresolved`.

## Details

La distanza usata e' la distanza coseno (`1 - cos(v_p, q)`), equivalente
a meta' della distanza euclidea quadrata fra vettori L2-normalizzati.
Questa scelta e' l'unica metrica della famiglia che fornisce una
decomposizione additiva pulita per singola competenza: \$\$1 - \cos(v_p,
q) = \frac{1}{2} \sum_s ( \tilde v\_{p,s} - \tilde q_s )^2.\$\$

## See also

[`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
per la fase di prep,
[`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
per il wrapper end-to-end.

## Examples

``` r
if (FALSE) { # \dontrun{
con  <- itaposts::oja_connect()
on.exit(itaposts::oja_disconnect(con), add = TRUE)

post <- itaposts::oja_postings(con) |> dplyr::collect() |> data.table::setDT()
skil <- itaposts::oja_skills(con)   |> dplyr::collect() |> data.table::setDT()

prep <- skillviz::match_professions_prepare(post, skil, basis = "coverage")

# query non pesata (uniforme)
res_uniform <- skillviz::match_professions_score(
  prep,
  c("S1.4.1", "S4.8.1", "S1.13.2")
)

# query pesata: la prima skill conta il doppio
res_weighted <- skillviz::match_professions_score(
  prep,
  c("S1.4.1", "S4.8.1", "S1.13.2"),
  weights = c(2, 1, 1)
)
} # }
```
