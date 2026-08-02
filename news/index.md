# Changelog

## skillviz 0.2.0

### Riproducibilità di `predict_cp4_knn()`

- Il campionamento che limita la dimensione del pool di addestramento
  per gruppo ESCO non usa più
  [`sample.int()`](https://rdrr.io/r/base/sample.html) senza seme, ma
  una selezione deterministica a passo costante sull’ordine già fissato
  dal chiamante. Il limite **scatta in produzione**: il gruppo ESCO 5223
  conta circa 52.700 annunci etichettati sulla finestra di 24 mesi,
  quindi finora due esecuzioni sullo stesso input restituivano
  previsioni diverse per quel gruppo. L’ordine non viene riordinato,
  perché determina anche il criterio di spareggio fra similarità uguali:
  riordinarlo cambierebbe le previsioni di tutti i gruppi, non solo di
  quelli troncati.
- Nuovo argomento `max_train` (default 50000, il valore finora cablato
  nel codice) per usare interi i gruppi più grandi. Quando il limite
  scatta viene emesso un avviso, prima l’evento era silenzioso.

### Recupero degli annunci senza codice ESCO

- Nuovo argomento `rescue_no_match` (default `FALSE`, che riproduce il
  comportamento precedente). Con `TRUE` gli annunci privi di
  `idesco_level_4` ma dotati di competenze vengono classificati da un
  k-NN Jaccard non ristretto sull’intero pool etichettato e restituiti
  con `method = "knn_global"`. Riguarda il 13,1% degli annunci non
  etichettati, di cui il 94,4% possiede competenze; in precedenza
  restavano `no_match` e venivano scartati a valle. Accuratezza attesa
  circa 74% CP4 / 78% CP3, contro l’86% del percorso ristretto per ESCO,
  quindi la `confidence` restituita è il filtro da usare. Argomenti
  `rescue_k` e `rescue_max_train` per regolarne il costo.

### Lettura dei dati OJA da itaposts

- Nuova dipendenza formale da `itaposts` (`Imports`, installato tramite
  `Remotes: gmontaletti/itaposts`), che centralizza import,
  normalizzazione e archiviazione dei dati OJA nello store DuckDB
  condiviso.
- [`read_oja_itaposts()`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md)
  — nuova funzione, modalità raccomandata per caricare i dati OJA in
  skillviz. Delega a
  [`itaposts::oja_normalised()`](https://rdrr.io/pkg/itaposts/man/oja_normalised.html)
  e restituisce la stessa lista a tre elementi
  `list(postings, skills, companies)` prodotta da
  [`normalize_ojv()`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md),
  con le tre tabelle chiavate su `general_id`. La connessione resta a
  carico del chiamante, secondo l’idioma itaposts
  ([`itaposts::oja_connect()`](https://rdrr.io/pkg/itaposts/man/oja_connect.html)
  /
  [`itaposts::oja_disconnect()`](https://rdrr.io/pkg/itaposts/man/oja_disconnect.html)).
  Accetta i filtri `snapshots`, `region_code`, `years` e `months`.
- Le colonne di skill che itaposts riemette con i nomi maiuscoli storici
  Lightcast (`ESCOSKILL_LEVEL_3`, `ESCO_V0101_REUSETYPE`,
  `ESCO_V0101_GREEN`, `ESCO_V0101_LANGUAGE`, …) vengono convertite in
  minuscolo, i nomi già attesi dal resto del pacchetto. Solo le colonne
  effettivamente presenti vengono rinominate, quindi la funzione tollera
  variazioni nell’insieme di colonne esposto da itaposts.
- Gli esempi di
  [`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md),
  [`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
  e
  [`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
  usano
  [`read_oja_itaposts()`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md):
  le chiamate dirette a
  [`itaposts::oja_postings()`](https://rdrr.io/pkg/itaposts/man/oja_postings.html)
  che vi figuravano non espongono `idesco_level_4`, richiesto da queste
  funzioni.

### Colonne `pillar_softskills` e `esco_v0101_ict` opzionali

- [`build_skillist()`](https://gmontaletti.github.io/skillviz/reference/build_skillist.md)
  non richiede più `pillar_softskills` e `esco_v0101_ict` fra le colonne
  di `skills`. Le due colonne restano sempre presenti nell’output e
  valgono `NA_integer_` quando assenti in input; la classificazione
  `tipo` deriva dal solo `esco_v0101_reusetype` e non cambia. Le
  sorgenti che ancora portano i due flag li propagano invariati.
- La modifica segue la migrazione `data_v2` di `itaposts`: il fornitore
  ha rimosso i due flag dalla consegna e non esiste alcun sostituto. I
  dati letti con
  [`read_oja_itaposts()`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md)
  riportano quindi `NA` in entrambe le colonne.

### Deprecazioni

- [`read_ojv_zip()`](https://gmontaletti.github.io/skillviz/reference/read_ojv_zip.md)
  e
  [`normalize_ojv()`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md)
  sono deprecate in favore di
  [`read_oja_itaposts()`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md).
  Le due funzioni continuano a operare ed emettono un avviso di
  deprecazione; la lettura diretta degli archivi ZIP Lightcast è
  superata dallo store DuckDB di `itaposts`, che possiede la stessa
  logica di lettura, deduplicazione e join. La rimozione è prevista in
  una versione futura, non prima di una MINOR successiva.

### API split per scoring veloce

- [`match_professions_prepare()`](https://gmontaletti.github.io/skillviz/reference/match_professions_prepare.md)
  e
  [`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
  separano la costruzione della matrice professione × competenza
  (operazione costosa, invariante rispetto alla query) dal calcolo della
  distanza rispetto a un insieme di competenze dichiarate (operazione
  leggera, sparsa). Il wrapper
  [`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
  resta invariato per backward compatibility e delega a entrambi
  internamente. Usare il pattern split nelle interfacce reattive che
  valutano molte query sulla stessa base.

### Query pesata

- [`match_professions_score()`](https://gmontaletti.github.io/skillviz/reference/match_professions_score.md)
  e
  [`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
  accettano l’argomento `weights` (numerico, non negativo, una entry per
  ogni competenza dichiarata). I pesi modulano la massa L2 del vettore
  di query; con pesi uniformi a 1 il comportamento coincide bit-per-bit
  con la versione non pesata. I denominatori di `hit_ratio` e
  `mean_coverage` usano la somma dei pesi al quadrato per mantenere
  coerenza dimensionale.

### Nuove funzionalità

- [`match_professions()`](https://gmontaletti.github.io/skillviz/reference/match_professions.md)
  — a partire da un vettore di identificativi ESCO L3 dichiarati,
  classifica le professioni ESCO L4 per affinità al profilo desiderato.
  Restituisce una graduatoria con distanza coseno complessiva e la
  decomposizione additiva del contributo di ogni competenza alla
  distanza totale. Tre basi di profilo selezionabili: `coverage`
  (probabilità che un annuncio della professione richieda la
  competenza), `tfidf` (term frequency per professione × inverse
  document frequency, pesa le competenze discriminanti) e `rca` (indice
  di Balassa, misura di specializzazione). Riusa
  [`build_skill_prof_sparse()`](https://gmontaletti.github.io/skillviz/reference/build_skill_prof_sparse.md)
  per la matrice sparsa professioni × competenze e
  [`compute_balassa_index()`](https://gmontaletti.github.io/skillviz/reference/compute_balassa_index.md)
  per la base RCA.

## skillviz 0.1.0

Versione iniziale.
