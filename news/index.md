# Changelog

## skillviz 0.4.1

- Nuovo documento `container/LIMITI.md`: riferimento per chi consuma
  `staging.gm_cp4_imputed`, con ciò che le procedure **non**
  garantiscono e la misura che quantifica ciascun limite. Il primo è
  quello che condiziona tutti gli altri — nessuna validazione riguarda
  la popolazione effettivamente trattata, perché ogni accuratezza è
  misurata su annunci già codificati dal fornitore e applicata a quelli
  che non lo sono.

- Corretta la voce di changelog della 0.4.0, che descriveva uno stato
  mai rilasciato: riportava il restrittore di livello 5 come non
  predefinito e la validazione walk-forward come non superata, mentre la
  versione pubblicata contiene il criterio corretto, il gate superato e
  il default già invertito.

## skillviz 0.4.0

### Restrittore ESCO di livello 5

- [`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md)
  accetta ora `restrictor`, che sceglie la colonna ESCO su cui si
  restringe lo spazio dei candidati. **Il valore predefinito è
  `"idesco_level_5"`**; `"idesco_level_4"` resta disponibile e riproduce
  il comportamento precedente.

- Il livello 5 è un restrittore nettamente migliore: 2.714 gruppi contro
  399, in media 23,9 candidati CP5 per gruppo contro 65,8, e
  **accuratezza CP5 dell’89,1% contro l’85,9%** sulle righe decise dal
  voto k-NN (+3,2 punti; al livello 4 del codice CP il guadagno è di
  +3,1 punti). La copertura cala di appena 0,018 punti. Vince in tutte
  le fasce di ampiezza del pool di addestramento sopra le 10 righe
  etichettate e perde solo nella più sottile (66,0% contro 73,1% fra 1 e
  10 righe).

- È diventato predefinito dopo la validazione walk-forward richiesta
  dalla regola fissata in precedenza: allenando su ogni mese etichettato
  precedente a quello di test, CP5 **+3,95 punti** appaiati e positivo
  in **6 mesi su 6** (da +3,61 a +4,54), CP4 +3,81, copertura −0,042
  punti, guadagno positivo in tutte le fasce di ampiezza del pool
  compresa la più sottile. È anche meglio calibrato del restrittore che
  sostituisce (errore di calibrazione atteso 5,5 punti contro 6,3) e più
  economico: 541 s contro 772 s, 5,64 GB di picco contro 6,41.

- La prima esecuzione del gate era fallita su un criterio secondario —
  monotonia della confidenza sui decili, 4 mesi su 6 anziché 5 — e in
  quell’occasione il restrittore **non** era stato adottato. Il criterio
  si è poi rivelato difettoso e non semplicemente scomodo: bocciava il
  livello 4 in carica più severamente (3 mesi su 6), perché i decili dal
  quinto al decimo stanno tutti a confidenza esattamente 100%, un unico
  blocco di pari merito che copre il 60% delle righe dove non esiste
  alcun ordinamento da verificare. È stato sostituito, **prima** di
  rieseguire, da due criteri comparativi: errore di calibrazione
  relativo al livello 4 con margine di 1,0 punti, e monotonia limitata
  ai decili in cui la confidenza varia davvero. Sul gate corretto il
  livello 5 supera ogni criterio.

- Restano valide due avvertenze indipendenti dall’esito: tutte le misure
  confrontano righe etichettate con righe etichettate, mentre gli
  annunci privi di codice sono 1,55 volte più lunghi e l’etichettatura è
  fortemente non casuale fra gruppi e fonti.

- La stessa esecuzione ha misurato un dato che vincola il container: una
  previsione di livello 5 ristretta su ESCO5 e una di livello 4
  ristretta su ESCO4 **discordano sul 12,2% delle righe**. Poiché il
  container scarta un codice di livello 5 il cui genitore contraddice la
  colonna di livello 4, una configurazione mista scarterebbe
  silenziosamente quelle righe: i due livelli si spostano insieme oppure
  nessuno dei due.

- Nuovo argomento `restrictor_na`, che normalizza a `NA` i valori
  sentinella del restrittore. Serve al livello 5, dove l’assenza di
  codice ESCO è scritta come la stringa `"Unclassifiable"` e non come
  `NA`: lasciata così sarebbe un valore di gruppo legittimo, e circa
  168.000 annunci confluirebbero in un unico pseudo-gruppo senza alcun
  errore visibile.

- Al livello 5 il pareggio fra codici con la stessa frequenza è ora
  risolto in modo deterministico sul codice CP.
  [`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
  conserva il criterio precedente, che dipende dall’ordine delle righe
  in ingresso, perché cambiarlo ne sposterebbe le previsioni.

### Crosswalk completo ESCO5 → CP2021 livello 5

- Nuova funzione
  [`build_esco_cp_crosswalk()`](https://gmontaletti.github.io/skillviz/reference/build_esco_cp_crosswalk.md):
  ricava dagli annunci etichettati la corrispondenza empirica fra codice
  ESCO e codici CP2021, e restituisce sia la struttura completa dei
  candidati sia una sintesi per gruppo estesa all’intera
  classificazione, non solo ai codici che compaiono nei dati.

- **Non è una funzione di lookup.** Il codice CP5 modale è corretto nel
  73,4% dei casi, ponderando per riga; la concentrazione sale lentamente
  (85,8% ai primi due codici, 90,3% ai primi tre) e solo lo 0,46% delle
  righe sta in un gruppo il cui codice modale copre l’intero gruppo. È
  lo spazio dei candidati entro cui vota il k-NN, ed è il voto a portare
  il 73% all’89%. La colonna `modal_reliable` marca la porzione
  utilizzabile come lookup diretto, circa un terzo delle righe.

- La sintesi per gruppo riporta `coverage_labelled`, la quota di annunci
  del gruppo che porta effettivamente un codice: 1.200 gruppi di livello
  5 sotto il 25% coprono 491.739 annunci, e il gruppo mediano ne ha
  codificato meno di un terzo. Il codice modale di quei gruppi è stimato
  su un campione selezionato, non su un campione casuale.

- Con l’argomento `strata_col` la funzione quantifica quella distorsione
  invece di limitarsi a segnalarla: ricalcola il codice modale ripesando
  ogni fonte sulla sua quota fra gli annunci **non** codificati del
  gruppo, e la colonna `modal_stable` dice se il codice regge. Misurato
  sull’intero archivio, il codice modale cambia in 497 gruppi su 2.708
  (18,4%), che coprono il **13,7% degli annunci privi di codice**. Il
  movimento segue la copertura come previsto dal meccanismo: 24,8% dei
  gruppi nella fascia 0-10% di copertura, 3,7% nella fascia 75-100%. La
  quota modale complessiva si sposta appena (dal 51,4% al 50,9%), quindi
  si tratta di scambi fra codici dentro gruppi ambigui, non di un
  crollo. La distorsione riguarda il crosswalk, non la scelta del
  restrittore: vale identica al livello 4.

- Un **backoff gerarchico** (livello 5 dove il pool è ampio, altrimenti
  il genitore di livello 4) è stato misurato per soglie 10, 30, 50 e
  100, sia in campione sia fuori campione nel tempo: **perde** fra 0,3 e
  3,0 punti e non migliora mai la copertura, perché si limita a spostare
  righe fra i due rami.

### Container: una sola passata, nessun riempimento

- Il container esegue ora **una sola** passata k-NN, a livello 5, e
  ricava il livello 4 per troncamento invece di votarlo separatamente.
  Il troncamento riproduce il voto diretto entro 0,004 punti in misura
  contemporanea e 0,02 walk-forward, mentre la ricerca dei vicini è il
  98,6% del tempo di esecuzione: una seconda passata raddoppierebbe il
  costo per riottenere la stessa risposta. Elimina anche un modo di
  fallire — votando i due livelli separatamente potevano discordare, e
  un codice di livello 5 in contrasto con la colonna di livello 4 andava
  scartato. Sotto troncamento la gerarchia è un invariante strutturale e
  una violazione interrompe l’esecuzione.

- **La convenzione del `.0` è stata rimossa.** Le versioni precedenti
  registravano codici precisi al livello 3 nella colonna di livello 4,
  riempiti a `<cp3>.0`, perché il segmento privo di ESCO era modellabile
  solo a livello 3. Ora ogni modello predice a livello 5, quindi un
  codice di livello 4 nella tabella è sempre un codice di livello 4
  autentico. Chi selezionava le righe precise con `code NOT LIKE '%.0'`
  non ne ha più bisogno.

- Il percorso per gli annunci privi di codice ESCO predice ora a livello
  5 (76,4%, contro l’84,1% del k-NN dove l’ESCO c’è). Il presupposto che
  lo dichiarava impraticabile era doppiamente sbagliato: le classi
  effettive sono 102 dopo l’accorpamento delle rare, non centinaia, e il
  costo proibitivo veniva dalla codifica, non dal livello.

- **`vtreat` è stato rimosso, non aggirato.** Su una ripartizione
  temporale di tre mesi sui 42.530 annunci etichettati privi di ESCO,
  l’impact coding è insieme più lento e meno accurato di un semplice
  one-hot: a livello 3, 736 s e 80,09% contro 21 s e 81,34%; a livello
  5, 2.953 s e 75,44% contro 35 s e 76,36%. Costruisce una matrice densa
  la cui larghezza cresce col numero di classi — la ragione per cui il
  livello fine era ritenuto impossibile su quel segmento — mentre il
  one-hot resta sparso e costa le stesse 1.531 colonne con 56 classi o
  con 102.

- Rimosse le variabili `IMPUTE_CP5` e `IMPUTE_XGB_PERIOD_A_END`. Il
  livello 5 non è più un’aggiunta opzionale ma il modello, e la ricetta
  vtreat che la seconda governava non esiste più.

## skillviz 0.3.0

### Codifica CP2021 al livello 5

- Nuova funzione
  [`predict_cp5_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp5_knn.md):
  assegna i codici CP2021 di livello 5 (unità professionali) agli
  annunci privi di codice, con lo stesso metodo già in uso al livello 4.
  Il codice ESCO di livello 4 restringe lo spazio dei candidati alle
  unità professionali osservate per quel gruppo, poi il voto k-NN su
  similarità di Jaccard fra i vettori binari delle competenze
  disambigua, con il consueto rafforzamento dei vicini dello stesso
  settore.

- Validata sulla finestra di produzione di 24 mesi (5 ripartizioni
  stratificate contemporanee, `skillviz_workflow/run_cp5_knn.R`):
  **accuratezza CP5 dell’85,9% sulle righe decise dal voto k-NN**,
  contro l’86,2% di
  [`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
  sulle stesse righe. Il livello più fine costa quindi circa 0,25 punti
  percentuali. Su tutte le righe di test, cascata di ripiego compresa,
  l’accuratezza è del 79,9%.

- Il livello 5 è appena più difficile del livello 4 perché la gerarchia
  è stretta: dei 813 codici CP5, 510 genitori CP4 hanno in media 1,60
  figli e 340 ne hanno esattamente uno, così il 67,2% delle righe
  etichettate ricade sotto un CP4 il cui CP5 è già determinato dalla
  classificazione.

- La confidenza restituita è monotòna rispetto all’accuratezza su tutti
  e dieci i decili ed è il filtro previsto: il 10% di righe più
  affidabili è accurato al 97,1%, il 50% al 96,9%. **Le soglie vanno
  ricavate da questa scala, non ereditate dal livello 4**: a parità di
  `k` i voti si distribuiscono su più classi, quindi lo stesso valore di
  confidenza significa una cosa diversa.

- Gli annunci con `idesco_level_5 = "Unclassifiable"` non hanno codice
  ESCO ad alcun livello e restano senza codice di livello 5
  (`method = "no_match"`, il 5,6% delle righe) se non si attiva
  `rescue_no_match`.

### Comportamento invariato

- [`predict_cp4_knn()`](https://gmontaletti.github.io/skillviz/reference/predict_cp4_knn.md)
  non cambia: firma, argomenti predefiniti, valore di ritorno e messaggi
  restano quelli della 0.2.0. Le due funzioni condividono ora un motore
  interno comune, e l’identità bit a bit dell’uscita di livello 4 è
  verificata da una regressione su 32 combinazioni di argomenti
  congelate prima della modifica
  (`tests/testthat/fixtures/cp4_baseline.rds`).

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
