# Il classificatore a centroidi: flusso, caratteristiche, esito

Nota di progetto su `build_cp_profiles()` e `predict_cp5_centroid()`, misurati da
`skillviz_workflow/run_cp5_centroid.R` il 2026-08-07.

**Stato: non adottato.** Il gate pre-registrato è fallito su un solo criterio,
la memoria. Nulla nella pipeline o nel container lo chiama. Questo documento
serve a poterlo riprendere senza rifare il ragionamento da capo.

---

## 1. Perché è stato tentato

`predict_cp5_knn()` tiene l'intero insieme etichettato in memoria al momento
della codifica e spende **il 98,6% del tempo** a cercarci dentro i vicini. Ne
discendono tre limiti operativi, indipendenti dall'accuratezza:

- il modello non esiste come oggetto: non si versiona, non si spedisce, non si
  ispeziona;
- i dati di addestramento devono essere presenti quando si codifica;
- il costo cresce con il **prodotto** fra righe da codificare e ampiezza del
  pool, non con le sole righe da codificare.

Un modello a centroidi (Rocchio) sostituisce la ricerca con una matrice sparsa
competenze × classe calcolata una volta sola. La previsione diventa un prodotto
sparso.

---

## 2. Il flusso

### 2.1 Costruzione — `build_cp_profiles()`

```
annunci etichettati
      │
      ├─ normalizza il sentinella del restrittore ("Unclassifiable" → NA)
      ├─ scarta le righe senza codice o senza gruppo
      │
      ├─ indice delle coppie (gruppo ESCO5, codice CP5) con n_c
      │     └─ filtro min_support
      │     └─ ordinamento per gruppo, poi per codice
      │        (così un argmax "primo massimo" spareggia sul codice minore,
      │         esattamente come fa la cascata modale)
      │
      ├─ conteggi sparsi delle competenze  per coppia   [coppie × V]
      ├─ conteggi sparsi delle competenze  per gruppo   [gruppi × V]
      ├─ conteggi sparsi dei settori       per coppia   (se sector = TRUE)
      └─ vettori di ponderazione diagonale (idf, balassa) se richiesti
```

L'oggetto restituito conserva i **conteggi**, non le distribuzioni lisciate. Lo
smoothing e lo shrinkage li applica la previsione: così una sola costruzione
serve molti parametri di punteggio e l'oggetto resta sparso. La sua dimensione
dipende dal numero di coppie (gruppo, codice) — circa 24.000 — non dal numero di
annunci.

### 2.2 Previsione — `predict_cp5_centroid()`

```
per ciascun gruppo ESCO5 presente fra le righe da codificare:
      │
      ├─ 1 solo candidato        → assegnazione diretta, method = "single_candidate"
      ├─ gruppo assente          → method = "no_match"
      │
      └─ altrimenti materializza il blocco del gruppo:
            θ̂_c = (conteggi_c + α) / normalizzatore          Lidstone
            π_e = profilo dell'intero gruppo
            θ_c = λ_c·θ̂_c + (1−λ_c)·π_e,  λ_c = n_c/(n_c+β)   shrinkage
            │
            ├─ punteggio = prior_weight·log n_c + s(q, θ_c) [+ settore]
            ├─ argmax con spareggio sul codice minore
            └─ confidenza = quota softmax del vincitore
                 └─ annuncio senza competenze note → decide la sola prior,
                    method = "frequency" (come fa predict_cp5_knn)
```

Il blocco denso di un gruppo è minuscolo — una decina di candidati × 1.685
competenze — quindi la memoria resta piatta qualunque sia la dimensione del
gruppo; le righe da codificare vengono spezzate secondo `dense_budget`.

### 2.3 La forma della regola non è arbitraria

```
score(q, c) = prior_weight · log n_c  +  s(q, θ_c)  [+ termine di settore]
```

È scritta così perché **rende lo shrinkage falsificabile**. Quando β cresce ogni
θ_c collassa su π_e, il termine di similarità diventa costante fra i candidati e
il ranking si riduce a `log n_c` — che *è* la cascata modale. Un'implementazione
corretta deve quindi riprodurre la baseline modale in quel limite, e l'harness si
rifiuta di riportare qualsiasi accuratezza prima di averlo verificato.

Misurato: **−0,270 pp** di scarto e **99,73%** di accordo riga per riga. Il
residuo cade interamente su gruppi in cui i primi due `n_c` sono esattamente
pari, cioè dove anche la cascata modale decide per convenzione.

---

## 3. Iperparametri e loro ottimi misurati

| Parametro | Che cosa governa | Ottimo | Nota |
|---|---|---|---|
| `distance` | la regola di similarità | `bernoulli_nb` | i tre naive Bayes staccano le distanze geometriche di ~5 pp |
| `prior_weight` (λ) | peso di `log n_c` | **1,0** | **il fattore dominante**: da λ=0 a λ=1 si guadagnano 5,1 pp |
| `shrink_beta` (β) | attrazione verso il gruppo | **0** | peggiora monotonicamente: a β=1000 si perdono 4 pp |
| `smooth_alpha` (α) | smoothing di Lidstone | **0,1** | ottimo interno, perde su entrambi i lati |
| `min_support` | soglia di ammissione dei candidati | **1** | alzarlo peggiora sempre |
| `sector` | termine naive-Bayes sul settore | **TRUE** | +0,7–0,8 pp, costante su tutte e 16 le combinazioni |

Selezione congelata: `bernoulli_nb`, λ=1, β=0, α=0,1, `min_support`=1,
`sector`=TRUE.

---

## 4. Prestazioni misurate

Blocco sigillato di 3 mesi, 93.796 righe da codificare, 713.196 di
addestramento, mai visto durante la selezione.

| | k-NN | Centroide | Rapporto |
|---|---|---|---|
| Accuratezza CP5, righe decise dal modello (n = 84.034) | 83,922% | **84,134%** | +0,212 pp |
| Accuratezza CP4 per troncatura | — | — | +0,249 pp |
| Copertura (`no_match`) | — | — | identica, +0,0000 pp |
| Costruzione del modello | — | **2,4 s** | — |
| Codifica | 118,7 s | **7,3 s** | **0,062×** |
| Costruzione + codifica | 118,7 s | 9,7 s | 0,082× |
| Picco di heap R | 6.646 MB | 3.529 MB | 0,531× |

Sull'accuratezza: McNemar dà p = 0,0318, ma +0,212 pp sta **sotto** la soglia di
rumore di 0,244 pp che questo progetto adotta. La lettura onesta è quindi
**accuratezza equivalente**, non superiore.

Sul tempo: la differenza è strutturale, non di costante. Il k-NN cresce con
*righe da codificare × pool*; il centroide con le sole righe da codificare,
perché il pool è già stato compresso nella build. In produzione l'ultima
esecuzione reale ha impiegato 1.899 s di k-NN su 894.084 righe; per linearità la
codifica a centroidi starebbe nell'ordine dei **due minuti**. È
un'estrapolazione da un solo punto e su hardware diverso: ordine di grandezza,
non previsione.

---

## 5. L'esito del gate

| Criterio | Misura | Soglia | Esito |
|---|---|---|---|
| P primario, Δ accuratezza CP5 | +0,212 pp | ≥ −2,00 | passa |
| S1 tempo | 0,062× | ≤ 0,20 | passa |
| **S1 memoria** | **0,531×** | **≤ 0,50** | **fallisce** |
| S2 copertura | +0,0000 pp | ≤ +0,10 | passa |
| S3 troncatura CP4 | +0,249 pp | ≥ −2,00 | passa |

**Il gate era registrato prima della corsa, quindi l'esito resta negativo.**

Un dato sulla misura, da tenere separato dall'esito: il picco è misurato sulla
**sessione**, che ospita anche l'incumbent e quindi tiene caricati i 713.196
annunci di addestramento con i loro assegnamenti di competenze. Il centroide in
esercizio non ne avrebbe bisogno — il modello è l'oggetto dei profili. Quel
0,531× misura dunque il centroide dentro un banco di prova costruito per il suo
concorrente, non il centroide in produzione.

---

## 6. Che cosa l'esperimento stabilisce comunque

Indipendentemente dal gate, l'esperimento chiude una questione aperta dal marzo
2026, quando un tentativo a centroidi aveva ottenuto il 47,1% contro l'81,1% del
k-NN e il metodo era stato archiviato.

**Quel risultato misurava una configurazione, non un metodo.** L'harness porta
come controllo negativo la configurazione esatta di allora e ne riproduce il
fallimento: **−19,53 pp** sotto la cascata modale, stesso segno e stesso ordine
di grandezza dei −15,5 pp misurati allora. La stessa famiglia di modelli, con la
configurazione corretta, arriva all'84,1%.

Scomposizione dei fattori, dal 2026-03 alla selezione congelata:

| Fattore | Contributo |
|---|---|
| Prior di dimensione di classe (λ) | **+5,1 pp** |
| Naive Bayes invece del coseno | **+6 pp** circa |
| Termine di settore | +0,8 pp |
| Shrinkage verso il gruppo | **0 o negativo** |

Due parti della diagnosi originale **non** reggono alla misura: si attribuiva il
fallimento all'assenza di smoothing e di supporto minimo, ma l'ottimo di β è
zero e quello di `min_support` è 1. Il fattore dominante era la prior.

L'invarianza del coseno diagnosticata nell'intestazione dell'harness è invece
confermata: la normalizzazione L1 prima di un coseno si cancella, quindi allora
si stava misurando il coseno contro centroidi di conteggi grezzi.

---

## 7. Se lo si riprende

**Decisione preliminare, da prendere e scrivere prima di rieseguire.** Il gate è
fallito su S1-memoria. Ri-specificarlo perché misuri la memoria del **modello
isolato** anziché della sessione è difendibile nel merito, ma è una
ri-specificazione: va motivata e fissata prima della corsa, non dopo aver visto
il risultato. Altrimenti l'esito resta quello che è.

**Difetto già corretto.** Nella corsa del 2026-08-07 il braccio `complement_nb`
costruiva il profilo complementare dai conteggi grezzi senza ricevere lo
shrinkage: la sua intera colonna β usciva costante. Non ha mai vinto, quindi né
la selezione né il blocco sigillato ne risentono, ma **i valori di
`complement_nb` nello stadio B di quella corsa non sono attendibili**. Il
sorgente è corretto; una nuova corsa li rimisurerebbe.

**Assi già chiusi, da non ripetere:**

- shrinkage: ottimo a β=0, peggiora monotonicamente;
- supporto minimo: ottimo a 1;
- distanze geometriche: tutte sotto i naive Bayes di ~5 pp;
- ponderazione Balassa: la peggiore delle dodici misure.

**Chiuso il 2026-08-07: il centroide sul segmento privo di ESCO.** Vedi la
sezione 8.

---

## 8. Il segmento privo di ESCO: asse chiuso

Misurato da `skillviz_workflow/run_cp5_no_esco_matrix.R` il 2026-08-07, sulle
4.496 righe di test del segmento (ultimi 3 mesi), contro l'incumbent one-hot +
xgboost.

L'idea era che il centroide rendesse praticabile ciò che il k-NN globale non era:
per gli annunci senza codice ESCO non esiste un restrittore, quindi si sarebbe
usata **tutta la matrice** — un gruppo unico con tutti i codici candidati. Il
k-NN globale era stato abbandonato perché costava il 92% del tempo di
esecuzione; il centroide fa lo stesso lavoro in 3 secondi.

Motivava il tentativo un fatto misurato: gli annunci senza ESCO **non** sono
poveri di competenze, ne portano nel 94,20% dei casi contro il 94,12% di quelli
classificati.

| Braccio | CP5 | Copertura | Δ | Tempo |
|---|---|---|---|---|
| `xgb_onehot` (incumbent) | **75,67%** | 96,4% | — | 38,6 s |
| centroide, addestrato sul solo segmento | 68,28% | 100% | −7,4 pp | 0,2 s |
| centroide, tutta la matrice + settore | 61,30% | 100% | −14,4 pp | 3,0 s |
| centroide, tutta la matrice | 58,47% | 100% | −17,2 pp | 3,1 s |

**L'esito è negativo e il segno è opposto all'ipotesi.** Allargare
l'addestramento da 38.000 righe del segmento a 765.000 di tutta la finestra —
diciotto volte più dati — costa **9 punti** invece di guadagnarne.

Due meccanismi, entrambi coerenti con i numeri:

- **Le classi in più sono rumore, non copertura.** La matrice intera ha 498
  codici CP5, il segmento ne mostra 270. Le 228 in eccesso non sono codici a cui
  quegli annunci appartengono e che l'incumbent non può proporre: sono massa di
  probabilità dirottata verso classi che su questa popolazione non ricorrono. Il
  termine di settore ne recupera 2,8 punti, il che colloca il problema nella
  composizione della popolazione e non nel segnale delle competenze.
- **Le righe senza ESCO sono senza ESCO per una ragione.** Addestrare
  prevalentemente su annunci che il fornitore *è riuscito* a classificare
  significa apprendere la relazione competenze → codice su una popolazione
  diversa da quella su cui si applica. La copertura di competenze identica
  faceva sperare in uno scarto piccolo: non lo è.

**Perché il centroide vince altrove e perde qui.** Sul segmento con ESCO il
restrittore riduce lo spazio a circa 9 candidati, e le competenze bastano a
separarli. Senza restrittore i candidati sono 270–498 e le sole competenze non
bastano: servono le covariate — città, settore, fonte, contratto, istruzione,
salario — che il centroide non usa e xgboost sì. Non è un modello peggiore, sta
giocando con meno informazione.

La copertura al 100% dei bracci a centroidi contro il 96,4% dell'incumbent non
compensa: xgboost lascia scoperto il 3,6% perché rifiuta di indovinare le classi
rare, ed è una scelta deliberata.

**Non riprovare questo asse.** Se si vuole migliorare la codifica di quel 13% di
annunci, la leva promettente è il **testo dell'annuncio**, che oggi nessun
modello del progetto usa.

---

## Riferimenti

| Cosa | Dove |
|---|---|
| Risultati completi della corsa | `skillviz_workflow/cp5_centroid_results.rds` |
| Selezione congelata | `skillviz_workflow/cp5_centroid_selection.rds` |
| Harness, con gate e diagnosi in intestazione | `skillviz_workflow/run_cp5_centroid.R` |
| Segmento privo di ESCO: harness e risultati | `skillviz_workflow/run_cp5_no_esco_matrix.R`, `cp5_no_esco_matrix_results.rds` |
| API delle due funzioni | `?skillviz::build_cp_profiles`, `?skillviz::predict_cp5_centroid` |
| Il modello in esercizio, con cui è stato confrontato | `?skillviz::predict_cp5_knn` |
