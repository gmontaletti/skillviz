# Limiti delle procedure di codifica del container

Documento di riferimento per chi consuma `staging.gm_cp4_imputed`. Elenca ciò
che le procedure del container **non** garantiscono, con le misure che
quantificano ciascun limite. È scritto per essere letto prima di usare i codici
imputati in un'analisi, non dopo.

Ogni cifra riportata è misurata, non stimata; la fonte è indicata fra parentesi.

---

## 1. Il limite principale: nessuna validazione riguarda la popolazione trattata

**Tutte le accuratezze note sono misurate su annunci già codificati dal
fornitore, e applicate ad annunci che il fornitore non ha codificato.** Non
esiste alcuna misura diretta di quanto il codificatore sbagli sulle righe su cui
lavora davvero, perché per quelle righe non c'è una verità con cui confrontarsi.

Due elementi rendono questa estrapolazione non neutrale, entrambi misurati.

**L'etichettatura non è casuale.** Fra le fonti la quota di annunci codificati va
dal 16,6% (IT_EXPERTEER) al 55,8% (IT_HELPLAVORO); fra i settori dal 31,9% al
72,5%. All'interno di uno stesso gruppo ESCO di livello 5, la fonte più coperta e
quella meno coperta hanno distribuzioni di codice CP5 che distano in media 0,19
in distanza di variazione totale, e il 38% dei gruppi confrontabili supera 0,25
(`run_esco5_coverage_bias.R`).

Ripesando ogni fonte sulla sua quota fra gli annunci **non** codificati del
gruppo — cioè chiedendo che cosa direbbe il crosswalk se il campione codificato
avesse la stessa composizione della popolazione trattata — il codice modale
cambia in 497 gruppi su 2.708, che coprono il **13,7% degli annunci non
codificati**. Il movimento segue la copertura come previsto dal meccanismo: 24,8%
dei gruppi nella fascia 0-10% di copertura, 3,7% nella fascia 75-100%.

**Gli annunci non codificati sono diversi da quelli codificati.** Portano in media
12,7 competenze contro 8,2, un rapporto di 1,55 stabile su tutti i 25 mesi e per
il 78% interno alle singole fonti (`R/crosswalk.R`, blocco `@details` di
`predict_cp4_knn`). Il k-NN è sensibile alla lunghezza del vettore di competenze:
l'accuratezza passa dal 44,3% con 1-4 competenze al 91,9% con 21 o più.

**Conseguenza pratica.** Le accuratezze citate altrove in questa documentazione
(84,1% a livello 5, 84,6% a livello 4) vanno lette come **limiti superiori
ottimistici**. Non esiste al momento una stima dell'errore sulla popolazione
trattata, e la colonna `coverage_labelled` del crosswalk pubblicato
(`?skillviz::build_esco_cp_crosswalk`) è l'unico strumento per individuare i
gruppi in cui l'estrapolazione è più fragile.

---

## 2. Copertura: che cosa resta senza codice, e perché

| Popolazione | Quota | Esito |
|---|---|---|
| Con codice del fornitore | 46,1% | `cp4_imputed = 0`, nessuna imputazione |
| Senza codice, con ESCO | ~46,8% | k-NN, livelli 4 e 5 |
| Senza codice, senza ESCO | ~13,1% delle non codificate | xgboost, livelli 4 e 5, meno accurato |
| Senza codice, classe rara | variabile | **nessun codice** |
| Senza codice, gruppo ESCO mai visto etichettato | ~0,3% delle non codificate | **nessun codice** (`no_match`) |

Tre osservazioni.

**Il 13,1% senza ESCO non è raggiungibile da alcun metodo basato su ESCO.** Quegli
annunci portano `idesco_level_5 = 'Unclassifiable'`, cioè nessun codice
occupazionale a nessun livello. Il restrittore non li tocca: passare da ESCO4 a
ESCO5 ha cambiato la copertura di 0,04 punti percentuali. Sono serviti da
xgboost, che li codifica al 76,4% a livello 5 contro l'84,1% del k-NN sul
segmento con ESCO.

**Le classi rare restano senza codice per scelta.** I codici CP5 con meno di
`IMPUTE_XGB_RARE_MIN` (30) esempi etichettati vengono accorpati in un secchio
`other`, e le righe che ci finiscono sono lasciate vuote invece di essere
indovinate in una classe comune. È deliberato — un NULL onesto vale più di un
codice sbagliato con l'aria di essere giusto — ma significa che **la copertura
non è uniforme fra le professioni**: le occupazioni rare sono sistematicamente
sotto-rappresentate fra le righe imputate.

**Un gruppo ESCO mai osservato insieme a un codice CP non è codificabile.** Sono
3.195 righe (0,34% delle non codificate), e 325 codici ESCO di livello 5 su 3.039
non compaiono mai con un codice CP nei dati. Il crosswalk pubblicato li marca con
`status = "no_target"` o `"absent"`.

---

## 3. Il crosswalk non è una tabella di corrispondenza

**Non usare `cp5_modal` come lookup.** Il codice modale di un gruppo ESCO di
livello 5 è corretto nel **73,4%** dei casi, ponderando per riga. La
concentrazione sale lentamente: 85,8% ai primi due codici, 90,3% ai primi tre,
94,3% ai primi cinque. Solo lo 0,46% delle righe sta in un gruppo il cui codice
modale copre l'intero gruppo.

Alcuni fra i gruppi più numerosi sono genuinamente divisi:

| Gruppo ESCO5 | Descrizione | Righe | Candidati | Quota modale |
|---|---|---|---|---|
| 8322.6 | autista privato | 13.607 | 30 | 49,3% |
| 7412.3 | manutentore elettromeccanico | 12.141 | 15 | 53,4% |
| 3115.1 | tecnico meccanico | 10.506 | 28 | 58,3% |

Il crosswalk è lo **spazio dei candidati** entro cui vota il k-NN, ed è il voto a
portare il 73% all'84%. La colonna `modal_reliable`
(`modal_share >= 0.90 & n_labelled >= 30`) marca la porzione utilizzabile come
corrispondenza diretta: circa un terzo delle righe, non la totalità.

Da leggere `n_eff` (candidati efficaci, inverso di Simpson) e non
`n_candidates`: sull'intero archivio la media ponderata è 2,28 contro 32,13
nominali, perché la coda di coppie viste una o due volte cresce con l'evidenza
senza portare massa.

---

## 4. Riproducibilità: che cosa è garantito e che cosa no

**Garantito.** A parità di dati in ingresso e di configurazione, due esecuzioni
danno lo stesso risultato. Il campionamento che limita i gruppi grandi usa un
passo deterministico e non il generatore casuale; `IMPUTE_SEED` fissa xgboost;
lo snapshot CRAN nel Dockerfile fissa le versioni dei pacchetti.

**Non garantito.**

*Nessun artefatto di modello.* Entrambi i modelli sono rifittati da zero a ogni
esecuzione completa. Non c'è modo di riprodurre una previsione fatta un mese fa,
né di sapere con quale modello una riga specifica sia stata codificata. La
tabella non porta né una versione di modello né una data di addestramento.

*La modalità incrementale mescola modelli.* Le esecuzioni incrementali allenano
su tutta la finestra ma predicono solo i mesi nuovi. Righe scritte in mesi
diversi possono quindi provenire da modelli addestrati su insiemi diversi, e la
tabella non lo registra.

*`decide_mode()` guarda solo i mesi.* Non può accorgersi che il modello o il
routing sono cambiati, solo che i dati lo sono. Dopo una modifica al modello
serve `IMPUTE_FORCE_FULL=1`, altrimenti convivono righe vecchie e nuove.

*Soglia di rumore.* Il criterio di spareggio fra similarità uguali produce
oscillazioni fino a **0,244 punti percentuali** fra esecuzioni su input
identici ma ordinati diversamente. Differenze inferiori a questa soglia non sono
interpretabili. Il livello 4 conserva inoltre un criterio di spareggio *legacy*
che dipende dall'ordine delle righe in ingresso: è pinnato per non spostare le
previsioni storiche, non perché sia corretto.

---

## 5. La confidenza è calcolata ma non scritta

`predict_cp5_knn()` restituisce una `confidence` ben calibrata, e xgboost una
probabilità di classe. **Nessuna delle due finisce nella tabella.** I consumatori
non possono quindi distinguere una riga decisa con voto unanime da una decisa di
misura, né filtrare le più incerte.

La calibrazione misurata sul segmento con ESCO, per decili di confidenza:

| Decile | Confidenza media | Accuratezza |
|---|---|---|
| 1 | 48,4% | 37,6% |
| 2 | 68,0% | 61,5% |
| 3 | 83,7% | 77,2% |
| 4 | 95,6% | 90,6% |
| 5-10 | 100,0% | 95,5-95,8% |

L'errore di calibrazione atteso è di 5,5 punti: il modello è **leggermente
sovraconfidente**, e nel 60% dei casi restituisce confidenza esattamente pari a 1
con un'accuratezza reale del 95,6%.

Due avvertenze. La scala è cambiata con il passaggio al livello 5 — gli stessi
`k` vicini si distribuiscono su più classi — quindi **una soglia tarata sul
vecchio modello di livello 4 non è trasferibile**. E il percorso xgboost non ha
alcuna calibrazione misurata.

---

## 6. Segmenti su cui le procedure sono più deboli

**Gruppi con pochi vicini.** Il 3,2% delle righe sta in gruppi ESCO di livello 5
con meno di 50 esempi etichettati. Il restrittore fine vince in tutte le fasce di
ampiezza sopra le 10 righe, ma nella fascia 1-10 perde contro il livello 4
(66,0% contro 73,1% in misura contemporanea). Sono poche righe, ma sono
sistematicamente le professioni meno frequenti.

**Annunci lunghi.** L'accuratezza cala con il numero di competenze non per un
difetto del kernel ma perché gli annunci lunghi hanno pochi simili: similarità
del primo vicino 0,45 con 21+ competenze contro 0,76 con 1-4. Sei kernel
alternativi sono stati provati e nessuno migliora la situazione.

**Annunci senza competenze.** Il k-NN non può trattarli e ripiega sul codice
modale del gruppo. Il percorso xgboost è l'unico che li classifica usando i
soli covariati (81,5% contro 69,7%).

**Copertura in calo nel tempo.** La quota mensile di annunci codificati dal
fornitore scende dal 47,5-50,2% fino a maggio 2025 al 41,7-43,3% nel 2026. Il
materiale di addestramento si assottiglia proprio dove il codificatore è più
richiesto, cioè sui dati recenti.

---

## 7. Parametri non riottimizzati dopo l'ultima modifica

`IMPUTE_XGB_ROUNDS=400` è ereditato dalla configurazione precedente, quando il
percorso senza ESCO usava vtreat e predisponeva a livello 3. Dopo il passaggio a
codifica one-hot e a livello 5 non è stato risweepato. L'esperimento di confronto
ha usato 150 iterazioni, quindi il valore in produzione è più conservativo di
quello misurato, non meno.

Analogamente, la misura del percorso xgboost proviene da **una sola ripartizione
temporale** di 4.496 righe di test. A un'accuratezza dell'80% l'errore standard è
di circa 0,6 punti, quindi i differenziali di accuratezza fra codifiche valgono
1-2 errori standard: indicativi, non conclusivi. È il differenziale di tempo
(fattore 84) ad aver deciso quella scelta, non quello di accuratezza.

---

## 8. Disallineamenti fra fonti dati

`staging.dim_cp2021_5` su Postgres contiene 813 codici CP5 ben formati.
`dim_cp2021` nell'archivio DuckDB usato da `skillviz_workflow` ne contiene 806
validi su 809, con **tre righe corrotte** in cui testo libero è finito nella
colonna identificativo. Il container e la pipeline validano quindi contro
universi leggermente diversi, e la differenza simmetrica non è mai stata
riconciliata. Il container filtra sulla forma `d.d.d.d.d` e interrompe
l'esecuzione se un codice non la rispetta, quindi il rischio è contenuto ma non
azzerato.

---

## 9. Che cosa il container non fa affatto

- **Non misura la propria qualità.** Verifica solo la coerenza interna: conteggio
  righe, assenza di duplicati, gerarchia fra livelli, nessun codice del fornitore
  sovrascritto. La qualità è misurata unicamente offline, in
  `skillviz_workflow/run_*.R`, contro dati già etichettati.
- **Non usa il testo dell'annuncio.** Né titolo né descrizione entrano in alcun
  modello. È la ragione per cui il 13,1% privo di ESCO resta il limite duro della
  copertura, e la via naturale per superarlo.
- **Non registra la provenienza per riga.** I flag dicono se un codice è imputato,
  non da quale modello né con quale metodo della cascata (`knn`,
  `single_candidate`, `frequency`).
- **Non riconcilia con revisioni successive del fornitore.** Se il fornitore
  codifica in seguito un annuncio prima privo di codice, l'esecuzione completa
  successiva sostituisce l'imputazione con il codice reale, ma nulla segnala che
  la stima precedente fosse sbagliata né di quanto.

---

## Riferimenti

| Misura | Fonte |
|---|---|
| Accuratezza k-NN, walk-forward | `skillviz_workflow/cp5_restrictor_temporal_results.rds` |
| Accuratezza k-NN, contemporanea | `skillviz_workflow/cp5_knn_results.rds` |
| Percorso xgboost, confronto codifiche | `skillviz_workflow/cp5_xgboost_no_esco_results.rds` |
| Distorsione di etichettatura | `skillviz_workflow/esco5_coverage_bias_results.rds` |
| Struttura del crosswalk | `?skillviz::build_esco_cp_crosswalk` |
| Esperimenti chiusi, da non ripetere | blocco `@details` di `?skillviz::predict_cp4_knn` e `?skillviz::predict_cp5_knn` |
