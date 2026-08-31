# Il classificatore a centroidi, spiegato a chi lavora su database

Guida al metodo implementato da `build_cp_profiles()` e
`predict_cp5_centroid()` in `R/centroid.R`.

Il verbale tecnico dell'esperimento sta in `centroide.md`, in questa stessa
cartella: riporta il gate pre-registrato, la griglia completa degli
iperparametri e le diagnosi. Questa guida non lo ripete. Spiega invece che cosa
fa il metodo e perché è costruito così, per un lettore che ragiona per tabelle,
indici e piani di esecuzione e non per notazione algebrica.

**Stato.** Il metodo non è adottato. Né la pipeline né il container lo
chiamano: la codifica in produzione resta quella di `predict_cp5_knn()`. Il gate
pre-registrato è fallito su un criterio, la memoria. Il paragrafo 8 riporta
l'esito per intero.

Due convenzioni usate ovunque nel testo. **pp** significa *punti percentuali*,
cioè la differenza fra due percentuali: passare da 83,9% a 84,1% è +0,2 pp, non
+0,2%. Il **restrittore** è la colonna che limita i codici ammissibili per un
annuncio — nel nostro caso il codice ESCO a cinque cifre. In SQL è una clausola
`WHERE`.

## 1. Il problema

Ogni annuncio di lavoro raccolto dal fornitore porta con sé un elenco di
competenze ESCO e, quando il fornitore è riuscito a codificarlo, un codice
CP2021 a cinque livelli. Circa metà degli annunci quel codice non ce l'ha, e va
attribuito.

Il modello in esercizio è un k-NN — *k nearest neighbours*, i k vicini più
somiglianti. Per ogni annuncio da codificare confronta l'insieme delle sue
competenze con quello di ogni annuncio già etichettato dello stesso gruppo ESCO,
tiene i sette più somiglianti e fa votare i loro codici.

Chi legge piani di esecuzione riconosce la forma: è una scansione dell'archivio
per ogni riga da codificare. Non c'è un indice che eviti il confronto, perché la
somiglianza fra due insiemi di competenze non è un predicato di uguaglianza —
non si può cercare su un B-tree. Il costo cresce quindi con il **prodotto** fra
righe da codificare e ampiezza dell'archivio. Nella corsa di produzione la
ricerca dei vicini assorbe il 98,6% del tempo totale.

Ha inoltre una conseguenza operativa: l'archivio etichettato deve essere
presente in memoria ogni volta che si codifica. Non esiste un "modello" da
salvare: esiste solo la procedura più i dati.

## 2. L'idea del centroide

L'alternativa è precalcolare un riassunto e buttare via l'archivio.

Per ogni classe — cioè per ogni coppia (gruppo ESCO, codice CP5) osservata nei
dati etichettati — si conta quante volte ciascuna competenza compare negli
annunci di quella classe. Quel vettore di conteggi è il **centroide** della
classe: il suo profilo di competenze tipico. Codificare un annuncio nuovo
diventa allora un confronto con una decina di profili, non con decine di
migliaia di annunci.

In termini di database è la differenza fra interrogare la tabella dei fatti e
interrogare una vista materializzata che la riassume:

```sql
-- quello che il k-NN fa, concettualmente: confronta con ogni riga etichettata
SELECT ...
FROM annuncio_da_codificare q
JOIN archivio_etichettato t ON t.gruppo_esco = q.gruppo_esco
...

-- quello che il centroide fa: la riga etichettata non compare più
CREATE MATERIALIZED VIEW profili AS
SELECT gruppo_esco, cp5, competenza, count(DISTINCT general_id) AS n
FROM archivio_etichettato JOIN competenze USING (general_id)
GROUP BY 1, 2, 3;
```

La vista si costruisce una volta, si salva, si versiona e si spedisce. È
l'oggetto restituito da `build_cp_profiles()`.

## 3. La matrice: che cosa contiene davvero

L'oggetto centrale è una matrice sparsa. Vale la pena essere precisi su che cosa
sta sulle righe, che cosa sulle colonne e che cosa nelle celle, perché tutte e
tre le risposte sono meno ovvie di quanto sembri.

- **Le righe sono le coppie (gruppo ESCO, codice CP5)**, non i codici CP5. Lo
  stesso codice CP5 osservato in due gruppi ESCO diversi occupa due righe
  diverse, con due profili diversi. In produzione le coppie sono circa 24.000,
  a fronte di 2.714 gruppi.
- **Le colonne sono le competenze**, 1.685 in tutto.
- **Le celle sono conteggi grezzi di annunci**: quanti annunci distinti di
  quella coppia citano quella competenza. Non frequenze, non punteggi
  normalizzati, non tf-idf.

I conteggi restano grezzi per scelta. Lo smoothing e la contrazione verso il
profilo di gruppo si applicano al momento della previsione, quando il blocco di
un gruppo viene materializzato: così una sola costruzione serve molte
configurazioni di punteggio e l'oggetto salvato resta sparso.

**Matrice sparsa**, in termini di storage, vuol dire che si memorizzano solo le
celle diverse da zero, con le loro coordinate — l'equivalente di una tabella
`(riga, colonna, valore)` invece di una griglia completa. Il formato usato,
`dgCMatrix`, è compresso per colonna: molto simile, come idea, a un indice
invertito che per ogni termine tiene la lista dei documenti che lo contengono.

Un dettaglio che sorprende chi arriva dal mondo relazionale: **la matrice non ha
nomi di riga né di colonna**. L'identità di una riga è la sua posizione, un
intero `pair_id`. La corrispondenza fra posizione ed etichetta sta in una
tabella a parte, `pairs`, ordinata per gruppo e poi per codice. Cioè: `pairs` è
la tabella dimensionale, la matrice è la tabella dei fatti, e la chiave esterna
è l'indice di riga. Non esiste nessuna stringa concatenata tipo
`"7212.3|6.2.1.2.0"`.

### Una porzione reale

Prendiamo il gruppo ESCO `7212.3`, "saldatore/saldatrice". Nei dodici mesi da
luglio 2025 a giugno 2026 gli annunci etichettati di quel gruppo si distribuiscono
su nove codici CP2021 distinti:

| `cp5` | denominazione CP2021 | `n_c` |
|---|---|---|
| `6.2.1.7.0` | Saldatori elettrici e a norme ASME | 627 |
| `6.2.1.2.0` | Saldatori e tagliatori a fiamma | 475 |
| `6.2.1.4.0` | Carpentieri e montatori di carpenteria metallica | 383 |
| `3.1.3.7.1` | — | 5 |
| `8.4.3.1.0` | — | 2 |
| `6.1.3.6.1` | — | 2 |
| `8.1.3.2.0` | — | 2 |
| `5.2.2.3.2` | — | 1 |
| `6.2.1.3.1` | — | 1 |

Sono nove righe della matrice: una per coppia. Tre classi vere e una coda di sei
classi da uno a cinque annunci — una forma che si ripete in quasi tutti i
gruppi, e su cui torneremo.

Ecco la porzione della matrice per le tre classi principali e le sei competenze
più frequenti del gruppo. I valori sono conteggi reali, estratti in sola lettura
il 31 agosto 2026 sulla finestra 202507–202606.

**Forma lunga**, come la restituirebbe una `GROUP BY`. La colonna `riga` è la
posizione della coppia dentro il blocco del gruppo: `pairs` è ordinata per
gruppo e poi per codice, quindi dei nove candidati elencati sopra questi tre
occupano le posizioni 4, 6 e 7. Non è un dettaglio ozioso — è quell'ordinamento
a stabilire che, a parità di punteggio, vince il codice CP5 più piccolo.

| `riga` | gruppo | `cp5` | competenza | `n` |
|---|---|---|---|---|
| 4 | `7212.3` | `6.2.1.2.0` | assemblare parti mediante brasatura, saldatura o saldobrasatura | 424 |
| 4 | `7212.3` | `6.2.1.2.0` | professioni inerenti alla metallurgia e alla meccanica | 421 |
| 4 | `7212.3` | `6.2.1.2.0` | collaborare in gruppi e reti | 196 |
| 4 | `7212.3` | `6.2.1.2.0` | materiali (vetro, carta, plastica e legno) | 187 |
| 6 | `7212.3` | `6.2.1.4.0` | assemblare parti mediante brasatura, saldatura o saldobrasatura | 324 |
| 6 | `7212.3` | `6.2.1.4.0` | professioni inerenti alla metallurgia e alla meccanica | 319 |
| 7 | `7212.3` | `6.2.1.7.0` | assemblare parti mediante brasatura, saldatura o saldobrasatura | 618 |
| 7 | `7212.3` | `6.2.1.7.0` | professioni inerenti alla metallurgia e alla meccanica | 617 |

**Forma a griglia**, la stessa porzione come blocco 6 × 3 della matrice. Le
colonne sono identificate dal codice competenza, perché due codici distinti
possono portare la stessa etichetta — `ESCOv1_1781` e `ESCOv1_3187` qui lo
fanno, ed è la ragione per cui la colonna della matrice è l'identificativo e non
il nome:

| competenza | `6.2.1.2.0` | `6.2.1.4.0` | `6.2.1.7.0` |
|---|---|---|---|
| `ESCOv1_1781` assemblare parti mediante brasatura… | 424 | 324 | 618 |
| `ESCOv1_7009` professioni inerenti alla metallurgia… | 421 | 319 | 617 |
| `ESCOv1_1678` collaborare in gruppi e reti | 196 | 147 | 303 |
| `ESCOv1_4392` materiali (vetro, carta, plastica e legno) | 187 | 155 | 279 |
| `ESCOv1_3187` assemblare parti mediante brasatura… | 105 | 115 | 220 |
| `ESCOv1_7000` posizionare materiali, strumenti o attrezzature | 105 | 115 | 219 |

In R la stessa porzione si scrive per coordinate, senza mai materializzare gli
zeri:

```r
Matrix::sparseMatrix(
  i = c(1, 1, 1, 1, 1, 1,  2, 2, 2, 2, 2, 2,  3, 3, 3, 3, 3, 3),
  j = c(1, 2, 3, 4, 5, 6,  1, 2, 3, 4, 5, 6,  1, 2, 3, 4, 5, 6),
  x = c(424, 421, 196, 187, 105, 105,
        324, 319, 147, 155, 115, 115,
        618, 617, 303, 279, 220, 219),
  dims = c(3, 6)
)
```

**Quanto è vuota.** Il blocco completo di questo gruppo ha 9 righe e vede 93
competenze distinte, per 239 celle piene: il 29% del rettangolo 9 × 93, ma solo
l'**1,6%** del rettangolo 9 × 1.685 che occuperebbe una matrice densa sull'intero
vocabolario. È questa sproporzione a rendere praticabile l'intera struttura.

**Che cosa insegna l'esempio.** I profili delle tre classi sono quasi paralleli.
Rapportando i conteggi alla dimensione di classe, la competenza `ESCOv1_1781`
copre l'89% degli annunci di `6.2.1.2.0`, l'85% di `6.2.1.4.0` e il 99% di
`6.2.1.7.0`. Un saldatore a fiamma, un saldatore elettrico e un carpentiere
metallico elencano in larga misura le stesse competenze. Le competenze da sole,
quindi, separano poco queste tre classi: il grosso del lavoro lo fa qualcos'altro,
ed è il paragrafo che segue.

## 4. Come si assegna il codice

Il punteggio di ogni classe candidata è la somma di tre addendi:

```
punteggio(annuncio, classe) =
      prior_weight * log(dimensione della classe)     -- quanto è comune
    + somiglianza(competenze annuncio, profilo classe) -- quanto si assomigliano
    + log P(settore dell'annuncio | classe)            -- il settore, se disponibile
```

Vince il punteggio più alto. A parità di punteggio vince il codice CP5 più
piccolo, perché le righe sono ordinate per codice e l'argmax prende il primo
massimo.

In SQL la forma è quella familiare di un join seguito da un'aggregazione e da un
ordinamento:

```sql
SELECT p.cp5,
       :lambda * ln(p.n_c) + sum(p.peso) + coalesce(s.log_p_settore, 0) AS punteggio
FROM profili p
JOIN competenze_annuncio a ON a.competenza = p.competenza
LEFT JOIN profili_settore s
       ON s.cp5 = p.cp5 AND s.settore = :settore_annuncio
WHERE p.gruppo_esco = :gruppo_annuncio      -- il restrittore
GROUP BY p.cp5, p.n_c, s.log_p_settore
ORDER BY punteggio DESC
LIMIT 1;
```

La clausola `WHERE` è il punto decisivo. Restringe i candidati ai soli codici che
in addestramento sono stati osservati insieme a quel gruppo ESCO: in media 8,4
candidati, contro gli 813 codici CP5 che la classificazione contiene. Senza
quella riga il metodo non regge, come mostra il paragrafo 9.

Il primo addendo, il **prior**, è la dimensione della classe passata al
logaritmo. `prior_weight` (λ nel verbale) ne regola il peso. È il fattore
dominante: portarlo da 0 a 1 vale **+5,1 pp** di accuratezza, più di qualsiasi
altra scelta. Nell'esempio dei saldatori si vede perché — quando i profili sono
quasi paralleli, a decidere resta quale classe è più comune.

Il secondo addendo, la **somiglianza**, si può calcolare con dodici regole
diverse, selezionabili con `distance`: tre varianti naive Bayes
(`multinomial_nb`, `complement_nb`, `bernoulli_nb`) e nove misure geometriche
(`cosine`, `hellinger`, `l2`, `l1`, `chisq`, `jsd`, `dice`, più due coseni
pesati). *Naive Bayes*, in una frase, tratta le competenze come indizi
indipendenti e somma il logaritmo della probabilità di osservare ciascuna in
quella classe; "naive" perché l'indipendenza è falsa e si assume lo stesso. Le
tre varianti bayesiane staccano quelle geometriche di circa 5 pp, e la migliore
misurata è `bernoulli_nb`.

Il terzo addendo, il **settore**, vale +0,7–0,8 pp ed è risultato positivo in
tutte e sedici le combinazioni provate.

La funzione restituisce anche una colonna `method`, che dice quale ramo ha
deciso — l'equivalente di leggere il piano invece del solo risultato:

| `method` | quando | come decide |
|---|---|---|
| `centroid` | caso normale | punteggio completo sui candidati |
| `single_candidate` | il gruppo ha un solo codice possibile | assegna quello, senza modello |
| `frequency` | l'annuncio non porta competenze note | solo il prior, cioè la classe più comune |
| `no_match` | il gruppo ESCO non compare nel modello | nessun codice |

## 5. Perché la regola ha questa forma

I tre addendi non sono una combinazione arbitraria. Sono scelti perché il
modello sia **falsificabile**, cioè perché esista una configurazione in cui deve
dare un risultato noto in anticipo.

Il parametro `shrink_beta` (β) contrae il profilo di ogni classe verso il profilo
medio del suo gruppo ESCO: a β basso ogni classe conserva il suo profilo, a β
alto tutte le classi del gruppo finiscono per condividerne uno solo. Quando i
profili coincidono, l'addendo di somiglianza diventa identico per tutti i
candidati e sparisce dal confronto. Resta il prior. Ordinare per
`log(dimensione della classe)` significa scegliere il codice più frequente del
gruppo: è esattamente la **cascata modale**, la regola banale che si userebbe
senza alcun modello.

L'harness verifica questa identità prima di leggere qualsiasi accuratezza: con
β molto grande il centroide riproduce la cascata modale con il 99,73% di accordo
e uno scarto di −0,270 pp. Il test corrispondente è in
`tests/testthat/test-centroid.R`.

Se ne ricava l'ancoraggio mentale giusto: **il centroide è la cascata modale più
l'evidenza delle competenze**, e β regola quanta evidenza lasciar passare.
L'ottimo misurato è β = 0, cioè nessuna contrazione; salendo peggiora sempre, e
a β = 1000 si perdono 4 pp.

Attenzione a un dettaglio pratico: il valore predefinito della funzione è
`shrink_beta = 50`, che **non** è l'ottimo misurato. Chi riprendesse queste
funzioni deve impostare esplicitamente β = 0. La selezione congelata per intero
è `bernoulli_nb`, λ = 1, β = 0, `smooth_alpha` = 0,1, `min_support` = 1,
`sector` = TRUE.

## 6. Il guadagno di tempo

Le misure vengono da un blocco di test sigillato di tre mesi (aprile–giugno
2026): 93.796 righe da codificare, contro un archivio etichettato di 713.196
righe, su un vocabolario di 1.685 competenze. La popolazione su cui i due
modelli sono confrontabili — le righe che entrambi decidono — è di 84.034 righe.

| | k-NN | Centroide | Rapporto |
|---|---|---|---|
| Accuratezza CP5 | 83,922% | 84,134% | +0,212 pp |
| Accuratezza CP4 per troncatura | — | — | +0,249 pp |
| Copertura (`no_match`) | — | — | identica, +0,0000 pp |
| Costruzione del modello | — | 2,4 s | — |
| **Codifica** | **118,7 s** | **7,3 s** | **0,062×** |
| Costruzione + codifica | 118,7 s | 9,7 s | 0,082× |
| Picco di heap R | 6.646 MB | 3.529 MB | 0,531× |

La codifica costa **un sedicesimo**. Il motivo è strutturale, non
implementativo:

- il k-NN paga *(righe da codificare) × (ampiezza dell'archivio)*, e per ogni
  gruppo materializza un blocco denso annunci × archivio. Quel blocco è così
  grande da richiedere due guardie separate: un sottocampionamento
  dell'archivio (`max_train`) e una suddivisione in lotti (`dense_budget`).
  Con il valore predefinito il blocco costa circa 4,8 GB, abbastanza da far
  terminare per esaurimento memoria un container da 8 GB;
- il centroide paga *(righe da codificare) × (una decina di candidati)*. Il
  blocco denso di un gruppo è minuscolo — dieci righe per 1.685 colonne — e non
  cresce con la dimensione del gruppo. La memoria resta piatta e serve una sola
  guardia.

Sull'accuratezza serve una precisazione, ed è importante non leggerla male. Il
+0,212 pp sta **sotto** la soglia di rumore adottata dal progetto, 0,244 pp. Quella
soglia non è un margine di comodo: misura di quanto si muove l'accuratezza
assoluta per cause che nulla hanno a che vedere col modello — due esecuzioni
sugli stessi dati sono arrivate a differire di 0,19 pp per il solo ordine con cui
si rompono i pareggi. La lettura corretta è quindi **accuratezza equivalente**,
non superiore. Il guadagno del centroide è nel costo, non nella qualità.

Per l'ordine di grandezza in produzione: la corsa reale del k-NN impiega 541 s e
5,64 GB di picco. Una singola misura su 894.084 righe ha richiesto 1.899 s, e
per proporzionalità la codifica a centroidi starebbe nell'ordine dei **due
minuti**. È un'estrapolazione da un solo punto e su hardware diverso: vale come
ordine di grandezza, non come previsione.

## 7. Pregi e difetti

| Pregi | Difetti |
|---|---|
| Il modello è un oggetto: si salva, si versiona, si ispeziona, si spedisce | Il gate pre-registrato è **fallito** sulla memoria: 0,531× contro una soglia di 0,50× |
| La codifica non richiede che i dati di addestramento siano presenti | Il vantaggio di accuratezza sta sotto la soglia di rumore: equivalenza, non superiorità |
| Codifica sedici volte più rapida (0,062× del tempo del k-NN) | Inutilizzabile sul segmento privo di codice ESCO: −7,4 pp contro il modello in esercizio |
| Memoria piatta rispetto alla dimensione del gruppo | Dipende interamente dal restrittore: senza `WHERE` i candidati diventano centinaia |
| Una sola guardia di memoria invece di due | Il valore predefinito `shrink_beta = 50` non coincide con l'ottimo misurato (β = 0) |
| Accuratezza equivalente e copertura identica sulle righe che entrambi decidono | Il modello va ricostruito quando i dati etichettati cambiano |
| Il comportamento degenere è noto e verificato da un test | Nessuno dei due percorsi è esercitato in produzione: non ha il collaudo del k-NN |

Il pregio meno appariscente è il primo. Il k-NN non produce nessun artefatto: la
sua tabella di corrispondenza viene ricalcolata in memoria a ogni esecuzione e
scompare. Il centroide produce un file, che si può confrontare con quello del
mese prima per vedere che cosa è cambiato nei dati. È una differenza di natura
gestionale prima che statistica.

Il difetto meno appariscente è l'ultimo. Il k-NN ha alle spalle esecuzioni
mensili ripetute, un container, e i guasti già trovati e corretti. Il centroide
ha una campagna di misura. Non sono la stessa cosa.

## 8. Lo stato: il gate pre-registrato

I criteri erano fissati per iscritto prima della prima esecuzione, con la regola
esplicita che un criterio fallito resta fallito anche se dopo si dimostrasse
difettoso.

| Criterio | Misura | Soglia | Esito |
|---|---|---|---|
| P — Δ accuratezza CP5 | +0,212 pp | ≥ −2,00 pp | passa |
| S1 — tempo | 0,062× | ≤ 0,20 | passa |
| **S1 — memoria** | **0,531×** | **≤ 0,50** | **fallisce** |
| S2 — copertura | +0,0000 pp | ≤ +0,10 pp | passa |
| S3 — troncatura a CP4 | +0,249 pp | ≥ −2,00 pp | passa |

S1 è una congiunzione: tempo **e** memoria devono passare entrambi. L'esito
complessivo è quindi negativo, per uno scarto di 0,031 sul solo rapporto di
memoria.

Il verbale registra un dato sulla misura, che va tenuto separato dall'esito: il
picco è stato rilevato sulla sessione R, che ospitava anche il modello in
esercizio e teneva quindi caricati i 713.196 annunci di addestramento. Il
centroide in produzione non ne avrebbe bisogno, perché il modello è l'oggetto dei
profili. Quel 0,531× misura dunque il centroide dentro un banco di prova
costruito per il suo concorrente. È un fatto registrato, non un argomento per
riaprire il gate.

## 9. Il segmento privo di codice ESCO

Circa il 13% degli annunci non porta alcun codice ESCO. Su quelle righe il
restrittore non esiste, e il metodo si trova a scegliere fra tutti i codici
osservati invece che fra otto o nove. È un asse già chiuso con esito negativo,
su 4.496 righe di test:

| Braccio | CP5 | Copertura | Tempo |
|---|---|---|---|
| `xgb_onehot`, in esercizio | **75,67%** | 96,4% | 38,6 s |
| centroide addestrato sul solo segmento | 68,28% | 100% | 0,2 s |
| centroide su tutta la matrice, con settore | 61,30% | 100% | 3,0 s |
| centroide su tutta la matrice | 58,47% | 100% | 3,1 s |

Senza restrittore i candidati sono fra 270 e 498, e le sole competenze non
bastano a separarli: si perdono da 7,4 a 17,2 pp. Il divario è di due ordini di
grandezza sopra la soglia di rumore, quindi la conclusione non dipende da come si
misura. Il confronto fra le due righe centrali dice anche che l'informazione
utile sta nella restrizione dei candidati, non nella quantità di dati di
addestramento: allargare la matrice peggiora invece di migliorare.

## 10. Riferimenti

| Cosa | Dove |
|---|---|
| Verbale tecnico: gate, griglie, diagnosi | `reference/skillviz/centroide.md` |
| Implementazione | `R/centroid.R` — `build_cp_profiles()` alla riga 117, `predict_cp5_centroid()` alla riga 478 |
| Test, compresa l'identità con la cascata modale | `tests/testthat/test-centroid.R` |
| Harness di misura e risultati completi | `skillviz_workflow/run_cp5_centroid.R`, `cp5_centroid_results.rds` |
| Segmento privo di ESCO | `skillviz_workflow/run_cp5_no_esco_matrix.R` |
| Il modello in esercizio | `?skillviz::predict_cp5_knn` |
| API delle due funzioni | `?skillviz::build_cp_profiles`, `?skillviz::predict_cp5_centroid` |
