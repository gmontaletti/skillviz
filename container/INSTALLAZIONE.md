# Installazione del container di codifica CP2021

Scheda operativa per mettere in esercizio il job di imputazione. Per che cosa il
job produce e con quali limiti, vedere [README.md](README.md) e
[LIMITI.md](LIMITI.md).

> **L'immagine non è pubblicata su alcun registry.** Non esiste su Docker Hub né
> su GitHub Container Registry: va costruita dai sorgenti come descritto al
> punto 2. Non c'è quindi un `docker pull` da eseguire.

---

## 1. Prerequisiti

| Requisito | Valore |
|---|---|
| Docker | qualsiasi versione recente |
| Memoria disponibile al container | **almeno 8 GB** |
| Spazio su disco | ~3 GB per l'immagine |
| Accesso di rete | al Postgres OJA, porta 5432, TLS |
| Credenziali Postgres | utente con lettura su `public` e scrittura su `staging` |

Sulla memoria: l'esecuzione sulla finestra di 24 mesi ha un picco misurato di
**5,64 GB**. Un container da 7,75 GB era già stato terminato dal kernel (uscita
137) quando `IMPUTE_DENSE_BUDGET` era lasciato al valore predefinito del
pacchetto; il file d'ambiente lo imposta a 2.5e7 proprio per questo. Non
abbassare quel margine senza rimisurare.

---

## 2. Costruzione dell'immagine

L'immagine si costruisce **dalla radice del pacchetto**, non da questa
directory: il contesto di build deve includere i sorgenti R.

```sh
git clone https://github.com/gmontaletti/skillviz.git
cd skillviz
docker build -f container/Dockerfile -t skillviz-impute:latest .
```

La build compila `src/global_knn.cpp` e installa il pacchetto, quindi richiede
qualche minuto. Lo snapshot CRAN è fissato al 2026-01-15 nel Dockerfile, così
ricostruzioni successive risolvono le stesse versioni dei pacchetti.

Il `.dockerignore` della radice esclude `container/.Renviron` e ogni altro file
di credenziali: **nessun segreto finisce nell'immagine.**

Verifica che la build sia andata a buon fine:

```sh
docker run --rm skillviz-impute:latest -e 'packageVersion("skillviz")'
```

---

## 3. Credenziali

Copiare il modello in un file leggibile solo dall'utente che esegue il job:

```sh
sudo mkdir -p /etc/skillviz
sudo install -m 0600 -o monty -g monty \
  container/postgres.env.example /etc/skillviz/postgres.env
sudoedit /etc/skillviz/postgres.env
```

Compilare almeno `PG_USER` e `PG_PASSWORD`; gli altri valori sono già impostati
sull'ambiente di produzione. Il file **non va mai** inserito nell'immagine né
versionato: viene passato a runtime con `--env-file`.

---

## 4. Directory di lavoro

Il job usa una directory persistente per il file di lock e per la cache del
risultato, che serve a non buttare via ore di calcolo se la scrittura fallisce:

```sh
sudo mkdir -p /var/skillviz/run /var/log/skillviz
sudo chown monty:monty /var/skillviz/run /var/log/skillviz
```

---

## 5. Prima esecuzione: prova a vuoto

Prima di scrivere qualcosa, eseguire una prova che legge, decide la modalità,
imputa e verifica la coerenza **senza scrivere nulla**:

```sh
docker run --rm \
  --env-file /etc/skillviz/postgres.env \
  -e IMPUTE_DRY_RUN=1 \
  -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest
```

Attendersi circa **20 minuti** sulla finestra di 24 mesi. Nel log devono
comparire: il numero di annunci letti, la normalizzazione dei codici
`Unclassifiable`, il mix dei metodi del k-NN, il percorso xgboost, e infine
`IMPUTE_DRY_RUN set — nothing written`.

Se la prova va a buon fine, ripetere senza `IMPUTE_DRY_RUN` per la prima
scrittura reale.

---

## 6. Esecuzione in produzione

```sh
docker run --rm \
  --name skillviz-impute \
  --user "$(id -u):$(id -g)" \
  --env-file /etc/skillviz/postgres.env \
  -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest
```

In cron, giornaliero:

```cron
# Imputazione CP2021, ogni giorno alle 05:40
40 5 * * * monty docker run --rm --name skillviz-impute --user "$(id -u):$(id -g)" \
  --env-file /etc/skillviz/postgres.env -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest >> /var/log/skillviz/impute.log 2>&1
```

La concorrenza è protetta due volte: `--name` fa fallire subito una seconda
`docker run`, e il file di lock interno riconosce un PID morto tramite `ps`.

---

## 7. Codici di uscita

| Codice | Significato | Che fare |
|---|---|---|
| `0` | completato, oppure niente da fare | — |
| `1` | errore | leggere il log: connessione, asserzione di coerenza o scrittura |
| `2` | `skillviz` assente dall'immagine | ricostruire l'immagine |
| `3` | un'altra esecuzione tiene il lock | attendere, oppure rimuovere il lock se il processo è morto |

Un'uscita `137` non è un codice del job ma la terminazione da parte del kernel
per esaurimento memoria: aumentare la RAM disponibile o abbassare
`IMPUTE_DENSE_BUDGET`.

---

## 8. Dopo un cambio di modello

La modalità di esecuzione è decisa confrontando i **mesi** già presenti nella
tabella di destinazione, quindi il job non può accorgersi che il modello è
cambiato, solo che i dati lo sono. Dopo un aggiornamento dell'immagine che
modifichi la codifica, forzare una ricostruzione completa:

```sh
docker run --rm --env-file /etc/skillviz/postgres.env \
  -e IMPUTE_FORCE_FULL=1 \
  -v /var/skillviz/run:/var/skillviz/run \
  skillviz-impute:latest
```

La scrittura resta uno scambio atomico: la tabella viva viene sostituita solo a
ricostruzione completata, quindi i lettori non vedono mai uno stato intermedio.

Se la tabella di destinazione è precedente all'introduzione delle colonne di
livello 5, la ricostruzione completa **è automatica**: il job se ne accorge e la
forza da sé, perché un'aggiunta incrementale non può creare colonne nuove.

---

## 9. Verifica dopo l'installazione

Test offline delle funzioni che decidono che cosa viene scritto, senza database:

```sh
Rscript container/test_impute_cp4.R
```

Controllo sulla tabella prodotta:

```sql
SELECT count(*)                                              AS righe,
       count(cp2021_id_level_4)                              AS con_cp4,
       count(cp2021_id_level_5)                              AS con_cp5,
       count(*) FILTER (WHERE cp4_imputed = 1)               AS cp4_imputati,
       count(*) FILTER (WHERE cp2021_id_level_4 LIKE '%.0')  AS cp4_malformati
FROM staging.gm_cp4_imputed;
```

`cp4_malformati` **deve essere zero**: nessun codice CP2021 di livello 4 termina
con `.0`, e il job interrompe l'esecuzione se ne produce uno. La stessa verifica
non va applicata al livello 5, dove 340 codici su 813 terminano legittimamente
con `.0`.
