# skillviz (in sviluppo)

## API split per scoring veloce

* `match_professions_prepare()` e `match_professions_score()` separano la
  costruzione della matrice professione × competenza (operazione costosa,
  invariante rispetto alla query) dal calcolo della distanza rispetto a un
  insieme di competenze dichiarate (operazione leggera, sparsa). Il wrapper
  `match_professions()` resta invariato per backward compatibility e
  delega a entrambi internamente. Usare il pattern split nelle interfacce
  reattive che valutano molte query sulla stessa base.

## Query pesata

* `match_professions_score()` e `match_professions()` accettano l'argomento
  `weights` (numerico, non negativo, una entry per ogni competenza
  dichiarata). I pesi modulano la massa L2 del vettore di query; con pesi
  uniformi a 1 il comportamento coincide bit-per-bit con la versione non
  pesata. I denominatori di `hit_ratio` e `mean_coverage` usano la somma
  dei pesi al quadrato per mantenere coerenza dimensionale.

## Nuove funzionalità

* `match_professions()` — a partire da un vettore di identificativi ESCO L3
  dichiarati, classifica le professioni ESCO L4 per affinità al profilo
  desiderato. Restituisce una graduatoria con distanza coseno complessiva e la
  decomposizione additiva del contributo di ogni competenza alla distanza
  totale. Tre basi di profilo selezionabili: `coverage` (probabilità che un
  annuncio della professione richieda la competenza), `tfidf` (term frequency
  per professione × inverse document frequency, pesa le competenze
  discriminanti) e `rca` (indice di Balassa, misura di specializzazione).
  Riusa `build_skill_prof_sparse()` per la matrice sparsa professioni ×
  competenze e `compute_balassa_index()` per la base RCA.

# skillviz 0.1.0

Versione iniziale.
