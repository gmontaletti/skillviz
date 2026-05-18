# skillviz (in sviluppo)

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
