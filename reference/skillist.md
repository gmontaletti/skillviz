# Master skills list

Pre-computed skills metadata including frequency, reuse type, and domain
flags (soft-skills, ICT, green, language).

## Usage

``` r
skillist
```

## Format

A data.table with columns:

- escoskill_level_3:

  Skill identifier (ESCO level 3)

- esco_v0101_reusetype:

  Reuse type: sector-specific, transversal, occupation-specific,
  multisettoriale

- N:

  Recurrence count across all announcements

- tipo:

  Mapped Italian type: settoriale, trasversale, specifico,
  multisettoriale

- pillar_softskills:

  Soft-skill flag

- esco_v0101_ict:

  ICT skill flag

- esco_v0101_green:

  Green skill flag

- esco_v0101_language:

  Language skill flag

## Source

Computed from OJA data via Lightcast/EMSI
