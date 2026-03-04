# _targets.R — skillviz analysis pipeline -----
#
# Minimal targets workflow using skillviz::read_ojv_zip() for data
# loading. Covers data ingestion and crosswalk preparation; further
# stages (relevance, co-occurrence, clustering, temporal) can be
# appended as needed.
#
# Usage:
#   targets::tar_make()       # run the full pipeline
#   targets::tar_visnetwork() # visualize the DAG
#   targets::tar_read(name)   # read a target result
#
# Before running, update the file paths below to match your data
# locations. Paths are relative to this wf/ directory.

library(targets)

# 1. Global options -----

tar_option_set(
  packages = c("skillviz", "data.table", "readxl")
)

# 2. Source pipeline helpers -----

source("R/functions.R")

# 3. File paths -----
# Update these to match your local data layout.
# Note: wf/ is inside skillviz/, so ../../ reaches progetti/.

path_ojv <- "/Users/giampaolomontaletti/Documents/annunci"
path_esco <- "../../datasets/escoIV.fst"
path_cp2021 <- "../../xlsx/CP2021.xlsx"

# 4. Pipeline targets -----

list(
  # -- Stage 1: Data loading -----
  # normalize_ojv() reads ZIP files, deduplicates postings, and ensures
  # referential integrity across tables. Returns a list with $postings,
  # $skills, $companies.

  tar_target(ojv, normalize_ojv(path_ojv)),

  tar_target(postings_raw, ojv$postings),

  tar_target(skills_raw, ojv$skills),

  tar_target(esco_mapping, read_esco_mapping(file = path_esco)),

  tar_target(cpi3, load_cpi3(path_cp2021)),

  # -- Stage 2: Data preparation -----
  # Crosswalk construction, ESCO mapping, classification, and merging.

  tar_target(cpi_esco, build_cpi_esco_crosswalk(esco_mapping, cpi3)),

  tar_target(annunci_esco, prepare_annunci_esco(postings_raw, esco_mapping)),

  tar_target(
    cpi_nb,
    classify_esco_to_cpi(postings_raw, skills_raw, crosswalk = cpi_esco)
  ),

  tar_target(cpi_esco_full, augment_crosswalk(cpi_esco, cpi_nb)),

  tar_target(
    skills_merged,
    merge_skills_annunci(postings_raw, skills_raw, cpi_esco_full)
  )
)
