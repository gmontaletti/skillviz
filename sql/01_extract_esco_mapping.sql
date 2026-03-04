-- 01_extract_esco_mapping.sql
-- Extract CPI to ESCO Level 4 crosswalk mapping
-- Source table: mappa_cpv_esco_iv
-- Used by: build_cpi_esco_crosswalk(), prepare_annunci_esco()

SELECT *
FROM mappa_cpv_esco_iv
ORDER BY 1;
