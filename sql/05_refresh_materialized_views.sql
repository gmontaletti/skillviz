-- 05_refresh_materialized_views.sql
-- Refresh materialized views before data extraction
-- Run this when underlying data has been updated

REFRESH MATERIALIZED VIEW mv_annunci_prof_skills_v3;
