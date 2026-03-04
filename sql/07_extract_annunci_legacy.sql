-- 07_extract_annunci_legacy.sql
-- Extract job announcements with skills (legacy pre-Lightcast source)
-- Source table: annunci_skills_ojv
-- Note: superseded by annunci_skills_ojv_lightcast_it for current analyses

SELECT *
FROM annunci_skills_ojv;
