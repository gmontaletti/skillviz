-- 02_extract_annunci_skills.sql
-- Extract job announcements with ESCO skill assignments (Lightcast Italy)
-- Source table: annunci_skills_ojv_lightcast_it
-- Used by: compute_balassa_index(), compute_skill_diffusion(), cooc_all_professions()

SELECT *
FROM annunci_skills_ojv_lightcast_it;
