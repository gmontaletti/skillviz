-- 04_extract_specific_skills.sql
-- Extract IDF-ranked skills per profession (top 36 per profession)
-- Source view: v_annunci_specific_skills
-- Used by: extract_specific_skills_cpi3(), build_profession_skill_profile()
--
-- The materialized view mv_annunci_prof_skills_v3 provides a cached version
-- of v_annunci_prof_skills_v3 for better read performance.

-- Option A: from materialized view (faster)
SELECT *
FROM mv_annunci_prof_skills_v3;

-- Option B: from live view (always up to date)
-- SELECT *
-- FROM v_annunci_prof_skills_v3;
