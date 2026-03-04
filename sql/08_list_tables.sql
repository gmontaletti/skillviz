-- 08_list_tables.sql
-- Inventory of tables, views, and materialized views in the OJA database
-- Run to verify available objects before extraction

-- 1. List all tables -----
SELECT table_schema, table_name, table_type
FROM information_schema.tables
WHERE table_schema = 'public'
ORDER BY table_type, table_name;

-- 2. List materialized views -----
SELECT schemaname, matviewname, ispopulated
FROM pg_matviews
WHERE schemaname = 'public'
ORDER BY matviewname;

-- 3. Column inventory for key tables -----
SELECT table_name, column_name, data_type, is_nullable
FROM information_schema.columns
WHERE table_schema = 'public'
  AND table_name IN (
    'mappa_cpv_esco_iv',
    'annunci_skills_ojv_lightcast_it',
    'annunci_professioni_ojv_lightcast',
    'mappa_shdl_skill',
    'annunci_skills_ojv'
  )
ORDER BY table_name, ordinal_position;
