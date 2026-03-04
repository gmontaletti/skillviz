# SQL Scripts for skillviz

SQL extraction scripts for the OJA (Online Job Advertising) PostgreSQL database.

## Database

- **Engine:** PostgreSQL (Azure)
- **Host:** `psiflariapostgres.postgres.database.azure.com`
- **Port:** `5432`
- **Database:** `postgres`
- **Schema:** `public` (default)

## Tables and Views

| Object | Type | Description |
|--------|------|-------------|
| `mappa_cpv_esco_iv` | Table | CPI to ESCO Level 4 crosswalk mapping |
| `annunci_skills_ojv` | Table | Job announcements with ESCO skill assignments |
| `annunci_skills_ojv_lightcast_it` | Table | Italian job ads with skills (Lightcast source) |
| `annunci_professioni_ojv_lightcast` | Table | Professions from Lightcast OJA data |
| `mappa_shdl_skill` | Table | Skill taxonomy mapping (SHDL) |
| `v_annunci_prof_skills_v3` | View | Denormalized professions + skills (v3) |
| `mv_annunci_prof_skills_v3` | Materialized View | Cached version of the above view |
| `v_annunci_specific_skills` | View | Top 36 IDF-ranked skills per profession |

## Scripts

| Script | Purpose |
|--------|---------|
| `01_extract_esco_mapping.sql` | Extract CPI-ESCO crosswalk |
| `02_extract_annunci_skills.sql` | Extract job ads with skill assignments |
| `03_extract_professions.sql` | Extract profession-level data from Lightcast |
| `04_extract_specific_skills.sql` | Extract IDF-ranked skills per profession |
| `05_refresh_materialized_views.sql` | Refresh materialized views before extraction |

## Usage from R

```r
library(DBI)
library(RPostgres)

conn <- dbConnect(
  Postgres(),
  dbname   = Sys.getenv("OJA_DBNAME", "postgres"),
  host     = Sys.getenv("OJA_HOST"),
  port     = as.integer(Sys.getenv("OJA_PORT", "5432")),
  user     = Sys.getenv("OJA_USER"),
  password = Sys.getenv("OJA_PASSWORD")
)

sql <- readLines("sql/01_extract_esco_mapping.sql")
dt <- data.table::setDT(dbGetQuery(conn, paste(sql, collapse = "\n")))

dbDisconnect(conn)
```

Set connection parameters in `.Renviron`:

```
OJA_HOST=psiflariapostgres.postgres.database.azure.com
OJA_PORT=5432
OJA_DBNAME=postgres
OJA_USER=your_user
OJA_PASSWORD=your_password
```
