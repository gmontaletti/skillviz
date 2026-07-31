# Deprecated functions in skillviz

The functions listed below are deprecated. Each one still works, but
emits a deprecation warning and will be removed in a future release.
Alternatives are given for each.

## Details

- [`read_ojv_zip`](https://gmontaletti.github.io/skillviz/reference/read_ojv_zip.md):

  Use
  [`read_oja_itaposts`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md).

- [`normalize_ojv`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md):

  Use
  [`read_oja_itaposts`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md).

Both read the raw Lightcast ZIP archives directly. That access path is
superseded by the DuckDB store maintained by the itaposts package, which
owns the read, dedup and join logic these two functions duplicate.
[`read_oja_itaposts`](https://gmontaletti.github.io/skillviz/reference/read_oja_itaposts.md)
returns the same `list(postings, skills, companies)` shape as
[`normalize_ojv`](https://gmontaletti.github.io/skillviz/reference/normalize_ojv.md),
so migrating a call site means changing only its data-loading line.
