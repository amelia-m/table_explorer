# Table Relationship Explorer

An interactive tool for mapping primary keys, foreign keys, and inter-table relationships across tabular data. Upload files or connect to a database, get an interactive ERD, column-level metadata, and exportable reports.

The primary implementation is **R/Shiny**. A **Python/Streamlit** version also exists but has fewer features and is not actively maintained at this time.

---

## R/Shiny Version (primary)

### Features

| Category | Detail |
|---|---|
| **File support** | CSV, TSV, Excel (xlsx/xlsm/xls), ODS, Parquet, JSON, NDJSON, SPSS, SAS, Stata, RDS, RData, Access (.mdb/.accdb) |
| **Database connectors** | PostgreSQL, MySQL, SQL Server, SQLite, Snowflake, BigQuery, Redshift, Oracle |
| **Schema import** | JSON/YAML schema files with inline PK/FK annotations |
| **7-signal FK detection** | Naming conventions, fuzzy name similarity, value overlap, cardinality, format fingerprint, distribution similarity, null-pattern correlation |
| **Confidence scoring** | Noisy-OR composite scoring with low/medium/high confidence tiers; filter by minimum confidence |
| **Signal toggles** | Enable/disable individual signals; changes take effect on "Run Detection" button click |
| **Scan triage** | Estimates scan complexity for large schemas; lets you choose full scan, naming-only, or skip |
| **Interactive ERD** | visNetwork graph with drag, zoom, hover tooltips; layout modes (force/hierarchical/circular), spring length slider |
| **Table Details** | Per-table column summary with type, non-null count, unique values, PK/FK flags, table size |
| **Relationships tab** | Grouped by detection method with confidence scores, signal chips, suppress/restore controls |
| **Name cleaning** | Automatic table and column name cleaning via janitor conventions, with full rename log |
| **Manual overrides** | Add relationships auto-detection misses |
| **Exports** | Relationships CSV, dbt schema.yml, Mermaid ERD, session save/restore (JSON) |
| **Duplicate handling** | Detects re-uploads by file size/dimensions; offers overwrite, keep both, or skip |

### Architecture (golem package)

The app is structured as an R package using the [golem](https://thinkr-open.github.io/golem/) framework.

```
R/
  run_app.R              Entry point — tableexplorer::run_app()
  app_ui.R               Top-level UI (assembles modules)
  app_server.R           Top-level server (wires module reactive values)
  mod_upload.R           Upload panel + manual override
  mod_db_connect.R       Database connection panel
  mod_detection.R        Detection controls + scan triage
  mod_erd.R              ERD visualization (visNetwork)
  mod_table_details.R    Per-table column summary
  mod_relationships.R    Relationships tab
  mod_name_changes.R     Name changes / rename log tab
  mod_export.R           Export panel (CSV, dbt YAML, Mermaid, session)
  utils_inference.R      7-signal PK/FK detection engine
  utils_file_readers.R   Multi-format file parser (18 formats)
  utils_db_connectors.R  Database connection, introspection, loading
  utils_export.R         dbt YAML, Mermaid ERD, session JSON
  utils_vis.R            ERD network builder (build_network)
  utils_helpers.R        Shared helpers (%||%)
inst/
  app/www/               Static assets (styles.css, app.js)
  extdata/               Sample data + schema.json
  golem-config.yml       App configuration
dev/
  01_start.R             One-time project setup
  02_dev.R               Development helpers
  03_deploy.R            Deployment helpers
tests/testthat/          Unit tests (~100 cases across 4 suites)
```

**Reactive data flow between modules:**

```
app_server.R
├─ all_tables_rv   ← mod_upload, mod_db_connect (mutate)
├─ schema_rels_rv  ← mod_upload, mod_db_connect (mutate)
├─ manual_rels_rv  ← mod_upload (returns)
├─ detection opts  ← mod_detection (returns)
├─ pk_map_rv, composite_pk_map_rv, auto_rels_rv  (computed)
├─ all_rels_rv     → mod_erd, mod_table_details, mod_relationships, mod_export
└─ false_positives_rv ← mod_relationships (mutated via observeEvents)
```

### Installation

```r
# Install from GitHub
remotes::install_github("amelia-m/table_explorer")
```

**Optional packages** (installed on demand for specific formats/databases):

```r
# File formats
install.packages(c("readxl", "arrow", "haven", "readODS", "jsonlite", "yaml"))

# Databases
install.packages(c("DBI", "RSQLite", "RPostgres", "RMariaDB", "odbc", "bigrquery"))

# Fuzzy name matching
install.packages("stringdist")

# Access databases (Java-based)
install.packages("RJDBC")
```

### Run locally

```r
# After install:
library(tableexplorer)
run_app()

# Or during development (no install needed):
pkgload::load_all()
run_app()
```

### The 7 inference signals

| Signal | What it measures | Weight |
|---|---|---|
| **Naming** | Exact FK naming patterns (`{table}_id`, `{table}_key`) | 1.00 |
| **Name similarity** | Jaro-Winkler fuzzy match between column/table names | 0.60 |
| **Value overlap** | Fraction of values in column A that exist in column B | 0.55-0.90 |
| **Cardinality** | Whether column A looks like an FK (many rows, few unique values) | 0.95 |
| **Format fingerprint** | Whether both columns share value format (UUID, ISO date, email, etc.) | 0.40 |
| **Distribution similarity** | KS-test on numeric value distributions | 0.30-0.50 |
| **Null-pattern correlation** | Pearson correlation of null positions across tables | 0.20 |

Scores are combined via noisy-OR aggregation: `score = 1 - prod(1 - weights)`. The composite score maps to confidence tiers: high (>= 0.85), medium (>= 0.55), low (< 0.55).

### Schema file format

JSON or YAML with a `tables` array. Each table lists columns with optional `primary_key` and `foreign_key` annotations:

```json
{
  "tables": [
    {
      "name": "orders",
      "columns": [
        {"name": "order_id", "type": "integer", "primary_key": true},
        {"name": "customer_id", "type": "integer",
         "foreign_key": {"table": "customers", "column": "customer_id"}}
      ]
    }
  ]
}
```

A sample schema (`schema.json`) is included in the repository.

### Deploy to Posit Connect

See `dev/03_deploy.R` for deployment helpers.

**From VS Code / Positron:** Install the Posit Publisher extension, open the project folder, and deploy as a Shiny Application.

**From GitHub:** Push to a public repo, then in Posit Connect go to New Content > Import from Git.

> Free plan limits: 4 GB RAM, 1 CPU, 20 active hours/month, 5 apps max.

### Running tests

```r
# Recommended: uses devtools and loads the package properly
devtools::test()

# Or directly with testthat:
testthat::test_package("tableexplorer")
```

---

## Python/Streamlit Version

> **Note:** The Python version is in an earlier development phase and has fewer features than the R/Shiny version. It will be updated over time but is not the current development focus.

### Current Python features

- Multi-format file upload (CSV, TSV, Excel, ODS, Parquet, JSON, NDJSON)
- 7-signal FK inference engine with confidence scoring
- JSON/YAML schema input
- Interactive ERD via PyVis
- Dark/light mode toggle
- Manual relationship overrides
- Relationship caching (MD5-digested)

### Not yet in Python

- Database connectors
- Scan triage for large schemas
- Run Detection button (settings changes trigger immediately)
- Table name cleaning
- dbt YAML / Mermaid ERD / session save-restore exports
- Table size in exports
- Relationship suppress/restore UI

### Python setup

```bash
pip install -r requirements.txt
streamlit run app.py
```

Opens at `http://localhost:8501`.

### Python deployment

Deploys to Streamlit Community Cloud, Docker, Railway, Render, Fly.io, or any Python host:

```bash
streamlit run app.py --server.port=$PORT --server.address=0.0.0.0
```

---

## Sample data

The `sample_data/` directory contains a 4-table e-commerce dataset:

```
orders.csv          customers.csv        products.csv        order_items.csv
-----------         -------------        ------------        ---------------
order_id (PK)       customer_id (PK)     product_id (PK)     item_id (PK)
customer_id  -->    name                 name                 order_id     -->
product_id   -->    email                category             product_id   -->
amount              country              price                quantity
order_date                                                    unit_price
```

The app auto-detects `customer_id` in `orders` -> `customers`, `product_id` in `orders` -> `products`, etc.
