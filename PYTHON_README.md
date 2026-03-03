# ◫ Table Relationship Explorer

A Streamlit app for mapping primary keys, foreign keys, and inter-table relationships across tabular data files and JSON/YAML schema definitions. Upload your data and instantly get an interactive ERD, column-level metadata, and an exportable relationships report.

---

## Features

- **Interactive ERD** — drag, zoom, pin, and hover over nodes and edges rendered with vis.js via pyvis
- **Multi-format upload** — CSV, TSV, Excel (xlsx/xlsm/xls), OpenDocument (ods), Parquet, JSON, and NDJSON; multi-sheet Excel files load each sheet as its own table
- **JSON/YAML schema input** — define exact relationships without needing data; schema-defined links always take priority over auto-detection
- **7-signal FK inference engine** — naming conventions, fuzzy name matching, value overlap, cardinality, format fingerprinting, distribution similarity, and null-pattern correlation all run in parallel and vote on each candidate relationship
- **Confidence scoring** — each detected relationship gets a composite score and low/medium/high confidence label; filter by minimum confidence in the sidebar
- **Signal toggles** — enable or disable individual content-analysis signals without changing the detection method
- **Table Details tab** — per-table column summary with type, non-null count, unique count, and PK/FK flags
- **Relationships tab** — grouped by detection method with CSV export
- **Dark / light mode** — toggle in the sidebar; all colors including the ERD update instantly
- **Manual overrides** — add relationships auto-detection misses; render in red on the ERD
- **Relationship caching** — FK detection is MD5-digested and cached; re-renders don't re-scan unless the data, method, or theme changes

---

## Getting Started

### 1. Install dependencies

```bash
pip install -r requirements.txt
```

### 2. Run the app

```bash
streamlit run app.py
```

Opens at `http://localhost:8501` by default.

---

## Requirements

```
streamlit>=1.32.0
pandas>=2.0.0
networkx>=3.0
pyvis>=0.3.2
pyyaml>=6.0
scipy>=1.10.0
openpyxl>=3.0.0
xlrd>=2.0.0
odfpy>=1.4.0
pyarrow>=12.0.0
```

---

## Usage

### Loading data

Use the **Upload Tables** panel in the sidebar. Supported formats:

| Format | Extensions |
|---|---|
| CSV | `.csv` |
| TSV | `.tsv` |
| Excel | `.xlsx`, `.xlsm`, `.xls` |
| OpenDocument Spreadsheet | `.ods` |
| Parquet | `.parquet` |
| JSON array | `.json` |
| Newline-delimited JSON | `.ndjson` |

Multiple files can be uploaded at once. Uploading a file with the same name as an existing table replaces it. Multi-sheet Excel files are split into one table per sheet, named `filename_sheetname`.

**JSON/YAML schema** — use the *Schema Definition* panel to upload a `.json`, `.yaml`, or `.yml` file. Schema tables render as ERD nodes even without row data, and their foreign keys appear as exact (blue) edges.

### Detection methods

| Method | How it works |
|---|---|
| **All signals (recommended)** | Runs all 7 signals and combines scores |
| **Naming conventions only** | Pattern-matches column names like `id`, `{table}_id` |
| **Content analysis only** | Runs value/statistical signals without name matching |
| **Manual only** | Disables auto-detection entirely |

Schema-defined relationships always take priority and are never overridden by auto-detection.

### The 7 inference signals

| Signal | What it measures |
|---|---|
| **Naming** | Exact name patterns: `id`, `{table}_id`, `{table}_key` |
| **Name similarity** | Fuzzy match between column names across tables |
| **Value overlap** | Fraction of values in column A that exist in column B |
| **Cardinality** | Whether column A looks like an FK (many values → few unique) |
| **Format fingerprint** | Whether both columns share the same value format (e.g. UUID, YYYY-MM-DD, INT) |
| **Distribution similarity** | KS-test on numeric value distributions |
| **Null-pattern correlation** | Pearson correlation of null positions (useful when tables share row counts) |

Each signal contributes a score 0–1. Scores are combined into a composite confidence rating of **low**, **medium**, or **high**.

### Confidence filtering

Use the **Min confidence to show** slider to hide low-confidence edges from the ERD and relationships list. `medium` is the default.

### Manual overrides

Use the **Manual Override** panel to add relationships auto-detection misses. Select the from-table, from-column, to-table, and to-column (PK), then click **＋ Add Relationship**. Manual relationships render in red on the ERD.

### ERD interaction

- **Drag** nodes to rearrange; dragged nodes pin in place
- **Double-click** a pinned node to unpin it
- **Scroll** to zoom
- **Hover** over a node or edge for a detailed tooltip

### ERD legend

| Color | Meaning |
|---|---|
| Green | Naming convention |
| Teal | Fuzzy name similarity |
| Orange | Value overlap |
| Yellow | Cardinality |
| Purple | Format fingerprint |
| Blue | Distribution similarity / schema-defined |
| Red | Manual override |

---

## Schema File Format

Schemas are a JSON or YAML object with a `tables` array. Each table lists its columns with optional `primary_key` and `foreign_key` annotations.

```json
{
  "tables": [
    {
      "name": "orders",
      "columns": [
        {"name": "order_id",    "type": "integer", "primary_key": true},
        {"name": "customer_id", "type": "integer",
         "foreign_key": {"table": "customers", "column": "customer_id"}},
        {"name": "amount",      "type": "numeric"},
        {"name": "order_date",  "type": "date"}
      ]
    },
    {
      "name": "customers",
      "columns": [
        {"name": "customer_id", "type": "integer", "primary_key": true},
        {"name": "name",        "type": "text"},
        {"name": "email",       "type": "text"}
      ]
    }
  ]
}
```

An `example_schema.json` is included — a 5-table e-commerce model ready to load.

---

## Deployment

### Streamlit Community Cloud (free)

1. Push the repo to GitHub
2. Go to [share.streamlit.io](https://share.streamlit.io) and connect your repo
3. Set the main file path to `app.py`
4. Deploy — no additional configuration needed

### Docker

```dockerfile
FROM python:3.11-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY app.py .
EXPOSE 8501
CMD ["streamlit", "run", "app.py", "--server.port=8501", "--server.address=0.0.0.0"]
```

```bash
docker build -t table-explorer .
docker run -p 8501:8501 table-explorer
```

### Other platforms

No external service dependencies. Deploys to Railway, Render, Fly.io, or any Python host. Start command:

```
streamlit run app.py --server.port=$PORT --server.address=0.0.0.0
```

---

## Project Structure

```
.
├── app.py                  # Main Streamlit application
├── requirements.txt        # Python dependencies
├── example_schema.json     # Sample 5-table e-commerce schema
└── README.md
```

---

## Performance Notes

- FK detection is skipped for tables with more than 100,000 rows or 200 columns
- Results are cached by an MD5 digest of table names, dimensions, detection method, signal flags, confidence threshold, and current theme
- Schema-defined relationships bypass all scanning entirely
- The null-pattern correlation signal is disabled by default — only meaningful when both tables share the same row count

---

## Migrated from R/Shiny

Originally an R/Shiny app. Key differences in the Python rewrite:

- `pandas` replaces R data frames — faster parsing, better dtype inference, multi-format support
- `pyvis` + vis.js replaces `visNetwork` for the ERD
- 7-signal inference engine replaces simple heuristic naming/uniqueness checks
- JSON/YAML schema input is new
- Dark/light theming is new
- Deploys to any Python host instead of requiring Posit Connect
