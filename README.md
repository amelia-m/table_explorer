# Table Relationship Explorer

An interactive Shiny app that visualises relationships between CSV tables — primary keys, foreign keys, row/column counts, and cross-table links.

---

## Features

| Feature | Detail |
|---|---|
| 📁 Multi-CSV upload | Upload as many CSVs as you like at once |
| 🔍 Auto PK detection | By naming convention (`id`, `{table}_id`) and/or column uniqueness |
| 🔗 Auto FK detection | By naming match and/or value-subset overlap |
| ✏️ Manual overrides | Add or override any relationship via dropdowns |
| 🕸️ ERD Diagram | Interactive visNetwork graph — drag, zoom, hover for details |
| 📊 Table Details | Per-table column summary with type, nulls, uniqueness, PK/FK flags |
| 🔗 Relationships list | Grouped by detection method, exportable as CSV |

---

## Required Packages

```r
install.packages(c("shiny", "visNetwork", "DT", "shinythemes"))
```

---

## Run Locally

```r
# From the project folder:
shiny::runApp("app.R")

# Or from R:
setwd("/path/to/table_explorer")
shiny::runApp()
```

---

## Deploy to Posit Connect (Free Plan)

### Option A — Deploy from Positron or VS Code

1. Install the **Posit Publisher** extension in VS Code or Positron.
2. Open the `table_explorer/` folder.
3. Click the Posit icon in the sidebar → **Publish** → select **Shiny Application**.
4. Log in with your free Posit Connect account.
5. Click **Deploy**.

### Option B — Deploy from a Public GitHub Repo

1. Push `app.R` to a **public** GitHub repository (free plan requires public repos).
2. In Posit Connect, go to **New Content → Import from Git**.
3. Paste your repo URL, select `app.R`, and deploy.

> **Free plan limits:** 4 GB RAM · 1 CPU · 20 active hours/month · 5 apps max.  
> This app is very lightweight — well within the free tier.

---

## How Key Detection Works

### Primary Keys (PKs)
- **Naming:** Column is named exactly `id`, `{tablename}_id`, or `{tablename}id` (case-insensitive).
- **Uniqueness:** Column has no NAs and all values are unique (no duplicates).

### Foreign Keys (FKs)
- **Naming:** Column in table A is named `{tablename_B}_id` or `{tablename_B}id`.
- **Uniqueness / Value subset:** All non-null values in column A exist in a PK column of table B.

You can switch between methods or combine them using the sidebar dropdown.

---

## Sample CSV Layout

```
orders.csv          customers.csv        products.csv
─────────────       ─────────────        ────────────
order_id            customer_id          product_id
customer_id   ──→   name                 name
product_id    ──→   email                price
amount              country              ↑
order_date                               product_id (orders.csv)
```

The app will auto-detect `customer_id` in `orders` → `customer_id` in `customers`, etc.
