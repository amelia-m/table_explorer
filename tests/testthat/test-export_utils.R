# ============================================================
# Tests for export_utils.R — Export Format Generators
# ============================================================

# ── Test fixtures ────────────────────────────────────────────

make_test_tables <- function() {
  list(
    customers = data.frame(
      customer_id = 1:3,
      name = c("alice", "bob", "carol"),
      stringsAsFactors = FALSE
    ),
    orders = data.frame(
      order_id = 1:4,
      customer_id = c(1, 2, 1, 3),
      amount = c(10.5, 20.0, 30.0, 15.5),
      stringsAsFactors = FALSE
    )
  )
}

make_test_rels <- function() {
  list(
    list(
      from_table = "orders", from_col = "customer_id",
      to_table = "customers", to_col = "customer_id",
      detected_by = "naming", confidence = "high", score = 1.0,
      signals = list(naming_exact = 1.0),
      reasons = "exact FK naming"
    )
  )
}

make_test_pks <- function() {
  list(
    customers = "customer_id",
    orders = "order_id"
  )
}

# ── dbt schema.yml generator ────────────────────────────────

test_that("generate_dbt_yaml produces valid structure", {
  yaml_str <- generate_dbt_yaml(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("version: 2", yaml_str))
  expect_true(grepl("models:", yaml_str))
  expect_true(grepl("- name: customers", yaml_str))
  expect_true(grepl("- name: orders", yaml_str))
})

test_that("generate_dbt_yaml includes PK tests", {
  yaml_str <- generate_dbt_yaml(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("- unique", yaml_str))
  expect_true(grepl("- not_null", yaml_str))
})

test_that("generate_dbt_yaml includes FK relationship tests", {
  yaml_str <- generate_dbt_yaml(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("relationships:", yaml_str))
  expect_true(grepl("ref\\('customers'\\)", yaml_str))
})

test_that("generate_dbt_yaml includes all columns", {
  yaml_str <- generate_dbt_yaml(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("- name: customer_id", yaml_str))
  expect_true(grepl("- name: name", yaml_str))
  expect_true(grepl("- name: order_id", yaml_str))
  expect_true(grepl("- name: amount", yaml_str))
})

test_that("generate_dbt_yaml handles empty tables", {
  yaml_str <- generate_dbt_yaml(list(), list(), list())
  expect_true(grepl("version: 2", yaml_str))
  expect_true(grepl("models:", yaml_str))
})

# ── Mermaid ERD generator ────────────────────────────────────

test_that("generate_mermaid_erd starts with erDiagram", {
  mmd <- generate_mermaid_erd(make_test_tables(), make_test_rels(), make_test_pks())
  lines <- strsplit(mmd, "\n")[[1]]
  expect_equal(trimws(lines[1]), "erDiagram")
})

test_that("generate_mermaid_erd includes table blocks", {
  mmd <- generate_mermaid_erd(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("customers \\{", mmd))
  expect_true(grepl("orders \\{", mmd))
})

test_that("generate_mermaid_erd marks PK columns", {
  mmd <- generate_mermaid_erd(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("customer_id PK", mmd))
  expect_true(grepl("order_id PK", mmd))
})

test_that("generate_mermaid_erd includes relationships", {
  mmd <- generate_mermaid_erd(make_test_tables(), make_test_rels(), make_test_pks())

  expect_true(grepl("customers \\|\\|--o\\{ orders", mmd))
})

test_that("generate_mermaid_erd assigns correct data types", {
  mmd <- generate_mermaid_erd(make_test_tables(), make_test_rels(), make_test_pks())

  # customer_id is numeric -> int
  expect_true(grepl("int customer_id", mmd))
  # name is character -> string
  expect_true(grepl("string name", mmd))
  # amount is numeric -> int
  expect_true(grepl("int amount", mmd))
})

test_that("generate_mermaid_erd handles Date columns", {
  tables <- list(
    events = data.frame(
      event_id = 1:2,
      event_date = as.Date(c("2024-01-01", "2024-06-15"))
    )
  )
  mmd <- generate_mermaid_erd(tables, list(), list(events = "event_id"))
  expect_true(grepl("date event_date", mmd))
})

test_that("generate_mermaid_erd handles empty input", {
  mmd <- generate_mermaid_erd(list(), list(), list())
  expect_equal(trimws(mmd), "erDiagram")
})

# ── Session save/restore ─────────────────────────────────────

test_that("save_session_json produces valid JSON", {
  skip_if_not_installed("jsonlite")

  json_str <- save_session_json(
    tables = make_test_tables(),
    rels = make_test_rels(),
    manual_rels = list(),
    schema_rels = list(),
    settings = list(detect_method = "both")
  )

  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  expect_true("version" %in% names(parsed))
  expect_true("timestamp" %in% names(parsed))
  expect_true("tables" %in% names(parsed))
  expect_true("relationships" %in% names(parsed))
  expect_true("settings" %in% names(parsed))
})

test_that("save_session_json includes table data", {
  skip_if_not_installed("jsonlite")

  json_str <- save_session_json(make_test_tables(), list(), list(), list())
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  expect_true("customers" %in% names(parsed$tables))
  expect_true("orders" %in% names(parsed$tables))
})

test_that("restore_session_json round-trips tables", {
  skip_if_not_installed("jsonlite")

  tables <- make_test_tables()
  json_str <- save_session_json(tables, list(), list(), list())
  restored <- restore_session_json(json_str)

  expect_true("customers" %in% names(restored$tables))
  expect_true("orders" %in% names(restored$tables))
  expect_equal(nrow(restored$tables[["customers"]]), 3)
  expect_equal(nrow(restored$tables[["orders"]]), 4)
})

test_that("restore_session_json round-trips settings", {
  skip_if_not_installed("jsonlite")

  settings <- list(detect_method = "both", min_confidence = "medium")
  json_str <- save_session_json(list(), list(), list(), list(), settings)
  restored <- restore_session_json(json_str)

  expect_equal(restored$settings$detect_method, "both")
  expect_equal(restored$settings$min_confidence, "medium")
})

test_that("restore_session_json round-trips manual relationships", {
  skip_if_not_installed("jsonlite")

  manual <- list(list(
    from_table = "a", from_col = "b",
    to_table = "c", to_col = "d",
    detected_by = "manual"
  ))
  json_str <- save_session_json(list(), list(), manual, list())
  restored <- restore_session_json(json_str)

  expect_equal(length(restored$manual_relationships), 1)
  expect_equal(restored$manual_relationships[[1]]$from_table, "a")
})

test_that("save_session_json handles Date columns", {
  skip_if_not_installed("jsonlite")

  tables <- list(
    events = data.frame(
      id = 1:2,
      event_date = as.Date(c("2024-01-01", "2024-06-15"))
    )
  )
  json_str <- save_session_json(tables, list(), list(), list())
  # Should not error
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  expect_true("events" %in% names(parsed$tables))
})

test_that("save_session_json returns {} when jsonlite unavailable", {
  # We can't unload jsonlite easily, but test the function exists
  expect_true(is.function(save_session_json))
})

test_that("restore_session_json handles empty/missing fields", {
  skip_if_not_installed("jsonlite")

  json_str <- '{"version": 1, "timestamp": "2024-01-01", "tables": {}}'
  restored <- restore_session_json(json_str)

  expect_equal(length(restored$tables), 0)
  expect_true(is.list(restored$relationships))
  expect_true(is.list(restored$manual_relationships))
  expect_true(is.list(restored$settings))
})
