# ============================================================
# Tests for file_readers.R — File Format Readers
# ============================================================

# ── %||% operator ────────────────────────────────────────────

test_that("%||% returns a when a is non-null and non-empty", {
  expect_equal("hello" %||% "default", "hello")
  expect_equal(42 %||% 0, 42)
})

test_that("%||% returns b when a is NULL or empty", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal("" %||% "fallback", "fallback")
  expect_equal(character(0) %||% "fallback", "fallback")
})

# ── supported_extensions ─────────────────────────────────────

test_that("supported_extensions includes core formats", {
  expect_true(".csv" %in% supported_extensions)
  expect_true(".tsv" %in% supported_extensions)
  expect_true(".xlsx" %in% supported_extensions)
  expect_true(".json" %in% supported_extensions)
  expect_true(".parquet" %in% supported_extensions)
  expect_true(".rds" %in% supported_extensions)
  expect_true(".mdb" %in% supported_extensions)
})

test_that("supported_extensions are all lowercase with dot prefix", {
  for (ext in supported_extensions) {
    expect_true(startsWith(ext, "."), info = paste("Missing dot:", ext))
    expect_equal(ext, tolower(ext), info = paste("Not lowercase:", ext))
  }
})

# ── CSV reading via read_table_file ──────────────────────────

test_that("read_table_file reads CSV files", {
  skip_if_not(dir.exists(sample_data_dir), "sample_data directory not found")

  path <- file.path(sample_data_dir, "customers.csv")
  result <- read_table_file(path, "customers.csv")

  expect_true(is.list(result))
  expect_true("tables" %in% names(result))
  expect_true("customers" %in% names(result$tables))
  df <- result$tables[["customers"]]
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)
})

test_that("read_table_file returns empty for unsupported extension", {
  errors <- character(0)
  notify <- function(msg) errors <<- c(errors, msg)

  # Create a temp file with unknown extension
  tmp <- tempfile(fileext = ".xyz")
  writeLines("test", tmp)
  on.exit(unlink(tmp))

  result <- read_table_file(tmp, "test.xyz", notify)
  expect_equal(length(result$tables), 0)
  expect_true(length(errors) > 0)
  expect_true(any(grepl("Unsupported", errors)))
})

test_that("read_table_file handles read errors gracefully", {
  errors <- character(0)
  notify <- function(msg) errors <<- c(errors, msg)

  tmp <- tempfile(fileext = ".csv")
  writeLines("this,is\nnot,properly,formatted\na,b,c,d", tmp)
  on.exit(unlink(tmp))

  result <- read_table_file(tmp, "broken.csv", notify)
  # Should either return data (lenient parsing) or catch error
  expect_true(is.list(result))
  expect_true("tables" %in% names(result))
})

# ── TSV reading ──────────────────────────────────────────────

test_that("read_table_file reads TSV files", {
  tmp <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(id = 1:3, name = c("alice", "bob", "carol")),
    tmp, sep = "\t", row.names = FALSE
  )
  on.exit(unlink(tmp))

  result <- read_table_file(tmp, "people.tsv")
  expect_true("people" %in% names(result$tables))
  expect_equal(nrow(result$tables[["people"]]), 3)
})

# ── RDS reading ──────────────────────────────────────────────

test_that("read_rds_file reads a data frame", {
  tmp <- tempfile(fileext = ".rds")
  df <- data.frame(x = 1:5, y = letters[1:5])
  saveRDS(df, tmp)
  on.exit(unlink(tmp))

  result <- read_rds_file(tmp, "test.rds", message)
  expect_true("test" %in% names(result$tables))
  expect_equal(nrow(result$tables[["test"]]), 5)
})

test_that("read_rds_file notifies for non-data-frame", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1, b = 2), tmp)
  on.exit(unlink(tmp))

  errors <- character(0)
  result <- read_rds_file(tmp, "test.rds", function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("does not contain", errors)))
})

# ── RData reading ────────────────────────────────────────────

test_that("read_rdata_file reads data frames from .RData", {
  tmp <- tempfile(fileext = ".rdata")
  my_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  another_df <- data.frame(c = 4:6)
  save(my_df, another_df, file = tmp)
  on.exit(unlink(tmp))

  result <- read_rdata_file(tmp, "test.rdata", message)
  expect_true("my_df" %in% names(result$tables))
  expect_true("another_df" %in% names(result$tables))
  expect_equal(nrow(result$tables[["my_df"]]), 3)
})

test_that("read_rdata_file notifies when no data frames found", {
  tmp <- tempfile(fileext = ".rdata")
  my_list <- list(a = 1)
  my_vec <- 1:10
  save(my_list, my_vec, file = tmp)
  on.exit(unlink(tmp))

  errors <- character(0)
  result <- read_rdata_file(tmp, "test.rdata", function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("No data frames", errors)))
})

# ── JSON reading ─────────────────────────────────────────────

test_that("read_json_file reads array-of-objects JSON", {
  skip_if_not_installed("jsonlite")

  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    data.frame(id = 1:3, name = c("a", "b", "c")),
    tmp
  )
  on.exit(unlink(tmp))

  result <- read_json_file(tmp, "items.json", message)
  expect_true("items" %in% names(result$tables))
  expect_equal(nrow(result$tables[["items"]]), 3)
})

test_that("read_json_file notifies for non-tabular JSON", {
  skip_if_not_installed("jsonlite")

  tmp <- tempfile(fileext = ".json")
  writeLines('{"key": "value", "nested": {"a": 1}}', tmp)
  on.exit(unlink(tmp))

  errors <- character(0)
  result <- read_json_file(tmp, "bad.json", function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
})

# ── Schema file parsing ─────────────────────────────────────

test_that("parse_schema_file reads the project schema.json", {
  skip_if_not_installed("jsonlite")
  skip_if_not(file.exists(schema_json_path), "schema.json not found")

  result <- parse_schema_file(schema_json_path, "schema.json")

  # Should have tables
  expect_true(length(result$tables) > 0)
  expect_true("customers" %in% names(result$tables))
  expect_true("orders" %in% names(result$tables))
  expect_true("products" %in% names(result$tables))

  # Should detect inline FK relationships
  expect_true(length(result$relationships) > 0)

  # Check relationship structure
  for (r in result$relationships) {
    expect_true(nzchar(r$from_table))
    expect_true(nzchar(r$from_col))
    expect_true(nzchar(r$to_table))
    expect_true(nzchar(r$to_col))
    expect_equal(r$detected_by, "schema")
    expect_equal(r$confidence, "high")
  }
})

test_that("parse_schema_file extracts FK from inline definitions", {
  skip_if_not_installed("jsonlite")
  skip_if_not(file.exists(schema_json_path), "schema.json not found")

  result <- parse_schema_file(schema_json_path, "schema.json")

  # orders.customer_id -> customers.customer_id should exist
  fk_descs <- vapply(result$relationships, function(r) {
    paste(r$from_table, r$from_col, r$to_table, r$to_col, sep = ".")
  }, character(1))

  expect_true(any(grepl("orders.*customer_id.*customers", fk_descs)))
  expect_true(any(grepl("orders.*product_id.*products", fk_descs)))
})

test_that("parse_schema_file handles YAML format", {
  skip_if_not_installed("yaml")

  tmp <- tempfile(fileext = ".yaml")
  writeLines(c(
    "tables:",
    "  - name: users",
    "    columns:",
    "      - name: user_id",
    "      - name: email",
    "relationships:",
    "  - from_table: orders",
    "    from_column: user_id",
    "    to_table: users",
    "    to_column: user_id"
  ), tmp)
  on.exit(unlink(tmp))

  result <- parse_schema_file(tmp, "schema.yaml")
  expect_true("users" %in% names(result$tables))
  expect_true(length(result$relationships) == 1)
  expect_equal(result$relationships[[1]]$from_table, "orders")
  expect_equal(result$relationships[[1]]$to_table, "users")
})

test_that("parse_schema_file notifies for unsupported format", {
  errors <- character(0)
  tmp <- tempfile(fileext = ".txt")
  writeLines("not a schema", tmp)
  on.exit(unlink(tmp))

  result <- parse_schema_file(tmp, "schema.txt", function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("Unsupported", errors)))
})

# ── Excel/Haven/Parquet graceful failure ─────────────────────

test_that("read_excel_file notifies when readxl missing", {
  # Only test if readxl is NOT installed
  skip_if(requireNamespace("readxl", quietly = TRUE), "readxl is installed")
  errors <- character(0)
  result <- read_excel_file("/fake/path.xlsx", "test.xlsx",
                            function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("readxl", errors)))
})

test_that("read_parquet_file notifies when arrow missing", {
  skip_if(requireNamespace("arrow", quietly = TRUE), "arrow is installed")
  errors <- character(0)
  result <- read_parquet_file("/fake/path.parquet", "test.parquet",
                              function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("arrow", errors)))
})

test_that("read_haven_file notifies when haven missing", {
  skip_if(requireNamespace("haven", quietly = TRUE), "haven is installed")
  errors <- character(0)
  result <- read_haven_file("/fake/path.sav", "test.sav", "sav",
                            function(msg) errors <<- c(errors, msg))
  expect_equal(length(result$tables), 0)
  expect_true(any(grepl("haven", errors)))
})
