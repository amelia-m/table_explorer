# ============================================================
# Tests for db_connectors.R — Database Connection Functions
# ============================================================

# ── db_types constant ────────────────────────────────────────

test_that("db_types has expected database types", {
  expect_true("postgres" %in% db_types)
  expect_true("mysql" %in% db_types)
  expect_true("sqlserver" %in% db_types)
  expect_true("snowflake" %in% db_types)
  expect_true("bigquery" %in% db_types)
  expect_true("redshift" %in% db_types)
  expect_true("sqlite" %in% db_types)
})

test_that("db_types has human-readable names", {
  nms <- names(db_types)
  expect_true(any(grepl("PostgreSQL", nms)))
  expect_true(any(grepl("MySQL", nms)))
  expect_true(any(grepl("SQLite", nms)))
})

# ── db_connect error handling ────────────────────────────────

test_that("db_connect notifies for unsupported type", {
  errors <- character(0)
  conn <- db_connect("fakedb", notify_fn = function(msg) errors <<- c(errors, msg))
  expect_null(conn)
  expect_true(any(grepl("Unsupported|failed", errors)))
})

test_that("db_connect notifies when package missing for postgres", {
  skip_if(requireNamespace("RPostgres", quietly = TRUE), "RPostgres is installed")
  errors <- character(0)
  conn <- db_connect("postgres", host = "localhost",
                     notify_fn = function(msg) errors <<- c(errors, msg))
  expect_null(conn)
  expect_true(any(grepl("RPostgres|failed", errors)))
})

test_that("db_connect notifies when package missing for mysql", {
  skip_if(requireNamespace("RMariaDB", quietly = TRUE), "RMariaDB is installed")
  errors <- character(0)
  conn <- db_connect("mysql", host = "localhost",
                     notify_fn = function(msg) errors <<- c(errors, msg))
  expect_null(conn)
  expect_true(any(grepl("RMariaDB|failed", errors)))
})

# ── SQLite round-trip test ───────────────────────────────────

test_that("db_connect works with SQLite", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  expect_false(is.null(conn))

  # Write a test table
  DBI::dbWriteTable(conn, "test_table", data.frame(
    id = 1:5,
    value = c("a", "b", "c", "d", "e")
  ))

  # Verify we can read it
  tables <- DBI::dbListTables(conn)
  expect_true("test_table" %in% tables)

  db_close(conn)
})

test_that("db_introspect lists SQLite tables", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  DBI::dbWriteTable(conn, "customers", data.frame(id = 1:3, name = letters[1:3]))
  DBI::dbWriteTable(conn, "orders", data.frame(id = 1:2, cust_id = c(1, 2)))

  meta <- db_introspect(conn, "sqlite")
  expect_true("customers" %in% meta$tables)
  expect_true("orders" %in% meta$tables)

  db_close(conn)
})

test_that("db_load_table reads from SQLite", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  DBI::dbWriteTable(conn, "items", data.frame(
    id = 1:10,
    name = paste0("item_", 1:10)
  ))

  df <- db_load_table(conn, "items")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 10)
  expect_true("id" %in% names(df))
  expect_true("name" %in% names(df))

  db_close(conn)
})

test_that("db_load_table respects limit parameter", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  DBI::dbWriteTable(conn, "big_table", data.frame(
    id = 1:100,
    value = rnorm(100)
  ))

  df <- db_load_table(conn, "big_table", limit = 5)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 5)

  db_close(conn)
})

# ── db_close ─────────────────────────────────────────────────

test_that("db_close handles NULL connection", {
  # Should not error
  expect_silent(db_close(NULL))
})

test_that("db_close handles already-closed connection", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  db_close(conn)
  # Closing again should not error
  expect_silent(db_close(conn))
})

# ── db_introspect edge cases ─────────────────────────────────

test_that("db_introspect returns correct structure", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  meta <- db_introspect(conn, "sqlite")

  expect_true("tables" %in% names(meta))
  expect_true("pks" %in% names(meta))
  expect_true("fks" %in% names(meta))
  expect_true(is.character(meta$tables))
  expect_true(is.list(meta$pks))
  expect_true(is.list(meta$fks))

  db_close(conn)
})

test_that("db_introspect handles empty database", {
  skip_if_not_installed("RSQLite")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  conn <- db_connect("sqlite", path = tmp_db)
  meta <- db_introspect(conn, "sqlite")

  expect_equal(length(meta$tables), 0)

  db_close(conn)
})
