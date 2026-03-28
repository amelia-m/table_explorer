# ============================================================
# Tests for inference.R — PK/FK Detection Engine
# ============================================================

# ── Name helpers ─────────────────────────────────────────────

test_that("clean_name lowercases and underscores", {
  expect_equal(clean_name("CustomerID"), "customer_id")
  expect_equal(clean_name("order items"), "order_items")
  expect_equal(clean_name("simple"), "simple")
})

test_that("id_stem strips _id suffix", {
  expect_equal(id_stem("customer_id"), "customer")
  expect_equal(id_stem("product_id"), "product")
  expect_equal(id_stem("name"), "name")
  expect_equal(id_stem("id"), "id")
})

test_that("is_pk_name detects pk-like columns", {
  expect_true(is_pk_name("id", "customers"))
  expect_true(is_pk_name("customer_id", "customer"))
  expect_true(is_pk_name("customers_id", "customers"))
  expect_false(is_pk_name("name", "customers"))
  expect_false(is_pk_name("email", "customers"))
})

test_that("is_fk_for detects fk-like columns for target table", {
  expect_true(is_fk_for("customer_id", "customer"))
  expect_true(is_fk_for("order_id", "order"))
  expect_false(is_fk_for("name", "customer"))
  expect_false(is_fk_for("customer_id", "product"))
})

# ── Type classification ──────────────────────────────────────

test_that("col_dtype_class classifies types correctly", {
  expect_equal(col_dtype_class(1:10), "numeric")
  expect_equal(col_dtype_class(c(1.5, 2.5)), "numeric")
  expect_equal(col_dtype_class(c("a", "b")), "string")
  expect_equal(col_dtype_class(as.Date("2024-01-01")), "datetime")
  expect_equal(col_dtype_class(Sys.time()), "datetime")
  expect_equal(col_dtype_class(factor(c("a", "b"))), "string")
})

# ── Format fingerprint ───────────────────────────────────────

test_that("format_fingerprint detects email format", {
  emails <- c("alice@example.com", "bob@example.com", "carol@test.org")
  expect_equal(format_fingerprint(emails), "email")
})

test_that("format_fingerprint detects uuid format", {
  uuids <- c(
    "550e8400-e29b-41d4-a716-446655440000",
    "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
    "f47ac10b-58cc-4372-a567-0e02b2c3d479"
  )
  expect_equal(format_fingerprint(uuids), "uuid")
})

test_that("format_fingerprint detects iso_date format", {
  dates <- c("2024-01-01", "2024-06-15", "2023-12-31")
  expect_equal(format_fingerprint(dates), "iso_date")
})

test_that("format_fingerprint detects int_code format", {
  codes <- c("101", "202", "303", "404")
  expect_equal(format_fingerprint(codes), "int_code")
})

test_that("format_fingerprint returns NULL for mixed data", {
  mixed <- c("hello", "123", "alice@test.com", "2024-01-01")
  expect_null(format_fingerprint(mixed))
})

test_that("format_fingerprint handles all-NA gracefully", {
  expect_null(format_fingerprint(c(NA, NA, NA)))
})

# ── Value overlap ────────────────────────────────────────────

test_that("value_overlap computes correct overlap ratio", {
  expect_equal(value_overlap(c(1, 2, 3), c(1, 2, 3, 4, 5)), 1.0)
  expect_equal(value_overlap(c(1, 2, 3, 4), c(1, 2)), 0.5)
  expect_equal(value_overlap(c("a", "b"), c("c", "d")), 0.0)
})

test_that("value_overlap handles NAs", {
  expect_equal(value_overlap(c(1, NA, 3), c(1, 3)), 1.0)
})

test_that("value_overlap returns 0 for empty input", {
  expect_equal(value_overlap(c(NA, NA), c(1, 2)), 0.0)
})

# ── Distribution similarity ─────────────────────────────────

test_that("distribution_similarity returns 1 for identical distributions", {
  v <- c("a", "a", "b", "b", "c")
  sim <- distribution_similarity(v, v)
  expect_equal(sim, 1.0, tolerance = 0.001)
})

test_that("distribution_similarity returns 0 for no-overlap", {
  sim <- distribution_similarity(c("a", "a"), c("x", "x"))
  expect_equal(sim, 0.0)
})

test_that("distribution_similarity handles empty input", {
  expect_equal(distribution_similarity(c(NA, NA), c("a")), 0.0)
})

# ── Null pattern correlation ─────────────────────────────────

test_that("null_pattern_correlation returns 0 for different-length dfs", {
  df1 <- data.frame(a = c(1, 2))
  df2 <- data.frame(b = c(1, 2, 3))
  expect_equal(null_pattern_correlation(df1, "a", df2, "b"), 0.0)
})

test_that("null_pattern_correlation returns 0 with no NAs", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  expect_equal(null_pattern_correlation(df, "a", df, "b"), 0.0)
})

test_that("null_pattern_correlation detects correlated nulls", {
  df <- data.frame(
    a = c(1, NA, 3, NA, 5, NA, 7, NA, 9, NA),
    b = c(10, NA, 30, NA, 50, NA, 70, NA, 90, NA)
  )
  r <- null_pattern_correlation(df, "a", df, "b")
  expect_gt(r, 0.9)
})

# ── Primary key detection ────────────────────────────────────

test_that("detect_pks finds id column by naming", {
  df <- data.frame(id = 1:5, name = c("a", "b", "c", "d", "e"))
  pks <- detect_pks(df, "customers", method = "naming")
  expect_true("id" %in% pks)
})

test_that("detect_pks finds table_id column by naming", {
  df <- data.frame(customer_id = 1:5, name = letters[1:5])
  pks <- detect_pks(df, "customers", method = "naming")
  expect_true("customer_id" %in% pks)
})

test_that("detect_pks finds unique columns by content", {
  df <- data.frame(
    code = c("A1", "A2", "A3"),
    value = c(10, 10, 30)
  )
  pks <- detect_pks(df, "items", method = "both")
  expect_true("code" %in% pks)
  expect_false("value" %in% pks)
})

test_that("detect_pks returns empty for no candidates", {
  df <- data.frame(value = c(1, 1, 1))
  pks <- detect_pks(df, "items", method = "both")
  expect_length(pks, 0)
})

# ── Score candidate ──────────────────────────────────────────

test_that("score_candidate detects exact FK naming", {
  df1 <- data.frame(customer_id = c(1, 2, 3))
  df2 <- data.frame(id = c(1, 2, 3))
  flags <- list(
    naming = TRUE, value_overlap = FALSE, cardinality = FALSE,
    format = FALSE, distribution = FALSE, null_pattern = FALSE
  )
  result <- score_candidate("orders", "customer_id", df1, "customer", "id", df2, flags)
  expect_false(is.null(result))
  expect_equal(result$detected_by, "naming")
  expect_equal(result$confidence, "high")
  expect_equal(result$score, 1.0)
})

test_that("score_candidate returns NULL for type mismatch", {
  df1 <- data.frame(customer_id = c("a", "b", "c"))
  df2 <- data.frame(id = c(1, 2, 3))
  flags <- list(
    naming = TRUE, value_overlap = TRUE, cardinality = TRUE,
    format = FALSE, distribution = FALSE, null_pattern = FALSE
  )
  result <- score_candidate("orders", "customer_id", df1, "customer", "id", df2, flags)
  expect_null(result)
})

test_that("score_candidate returns NULL with no signals triggered", {
  df1 <- data.frame(foo = c(1, 2, 3))
  df2 <- data.frame(bar = c(4, 5, 6))
  flags <- list(
    naming = TRUE, value_overlap = TRUE, cardinality = TRUE,
    format = FALSE, distribution = FALSE, null_pattern = FALSE
  )
  result <- score_candidate("t1", "foo", df1, "t2", "bar", df2, flags)
  expect_null(result)
})

test_that("score_candidate detects high value overlap", {
  ids <- c(1, 2, 3, 4, 5)
  df1 <- data.frame(cust_ref = ids)
  df2 <- data.frame(pk = ids)
  flags <- list(
    naming = FALSE, value_overlap = TRUE, cardinality = TRUE,
    format = FALSE, distribution = FALSE, null_pattern = FALSE
  )
  result <- score_candidate("orders", "cust_ref", df1, "customers", "pk", df2, flags)
  expect_false(is.null(result))
  expect_true(result$score >= 0.85)
  expect_equal(result$confidence, "high")
})

test_that("score_candidate noisy-OR aggregation combines signals", {
  # Create data that triggers multiple signals
  ids <- c(1, 2, 3, 4, 5)
  df1 <- data.frame(customer_id = ids)
  df2 <- data.frame(id = ids)
  flags <- list(
    naming = TRUE, value_overlap = TRUE, cardinality = TRUE,
    format = TRUE, distribution = TRUE, null_pattern = FALSE
  )
  result <- score_candidate("orders", "customer_id", df1, "customer", "id", df2, flags)
  expect_false(is.null(result))
  # Naming exact alone = 1.0, so score should be 1.0
  expect_equal(result$score, 1.0)
  # Should have multiple signals triggered
  expect_true(length(result$signals) >= 1)
})

# ── Full FK detection pipeline ───────────────────────────────

test_that("detect_fks returns empty for manual method", {
  tables <- list(a = data.frame(id = 1), b = data.frame(id = 2))
  expect_equal(detect_fks(tables, method = "manual"), list())
})

test_that("detect_fks returns empty for single table", {
  tables <- list(only = data.frame(id = 1:3, name = letters[1:3]))
  expect_equal(detect_fks(tables, method = "both"), list())
})

test_that("detect_fks detects naming-based relationship", {
  customers <- data.frame(
    customer_id = 1:5,
    name = letters[1:5]
  )
  orders <- data.frame(
    order_id = 1:5,
    customer_id = c(1, 2, 1, 3, 2),
    amount = c(10, 20, 30, 40, 50)
  )
  tables <- list(customers = customers, orders = orders)
  rels <- detect_fks(tables, method = "naming", min_confidence = "low")
  # Should detect orders.customer_id -> customers
  fk_cols <- vapply(rels, `[[`, character(1), "from_col")
  expect_true("customer_id" %in% fk_cols)
})

test_that("detect_fks respects min_confidence filter", {
  # Create a scenario where only weak signals exist
  df1 <- data.frame(ref = c(1, 2, 3, 4, 5))
  df2 <- data.frame(pk = c(1, 2, 3, 4, 5))
  tables <- list(source = df1, target = df2)

  rels_low <- detect_fks(tables, method = "content", min_confidence = "low")
  rels_high <- detect_fks(tables, method = "content", min_confidence = "high")
  # High filter should be equal or more restrictive
  expect_true(length(rels_high) <= length(rels_low))
})

test_that("detect_fks with sample data detects expected relationships", {
  skip_if_not(
    dir.exists(sample_data_dir),
    "sample_data directory not found"
  )

  customers <- read.csv(file.path(sample_data_dir, "customers.csv"),
                        stringsAsFactors = FALSE, check.names = FALSE)
  orders <- read.csv(file.path(sample_data_dir, "orders.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)
  products <- read.csv(file.path(sample_data_dir, "products.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  order_items <- read.csv(file.path(sample_data_dir, "order_items.csv"),
                          stringsAsFactors = FALSE, check.names = FALSE)

  # Clean names like the app does
  customers <- janitor::clean_names(customers)
  orders <- janitor::clean_names(orders)
  products <- janitor::clean_names(products)
  order_items <- janitor::clean_names(order_items)

  tables <- list(
    customers = customers,
    orders = orders,
    products = products,
    order_items = order_items
  )

  rels <- detect_fks(tables, method = "both", min_confidence = "low")
  expect_true(length(rels) > 0)

  # Each result should have required fields
  for (r in rels) {
    expect_true(!is.null(r$from_table))
    expect_true(!is.null(r$from_col))
    expect_true(!is.null(r$to_table))
    expect_true(!is.null(r$detected_by))
    expect_true(!is.null(r$confidence))
    expect_true(!is.null(r$score))
    expect_true(r$confidence %in% c("low", "medium", "high"))
    expect_true(r$score >= 0 && r$score <= 1)
  }
})

test_that("detect_fks results are sorted by confidence then score", {
  skip_if_not(dir.exists(sample_data_dir), "sample_data directory not found")

  customers <- janitor::clean_names(read.csv(
    file.path(sample_data_dir, "customers.csv"), stringsAsFactors = FALSE))
  orders <- janitor::clean_names(read.csv(
    file.path(sample_data_dir, "orders.csv"), stringsAsFactors = FALSE))

  tables <- list(customers = customers, orders = orders)
  rels <- detect_fks(tables, method = "both", min_confidence = "low")

  if (length(rels) > 1) {
    conf_rank <- c(low = 0, medium = 1, high = 2)
    for (i in seq_len(length(rels) - 1)) {
      r1 <- rels[[i]]
      r2 <- rels[[i + 1]]
      rank1 <- conf_rank[[r1$confidence]]
      rank2 <- conf_rank[[r2$confidence]]
      # Either higher confidence or same confidence with >= score
      expect_true(rank1 > rank2 || (rank1 == rank2 && r1$score >= r2$score))
    }
  }
})

# ── Complexity estimator ──────────────────────────────────────

test_that("estimate_scan_complexity returns correct structure", {
  tables <- list(
    a = data.frame(id = 1:10, name = letters[1:10]),
    b = data.frame(id = 1:5, a_id = c(1, 2, 3, 4, 5))
  )
  est <- estimate_scan_complexity(tables)
  expect_true(is.list(est))
  expect_true(all(c("n_tables", "total_cols", "total_rows", "max_rows",
                     "est_pairs", "est_time_sec", "tier") %in% names(est)))
  expect_equal(est$n_tables, 2)
  expect_equal(est$total_cols, 4)
  expect_equal(est$tier, "fast")
})

test_that("estimate_scan_complexity flags large schemas as slow", {
  # Simulate 30 tables x 50 columns x 1000 rows each
  tables <- lapply(1:30, function(i) {
    df <- as.data.frame(matrix(sample(1000, 50 * 1000, replace = TRUE), ncol = 50))
    names(df) <- paste0("col_", seq_len(50))
    df
  })
  names(tables) <- paste0("table_", seq_along(tables))
  est <- estimate_scan_complexity(tables)
  expect_true(est$tier %in% c("moderate", "slow"))
  expect_gt(est$est_pairs, 2000)
})

test_that("is_fk_candidate rejects boolean and low-cardinality columns", {
  expect_false(is_fk_candidate(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
                                  FALSE, TRUE, FALSE, FALSE, TRUE), "is_active"))
  expect_false(is_fk_candidate(c(NA, NA, NA, NA, NA), "empty"))
})

test_that("is_fk_candidate accepts id-like columns", {
  expect_true(is_fk_candidate(1:100, "customer_id"))
  expect_true(is_fk_candidate(c("A1", "A2", "A3", "A4"), "code"))
})

test_that("is_fk_candidate rejects long free-text columns", {
  long_text <- paste(rep("lorem ipsum dolor sit amet consectetur adipiscing", 5), collapse = " ")
  expect_false(is_fk_candidate(rep(long_text, 20), "description"))
})

# ── Constants and maps ───────────────────────────────────────

test_that("weight_map has all expected signal keys", {
  expected_keys <- c(
    "naming_exact", "cardinality_match", "overlap_high", "name_sim",
    "overlap_medium", "dist_high", "format_match", "dist_med",
    "name_sim_weak", "null_corr"
  )
  for (k in expected_keys) {
    expect_true(k %in% names(weight_map), info = paste("Missing key:", k))
  }
})

test_that("label_map covers all weight_map keys", {
  for (k in names(weight_map)) {
    expect_true(k %in% names(label_map), info = paste("Missing label for:", k))
  }
})

test_that("all weight_map values are in [0, 1]", {
  for (w in weight_map) {
    expect_true(w >= 0 && w <= 1)
  }
})

test_that("format_patterns list has expected entries", {
  pattern_names <- vapply(format_patterns, `[[`, character(1), "name")
  expect_true("uuid" %in% pattern_names)
  expect_true("email" %in% pattern_names)
  expect_true("iso_date" %in% pattern_names)
  expect_true("int_code" %in% pattern_names)
})
