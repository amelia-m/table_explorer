# ============================================================
# inference.R — PK/FK Detection Engine (7-Signal)
# Table Relationship Explorer
# ============================================================

library(janitor)

# ── Confidence thresholds ────────────────────────────────────

overlap_high   <- 0.98
overlap_medium <- 0.80
name_sim_high  <- 0.85
name_sim_med   <- 0.72
dist_sim_high  <- 0.90
dist_sim_med   <- 0.75

# ── Signal weight map for noisy-OR aggregation ───────────────

weight_map <- c(
  naming_exact     = 1.00,
  cardinality_match = 0.95,
  overlap_high     = 0.90,
  name_sim         = 0.60,
  overlap_medium   = 0.55,
  dist_high        = 0.50,
  format_match     = 0.40,
  dist_med         = 0.30,
  name_sim_weak    = 0.25,
  null_corr        = 0.20
)

# ── Signal -> human-readable label map ───────────────────────

label_map <- c(
  naming_exact     = "naming",
  name_sim         = "name_similarity",
  name_sim_weak    = "name_similarity",
  overlap_high     = "value_overlap",
  overlap_medium   = "value_overlap",
  cardinality_match = "cardinality",
  format_match     = "format",
  dist_high        = "distribution",
  dist_med         = "distribution",
  null_corr        = "null_pattern"
)

# ── Format fingerprint patterns ──────────────────────────────

format_patterns <- list(
  list(name = "uuid",       pattern = "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"),
  list(name = "email",      pattern = "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"),
  list(name = "iso_ts",     pattern = "^\\d{4}-\\d{2}-\\d{2}[ T]\\d{2}:\\d{2}"),
  list(name = "iso_date",   pattern = "^\\d{4}-\\d{2}-\\d{2}$"),
  list(name = "zip_us",     pattern = "^\\d{5}(-\\d{4})?$"),
  list(name = "phone",      pattern = "^\\+?[\\d\\s\\-().]{7,15}$"),
  list(name = "hex_color",  pattern = "^#[0-9a-fA-F]{3,6}$"),
  list(name = "int_code",   pattern = "^\\d{1,6}$"),
  list(name = "alpha_code", pattern = "^[A-Z]{2,4}$")
)

# ── Name helpers ─────────────────────────────────────────────

clean_name <- function(name) janitor::make_clean_names(name)

id_stem <- function(col_clean) sub("_id$", "", col_clean)

is_pk_name <- function(col_clean, tname_clean) {
  col_clean == "id" ||
    col_clean == paste0(tname_clean, "_id") ||
    (grepl("_id$", col_clean) && startsWith(tname_clean, id_stem(col_clean)))
}

is_fk_for <- function(col_clean, t2clean) {
  col_clean == paste0(t2clean, "_id") ||
    (grepl("_id$", col_clean) && startsWith(t2clean, id_stem(col_clean)))
}

# ── Type classification ──────────────────────────────────────

col_dtype_class <- function(x) {
  if (is.numeric(x)) return("numeric")
  if (inherits(x, c("Date", "POSIXt", "POSIXct", "POSIXlt"))) return("datetime")
  "string"
}

# ── Jaro-Winkler similarity ──────────────────────────────────

jaro_winkler_sim <- function(s1, s2) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return(0.0)
  }
  1 - stringdist::stringdist(s1, s2, method = "jw", p = 0.1)
}

# ── Format fingerprint ───────────────────────────────────────

format_fingerprint <- function(col, sample_size = 200) {
  vals <- na.omit(col)
  if (length(vals) == 0) return(NULL)
  vals <- as.character(vals)
  if (length(vals) > sample_size) {
    set.seed(42)
    vals <- sample(vals, sample_size)
  }
  for (fp in format_patterns) {
    hits <- sum(grepl(fp$pattern, vals, ignore.case = TRUE, perl = TRUE))
    if (hits / length(vals) >= 0.80) {
      return(fp$name)
    }
  }
  NULL
}

# ── Value overlap ────────────────────────────────────────────

value_overlap <- function(v1, v2, sample_cap = 5000) {
  s1 <- unique(na.omit(as.character(v1)))
  s2 <- unique(na.omit(as.character(v2)))
  # Sample down for performance on large value sets
  if (length(s1) > sample_cap) {
    set.seed(42)
    s1 <- s1[sample(length(s1), sample_cap)]
  }
  if (length(s2) > sample_cap * 2) {
    s2_set <- new.env(parent = emptyenv())
    for (v in s2[seq_len(min(length(s2), sample_cap * 2))]) assign(v, TRUE, envir = s2_set)
    if (length(s1) == 0) return(0.0)
    hits <- sum(vapply(s1, function(v) exists(v, envir = s2_set), logical(1)))
    return(hits / length(s1))
  }
  if (length(s1) == 0) return(0.0)
  length(intersect(s1, s2)) / length(s1)
}

# ── Distribution similarity (cosine) ────────────────────────

distribution_similarity <- function(v1, v2, sample_cap = 5000) {
  c1 <- as.character(na.omit(v1))
  c2 <- as.character(na.omit(v2))
  # Sample down for large columns
  if (length(c1) > sample_cap) { set.seed(42); c1 <- c1[sample(length(c1), sample_cap)] }
  if (length(c2) > sample_cap) { set.seed(43); c2 <- c2[sample(length(c2), sample_cap)] }

  t1 <- table(c1)
  t2 <- table(c2)
  if (length(t1) == 0 || length(t2) == 0) return(0.0)

  # Use only shared vocabulary for cosine — much cheaper than full union
  shared <- intersect(names(t1), names(t2))
  if (length(shared) == 0) return(0.0)

  a <- as.numeric(t1[shared])
  b <- as.numeric(t2[shared])

  dot <- sum(a * b)
  norm_a <- sqrt(sum(as.numeric(t1)^2))
  norm_b <- sqrt(sum(as.numeric(t2)^2))
  if (norm_a == 0 || norm_b == 0) return(0.0)
  dot / (norm_a * norm_b)
}

# ── Null pattern correlation ─────────────────────────────────

null_pattern_correlation <- function(df1, col1, df2, col2) {
  if (nrow(df1) != nrow(df2)) return(0.0)
  mask1 <- as.numeric(is.na(df1[[col1]]))
  mask2 <- as.numeric(is.na(df2[[col2]]))
  if (sd(mask1) == 0 || sd(mask2) == 0) return(0.0)
  tryCatch(
    cor(mask1, mask2),
    error = function(e) 0.0
  )
}

# ── Primary key detection ────────────────────────────────────

detect_pks <- function(df, table_name, method = "both") {
  cols <- names(df)
  candidates <- character(0)
  n <- nrow(df)

  if (method %in% c("naming", "both", "content", "all")) {
    tname <- clean_name(table_name)
    cols_clean <- clean_name(cols)
    hits <- cols[vapply(
      cols_clean,
      is_pk_name,
      logical(1),
      tname_clean = tname
    )]
    candidates <- union(candidates, hits)
  }

  if (method %in% c("uniqueness", "both", "content", "all") && n > 0) {
    hits <- cols[vapply(
      cols,
      function(c) {
        v <- df[[c]]
        !anyNA(v) && length(unique(v)) == n
      },
      logical(1)
    )]
    candidates <- union(candidates, hits)
  }

  candidates
}

# ── Score a single candidate pair ────────────────────────────

score_candidate <- function(t1, col1, df1, t2, col2, df2, enable_flags) {
  signals <- list()
  reasons <- character(0)

  # 1. Naming conventions
  if (isTRUE(enable_flags[["naming"]])) {
    c1 <- clean_name(col1)
    t2c <- clean_name(t2)
    if (is_fk_for(c1, t2c)) {
      signals[["naming_exact"]] <- 1.0
      reasons <- c(reasons, "exact FK naming")
    } else {
      stem1 <- sub("_(id|key|code|num|no)$", "", c1)
      stem2 <- sub("_(id|key|code|num|no)$", "", clean_name(col2))
      sim <- jaro_winkler_sim(stem1, stem2)
      if (sim >= name_sim_high) {
        signals[["name_sim"]] <- sim
        reasons <- c(reasons, sprintf("name similarity %.2f", sim))
      } else if (sim >= name_sim_med) {
        signals[["name_sim_weak"]] <- sim
        reasons <- c(reasons, sprintf("weak name similarity %.2f", sim))
      }
    }
  }

  # 2. Type compatibility guard
  dtype1 <- col_dtype_class(df1[[col1]])
  dtype2 <- col_dtype_class(df2[[col2]])
  if (dtype1 != dtype2) return(NULL)

  n1 <- nrow(df1)
  n2 <- nrow(df2)

  # 3. Value overlap
  if (isTRUE(enable_flags[["value_overlap"]]) && n1 > 0 && n2 > 0) {
    ov <- value_overlap(df1[[col1]], df2[[col2]])
    if (ov >= overlap_high) {
      signals[["overlap_high"]] <- ov
      reasons <- c(reasons, sprintf("value overlap %.0f%%", ov * 100))
    } else if (ov >= overlap_medium) {
      signals[["overlap_medium"]] <- ov
      reasons <- c(reasons, sprintf("partial overlap %.0f%%", ov * 100))
    }
  }

  # 4. Exact cardinality match
  if (isTRUE(enable_flags[["cardinality"]]) && n1 > 0 && n2 > 0) {
    u1 <- unique(na.omit(as.character(df1[[col1]])))
    u2 <- unique(na.omit(as.character(df2[[col2]])))
    if (length(u1) > 0 && length(u2) > 0 && setequal(u1, u2)) {
      signals[["cardinality_match"]] <- 1.0
      reasons <- c(reasons, "identical value sets")
    }
  }

  # 5. Format fingerprint
  if (isTRUE(enable_flags[["format"]]) && n1 > 0 && n2 > 0) {
    fmt1 <- format_fingerprint(df1[[col1]])
    fmt2 <- format_fingerprint(df2[[col2]])
    if (!is.null(fmt1) && !is.null(fmt2) && fmt1 == fmt2) {
      signals[["format_match"]] <- 0.6
      reasons <- c(reasons, sprintf("shared format [%s]", fmt1))
    }
  }

  # 6. Distribution similarity
  if (isTRUE(enable_flags[["distribution"]]) && n1 > 0 && n2 > 0) {
    dist_sim <- distribution_similarity(df1[[col1]], df2[[col2]])
    if (dist_sim >= dist_sim_high) {
      signals[["dist_high"]] <- dist_sim
      reasons <- c(reasons, sprintf("distribution similarity %.2f", dist_sim))
    } else if (dist_sim >= dist_sim_med) {
      signals[["dist_med"]] <- dist_sim
      reasons <- c(reasons, sprintf("weak distribution similarity %.2f", dist_sim))
    }
  }

  # 7. Null pattern correlation
  if (isTRUE(enable_flags[["null_pattern"]]) && n1 == n2) {
    null_r <- null_pattern_correlation(df1, col1, df2, col2)
    if (null_r >= 0.80) {
      signals[["null_corr"]] <- null_r
      reasons <- c(reasons, sprintf("null pattern corr %.2f", null_r))
    }
  }

  if (length(signals) == 0) return(NULL)

  # Composite confidence score (noisy-OR)
  weights <- vapply(
    names(signals),
    function(k) if (k %in% names(weight_map)) weight_map[[k]] else 0.1,
    numeric(1)
  )
  score <- 1 - prod(1 - weights)
  score <- round(min(score, 1.0), 3)

  # Primary detected_by label (highest-weight signal)
  top_signal <- names(signals)[which.max(weights)]
  detected_by <- if (top_signal %in% names(label_map)) label_map[[top_signal]] else "content"

  # Confidence tier
  confidence <- if (score >= 0.85) "high" else if (score >= 0.55) "medium" else "low"

  list(
    signals     = signals,
    reasons     = reasons,
    score       = score,
    confidence  = confidence,
    detected_by = detected_by
  )
}

# ── Complexity estimator ───────────────────────────────────────

estimate_scan_complexity <- function(tables) {
  n_tables <- length(tables)
  total_cols <- sum(vapply(tables, ncol, integer(1)))
  total_rows <- sum(vapply(tables, nrow, integer(1)))
  max_rows <- if (n_tables > 0) max(vapply(tables, nrow, integer(1))) else 0

  # Estimate pairs: each non-PK col in t1 x each PK-like col in t2
  # Rough: ~40% of cols are non-PK, ~15% are PK-like targets
  est_source_cols <- total_cols * 0.40
  est_target_cols_per_table <- max(total_cols / max(n_tables, 1) * 0.15, 1)
  est_pairs <- est_source_cols * (n_tables - 1) * est_target_cols_per_table

  # Cost per pair: ~1ms for naming-only, ~5ms for content signals on small data,
  # ~20ms if rows are large
  cost_per_pair_ms <- if (max_rows > 5000) 20 else if (max_rows > 500) 5 else 1
  est_time_sec <- (est_pairs * cost_per_pair_ms) / 1000

  tier <- if (est_pairs < 2000 || est_time_sec < 5) {
    "fast"
  } else if (est_pairs < 15000 || est_time_sec < 30) {
    "moderate"
  } else {
    "slow"
  }

  list(
    n_tables    = n_tables,
    total_cols  = total_cols,
    total_rows  = total_rows,
    max_rows    = max_rows,
    est_pairs   = round(est_pairs),
    est_time_sec = round(est_time_sec, 1),
    tier        = tier
  )
}

# ── Column pre-screening heuristic ─────────────────────────────
# Skip columns that are very unlikely to be FK/PK candidates
# to avoid the expensive score_candidate call entirely.

is_fk_candidate <- function(col, col_name) {
  # Fast heuristic checks — reject columns that almost never form relationships
  n <- length(col)
  if (n == 0) return(FALSE)

  # Boolean / low-cardinality columns are not FKs
  n_unique <- length(unique(na.omit(col)))
  if (n_unique <= 2 && n > 10) return(FALSE)

  # All-NA columns
  if (n_unique == 0) return(FALSE)

  # Very high cardinality text (long free-text, descriptions, notes)
  if (is.character(col)) {
    sample_vals <- head(na.omit(col), 50)
    if (length(sample_vals) > 0) {
      median_len <- median(nchar(sample_vals))
      if (median_len > 80) return(FALSE)
    }
  }

  TRUE
}

# ── Foreign key detection (multi-signal) ─────────────────────

detect_fks <- function(tables, method = "both", min_confidence = "medium",
                       enable_flags = NULL) {
  if (method == "manual" || length(tables) < 2) return(list())

  conf_rank <- c(low = 0L, medium = 1L, high = 2L)
  min_rank <- conf_rank[[min_confidence]]

  # Build enable_flags from method if not provided
  if (is.null(enable_flags)) {
    use_naming  <- method %in% c("naming", "both", "all")
    use_content <- method %in% c("content", "both", "all", "uniqueness")
    enable_flags <- list(
      naming       = use_naming,
      value_overlap = use_content,
      cardinality  = use_content,
      format       = use_content,
      distribution = use_content,
      null_pattern = use_content
    )
  }

  tnames <- names(tables)

  # Pre-compute PK columns per table (optimized: stop early, skip wide text)
  pk_map <- lapply(tnames, function(t) {
    df <- tables[[t]]
    n <- nrow(df)
    if (n == 0) return(character(0))
    pks <- character(0)
    for (c in names(df)) {
      v <- df[[c]]
      # Quick reject: skip long-text, logical, Date columns as PK candidates
      if (is.logical(v) || inherits(v, c("Date", "POSIXt"))) next
      if (is.character(v) && length(v) > 0 && median(nchar(head(na.omit(v), 20))) > 60) next
      if (!anyNA(v) && length(unique(v)) == n) {
        pks <- c(pks, c)
      }
    }
    pks
  })
  names(pk_map) <- tnames

  # Pre-compute FK-candidate columns per table (skip non-FK-like columns)
  fk_candidates <- lapply(tnames, function(t) {
    df <- tables[[t]]
    cols <- setdiff(names(df), pk_map[[t]])
    cols[vapply(cols, function(c) is_fk_candidate(df[[c]], c), logical(1))]
  })
  names(fk_candidates) <- tnames

  # Pre-compute format fingerprints to avoid recomputation
  fingerprint_cache <- new.env(parent = emptyenv())

  get_fingerprint <- function(tname, cname, col) {
    key <- paste(tname, cname, sep = "|")
    if (exists(key, envir = fingerprint_cache)) {
      return(get(key, envir = fingerprint_cache))
    }
    fp <- format_fingerprint(col)
    assign(key, fp, envir = fingerprint_cache)
    fp
  }

  results <- list()
  seen <- new.env(parent = emptyenv())  # O(1) lookup vs character vector
  best_scores <- new.env(parent = emptyenv())

  # Cap total pair evaluations to prevent runaway on huge schemas
  max_pairs <- 50000L
  pair_count <- 0L

  for (t1 in tnames) {
    df1 <- tables[[t1]]
    if (nrow(df1) == 0 && !isTRUE(enable_flags[["naming"]])) next

    source_cols <- fk_candidates[[t1]]

    for (col1 in source_cols) {
      for (t2 in tnames) {
        if (t2 == t1) next

        rel_key <- paste(t1, col1, t2, sep = "|")
        if (exists(rel_key, envir = seen)) next

        df2 <- tables[[t2]]
        target_cols <- if (length(pk_map[[t2]]) > 0) pk_map[[t2]] else fk_candidates[[t2]]
        if (length(target_cols) == 0) next

        best_result <- NULL
        best_to_col <- NULL

        for (col2 in target_cols) {
          pair_count <- pair_count + 1L
          if (pair_count > max_pairs) break

          result <- score_candidate(t1, col1, df1, t2, col2, df2, enable_flags)
          if (is.null(result)) next
          if (is.null(best_result) || result$score > best_result$score) {
            best_result <- result
            best_to_col <- col2
          }
        }

        if (pair_count > max_pairs) break
        if (is.null(best_result)) next
        if (conf_rank[[best_result$confidence]] < min_rank) next

        # Deduplicate: keep best target per source column
        col_key <- paste(t1, col1, sep = "|")
        prev_score <- if (exists(col_key, envir = best_scores)) {
          get(col_key, envir = best_scores)
        } else {
          NULL
        }
        if (!is.null(prev_score) && prev_score > best_result$score + 0.05) next
        assign(col_key, best_result$score, envir = best_scores)

        assign(rel_key, TRUE, envir = seen)
        results[[length(results) + 1]] <- list(
          from_table  = t1,
          from_col    = col1,
          to_table    = t2,
          to_col      = best_to_col,
          detected_by = best_result$detected_by,
          confidence  = best_result$confidence,
          score       = best_result$score,
          reasons     = best_result$reasons,
          signals     = best_result$signals
        )
      }
      if (pair_count > max_pairs) break
    }
    if (pair_count > max_pairs) break
  }

  # Sort by confidence desc, score desc
  if (length(results) > 0) {
    order_idx <- order(
      -vapply(results, function(r) conf_rank[[r$confidence]], integer(1)),
      -vapply(results, function(r) r$score, numeric(1))
    )
    results <- results[order_idx]
  }

  results
}
