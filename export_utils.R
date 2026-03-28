# ============================================================
# export_utils.R — Export Format Generators
# Table Relationship Explorer
# ============================================================

# ── dbt schema.yml generator ──────────────────────────────────

generate_dbt_yaml <- function(tables, rels, pks) {
  lines <- c("version: 2", "", "models:")

  for (tname in names(tables)) {
    df <- tables[[tname]]
    lines <- c(lines, paste0("  - name: ", tname))
    lines <- c(lines, "    columns:")

    pk_cols <- pks[[tname]]
    t_rels <- Filter(function(r) r$from_table == tname, rels)
    fk_map <- setNames(
      lapply(t_rels, function(r) r),
      vapply(t_rels, `[[`, character(1), "from_col")
    )

    for (col in names(df)) {
      lines <- c(lines, paste0("      - name: ", col))

      tests <- character(0)
      if (col %in% pk_cols) {
        tests <- c(tests, "          - unique", "          - not_null")
      }
      if (col %in% names(fk_map)) {
        r <- fk_map[[col]]
        to_col <- if (!is.null(r$to_col) && !is.na(r$to_col)) r$to_col else col
        tests <- c(tests, paste0(
          "          - relationships:\n",
          "              to: ref('", r$to_table, "')\n",
          "              field: ", to_col
        ))
      }
      if (length(tests) > 0) {
        lines <- c(lines, "        tests:", tests)
      }
    }
  }

  paste(lines, collapse = "\n")
}

# ── Mermaid ERD generator ─────────────────────────────────────

generate_mermaid_erd <- function(tables, rels, pks) {
  lines <- c("erDiagram")

  # Table definitions
  for (tname in names(tables)) {
    df <- tables[[tname]]
    pk_cols <- pks[[tname]]
    lines <- c(lines, paste0("    ", tname, " {"))
    for (col in names(df)) {
      dtype <- if (is.numeric(df[[col]])) {
        "int"
      } else if (inherits(df[[col]], c("Date", "POSIXt"))) {
        "date"
      } else {
        "string"
      }
      pk_marker <- if (col %in% pk_cols) " PK" else ""
      lines <- c(lines, paste0("        ", dtype, " ", col, pk_marker))
    }
    lines <- c(lines, "    }")
  }

  # Relationships
  for (r in rels) {
    label <- r$from_col
    lines <- c(lines, paste0(
      "    ", r$to_table, " ||--o{ ", r$from_table, " : \"", label, "\""
    ))
  }

  paste(lines, collapse = "\n")
}

# ── Session save/restore ──────────────────────────────────────

save_session_json <- function(tables, rels, manual_rels, schema_rels,
                              settings = list()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return("{}")
  }

  # Convert data frames to serializable format
  tables_ser <- lapply(tables, function(df) {
    lapply(df, function(col) {
      if (inherits(col, c("Date", "POSIXt"))) as.character(col)
      else col
    })
  })

  session <- list(
    version = 1L,
    timestamp = as.character(Sys.time()),
    tables = tables_ser,
    relationships = rels,
    manual_relationships = manual_rels,
    schema_relationships = schema_rels,
    settings = settings
  )

  jsonlite::toJSON(session, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

restore_session_json <- function(json_text) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(tables = list(), relationships = list(),
                manual_relationships = list(), schema_relationships = list(),
                settings = list()))
  }

  session <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)

  # Reconstruct data frames
  tables <- list()
  if (!is.null(session$tables)) {
    for (tname in names(session$tables)) {
      tdata <- session$tables[[tname]]
      if (length(tdata) > 0) {
        # tdata is a named list of columns (each column is a list of values)
        col_list <- lapply(tdata, function(col) {
          if (is.list(col)) unlist(col) else col
        })
        df <- tryCatch(
          as.data.frame(col_list, stringsAsFactors = FALSE),
          error = function(e) as.data.frame(tdata, stringsAsFactors = FALSE)
        )
        tables[[tname]] <- df
      }
    }
  }

  list(
    tables = tables,
    relationships = session$relationships %||% list(),
    manual_relationships = session$manual_relationships %||% list(),
    schema_relationships = session$schema_relationships %||% list(),
    settings = session$settings %||% list()
  )
}
