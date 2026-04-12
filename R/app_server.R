# ============================================================
# app_server.R — Main Application Server
# ============================================================

#' @noRd
app_server <- function(input, output, session) {
  # ── Shared mutable state ─────────────────────────────────────
  all_tables_rv <- reactiveVal(list())
  rename_log_rv <- reactiveVal(data.frame(
    object_type = character(), source = character(),
    original_name = character(), cleaned_name = character(),
    stringsAsFactors = FALSE
  ))
  schema_rels_rv  <- reactiveVal(list())
  table_meta_rv   <- reactiveVal(list())
  false_positives_rv <- reactiveVal(character(0))
  conf_overrides_rv  <- reactiveVal(list())

  # FK detection cache (mutable env, shared across modules)
  fk_cache <- new.env(parent = emptyenv())
  fk_cache$key    <- NULL
  fk_cache$result <- list()

  # ── Upload module (includes manual override section) ─────────
  upload_out <- mod_upload_server(
    "upload", all_tables_rv, rename_log_rv, schema_rels_rv, table_meta_rv, fk_cache
  )
  manual_rels_rv <- upload_out$manual_rels_rv

  # ── Database connection module ───────────────────────────────
  mod_db_connect_server("db", all_tables_rv, rename_log_rv, schema_rels_rv, table_meta_rv)

  # ── Detection module (returns settings + strategy) ───────────
  detection <- mod_detection_server("detection", all_tables_rv, fk_cache)

  # ── PK detection reactives ───────────────────────────────────
  pk_map_rv <- reactive({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    method <- if (detection$detect_method() == "manual") "both" else detection$detect_method()
    setNames(
      lapply(names(tbls), function(t) detect_pks(tbls[[t]], t, method)),
      names(tbls)
    )
  })

  composite_pk_map_rv <- reactive({
    tbls <- all_tables_rv()
    if (!detection$enable_composite_pk()) {
      return(setNames(vector("list", length(tbls)), names(tbls)))
    }
    req(length(tbls) > 0)
    setNames(
      lapply(names(tbls), function(t) detect_composite_pks(tbls[[t]], t)),
      names(tbls)
    )
  })

  # ── FK detection with cache ───────────────────────────────────
  auto_rels_rv <- reactive({
    tbls <- all_tables_rv()
    strategy <- detection$scan_strategy_rv()
    settings <- detection$detection_settings_rv()
    detection$detection_run_counter()   # explicit dependency
    detection$triage_btn_counter()      # explicit dependency
    req(length(tbls) > 0)

    if (identical(strategy, "pending")) return(list())
    if (identical(strategy, "skip"))    return(list())
    if (is.null(settings))             return(list())

    method <- if (identical(strategy, "naming_only")) "naming" else settings$method
    min_conf <- settings$min_conf %||% "medium"

    flags <- if (identical(strategy, "naming_only")) {
      list(naming = TRUE, value_overlap = FALSE, cardinality = FALSE,
           format = FALSE, distribution = FALSE, null_pattern = FALSE)
    } else {
      list(
        naming        = isTRUE(settings$naming),
        value_overlap = isTRUE(settings$value_overlap),
        cardinality   = isTRUE(settings$cardinality),
        format        = isTRUE(settings$format),
        distribution  = isTRUE(settings$distribution),
        null_pattern  = isTRUE(settings$null_pattern)
      )
    }

    key_parts <- paste(
      names(tbls),
      vapply(tbls, nrow, integer(1)),
      vapply(tbls, ncol, integer(1)),
      sep = ":", collapse = "|"
    )
    flag_str  <- paste(vapply(flags, as.character, character(1)), collapse = "")
    cache_key <- paste0(key_parts, "//", method, "//", min_conf, "//", flag_str, "//", strategy)

    if (!is.null(fk_cache$key) && identical(fk_cache$key, cache_key)) {
      return(fk_cache$result)
    }

    sampled_tbls <- lapply(tbls, function(df) {
      if (nrow(df) > 10000) {
        set.seed(42)
        df[sample(nrow(df), 10000), , drop = FALSE]
      } else {
        df
      }
    })
    names(sampled_tbls) <- names(tbls)

    for (nm in names(sampled_tbls)) {
      df <- sampled_tbls[[nm]]
      if (ncol(df) > 60) {
        id_cols   <- grep("(_id|_key|id$|key$|_code|_num)", names(df), value = TRUE)
        other_cols <- setdiff(names(df), id_cols)
        keep <- union(id_cols, head(other_cols, 60 - length(id_cols)))
        sampled_tbls[[nm]] <- df[, keep, drop = FALSE]
      }
    }

    result <- withProgress(message = "Detecting relationships...", value = 0.1, {
      incProgress(0.1, detail = paste0(length(sampled_tbls), " tables, sampling..."))
      tryCatch(
        {
          r <- detect_fks(sampled_tbls, method, min_conf, flags)
          incProgress(0.8, detail = paste0("Found ", length(r), " relationship(s)"))
          r
        },
        error = function(e) {
          showNotification(
            paste0("Relationship detection error: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          list()
        }
      )
    })

    fk_cache$key    <- cache_key
    fk_cache$result <- result
    result
  })

  # ── Combined relationships ────────────────────────────────────
  rel_key <- function(r) paste(r$from_table, r$from_col, r$to_table, r$to_col, sep = "|")

  all_rels_rv <- reactive({
    raw       <- c(auto_rels_rv(), manual_rels_rv(), schema_rels_rv())
    suppressed <- false_positives_rv()
    overrides  <- conf_overrides_rv()
    filtered  <- Filter(function(r) !rel_key(r) %in% suppressed, raw)
    lapply(filtered, function(r) {
      k <- rel_key(r)
      if (k %in% names(overrides)) {
        r$confidence <- overrides[[k]]
        conf_rank <- c(low = 0.3, medium = 0.7, high = 0.95)
        r$score <- conf_rank[[r$confidence]]
      }
      r
    })
  })

  # ── has_tables output (used by conditionalPanel in ERD tab) ──
  output$has_tables <- reactive({
    if (length(all_tables_rv()) > 0) "true" else "false"
  })
  outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

  # ── Module servers ────────────────────────────────────────────
  mod_erd_server("erd", all_tables_rv, all_rels_rv, pk_map_rv, composite_pk_map_rv)

  mod_table_details_server(
    "table_details", all_tables_rv, pk_map_rv, composite_pk_map_rv, all_rels_rv
  )

  mod_relationships_server(
    "relationships", all_tables_rv, all_rels_rv, false_positives_rv, conf_overrides_rv
  )

  mod_name_changes_server("name_changes", rename_log_rv)

  mod_export_server(
    "export",
    all_tables_rv, all_rels_rv, pk_map_rv, composite_pk_map_rv,
    manual_rels_rv, schema_rels_rv,
    detect_method  = detection$detect_method,
    min_confidence = reactive(
      detection$detection_settings_rv()$min_conf %||% "medium"
    )
  )
}
