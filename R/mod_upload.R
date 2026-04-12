# ============================================================
# mod_upload.R — Upload, Schema Import, Manual Override
# ============================================================

#' Upload module UI
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-title", "01 // Upload Tables"),
    fileInput(
      ns("data_files"),
      NULL,
      multiple = TRUE,
      accept = supported_extensions,
      placeholder = "No files selected",
      buttonLabel = "Add File(s)"
    ),
    div(
      style = "font-size:10px;color:var(--text-faint);margin-top:-8px;margin-bottom:8px;line-height:1.5;",
      "CSV, TSV, Excel, Parquet, JSON, SPSS, SAS, Stata, RDS, RData, ODS, Access"
    ),
    fileInput(
      ns("schema_file"),
      NULL,
      multiple = FALSE,
      accept = c(".json", ".yaml", ".yml"),
      placeholder = "No file selected",
      buttonLabel = "Import Schema"
    ),
    uiOutput(ns("loaded_tables_ui")),
    actionButton(
      ns("btn_clear_tables"),
      "\u2715  Remove All Tables",
      class = "btn-danger-soft"
    ),

    tags$hr(),
    div(class = "section-title", "04 // Manual Override"),
    div(
      style = "font-size: 11px; color: #475569; margin-bottom: 8px;",
      "Add or override relationships directly."
    ),
    uiOutput(ns("ui_man_from_table")),
    uiOutput(ns("ui_man_from_col")),
    uiOutput(ns("ui_man_to_table")),
    uiOutput(ns("ui_man_to_col")),
    actionButton(ns("btn_add_rel"), "\uff0b Add Relationship", class = "btn-add"),
    actionButton(ns("btn_clear_manual"), "\u2715  Clear Manual", class = "btn-danger-soft")
  )
}

#' Upload module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding the named list of data frames
#' @param rename_log_rv reactiveVal holding the rename log data frame
#' @param schema_rels_rv reactiveVal holding schema-defined relationships
#' @param table_meta_rv reactiveVal holding table metadata
#' @param fk_cache Mutable environment used as FK detection cache
#' @return list with manual_rels_rv reactive value
#' @noRd
mod_upload_server <- function(id, all_tables_rv, rename_log_rv,
                               schema_rels_rv, table_meta_rv, fk_cache) {
  moduleServer(id, function(input, output, session) {
    pending_conflicts_rv <- reactiveVal(list())
    manual_rels_rv <- reactiveVal(list())

    # ---- Add files when fileInput fires ----
    observeEvent(input$data_files, {
      req(input$data_files)
      existing <- all_tables_rv()
      existing_meta <- table_meta_rv()
      n_files <- nrow(input$data_files)

      errors <- character(0)
      notify_fn <- function(msg) { errors <<- c(errors, msg) }

      all_results <- withProgress(message = "Reading files...", value = 0, {
        lapply(seq_len(n_files), function(i) {
          incProgress(1 / n_files, detail = input$data_files$name[i])
          fname <- input$data_files$name[i]
          fpath <- input$data_files$datapath[i]
          fsize <- input$data_files$size[i]
          tryCatch(
            {
              result <- read_table_file(fpath, fname, notify_fn)
              raw_tbls <- result$tables
              if (length(raw_tbls) == 0) return(list(list(ok = FALSE, name = fname)))
              lapply(names(raw_tbls), function(tname) {
                raw_df <- raw_tbls[[tname]]
                clean_df <- janitor::clean_names(raw_df)
                clean_tname <- janitor::make_clean_names(tname)
                list(
                  ok = TRUE,
                  raw = raw_df,
                  clean = clean_df,
                  tname = clean_tname,
                  raw_tname = tname,
                  meta = list(size = fsize, nrow = nrow(clean_df), ncol = ncol(clean_df))
                )
              })
            },
            error = function(e) {
              notify_fn(paste0("Read error on ", fname, ": ", conditionMessage(e)))
              list(list(ok = FALSE, name = fname))
            }
          )
        })
      })

      flat <- do.call(c, all_results)
      valid <- vapply(flat, function(x) isTRUE(x$ok), logical(1))
      valid_entries <- flat[valid]
      failed_entries <- flat[!valid]

      for (e in errors) showNotification(e, type = "error", duration = 8)
      for (fe in failed_entries) {
        if (!is.null(fe$name))
          showNotification(paste0("Could not read: ", fe$name), type = "error", duration = 8)
      }
      if (length(valid_entries) == 0) {
        showNotification("No valid files could be loaded.", type = "error", duration = 8)
        return()
      }

      new_files <- list()
      exact_dupes <- character(0)
      conflicts <- list()

      for (entry in valid_entries) {
        nm <- entry$tname
        inc <- entry$meta
        if (!nm %in% names(existing)) {
          new_files[[nm]] <- entry
        } else {
          ex <- existing_meta[[nm]]
          same_size <- !is.null(ex) && identical(as.numeric(inc$size), as.numeric(ex$size))
          same_dims <- !is.null(ex) && identical(inc$nrow, ex$nrow) && identical(inc$ncol, ex$ncol)
          if (same_size && same_dims) {
            exact_dupes <- c(exact_dupes, nm)
          } else {
            conflicts[[length(conflicts) + 1]] <- list(
              name = nm, rc = entry, existing_meta = ex, incoming_meta = inc
            )
          }
        }
      }

      for (nm in exact_dupes) {
        showNotification(
          paste0("\u26a0 '", nm, "': identical file already loaded \u2014 no changes made."),
          type = "warning", duration = 6
        )
      }

      if (length(new_files) > 0) {
        .apply_new_files(new_files, existing, existing_meta,
                         vapply(valid_entries, `[[`, character(1), "tname"))
      }
      if (length(conflicts) > 0) {
        pending_conflicts_rv(conflicts)
        .show_conflict_modal(conflicts[[1]])
      }
    })

    # ---- Helper: apply new file entries to shared state ----
    .apply_new_files <- function(file_map, existing, existing_meta, new_names) {
      merged <- existing
      merged_meta <- existing_meta
      for (nm in names(file_map)) {
        rc <- file_map[[nm]]
        merged[[nm]] <- rc$clean
        merged_meta[[nm]] <- rc$meta
      }
      all_tables_rv(merged)
      table_meta_rv(merged_meta)

      log_rows <- list()
      for (nm in names(file_map)) {
        rc <- file_map[[nm]]
        raw_tname <- rc$raw_tname %||% nm
        if (!is.null(raw_tname) && raw_tname != nm) {
          log_rows[[length(log_rows) + 1]] <- data.frame(
            object_type = "table", source = nm,
            original_name = raw_tname, cleaned_name = nm,
            stringsAsFactors = FALSE
          )
        }
        orig <- names(rc$raw)
        cleaned <- names(rc$clean)
        changed <- orig != cleaned
        if (any(changed)) {
          log_rows[[length(log_rows) + 1]] <- data.frame(
            object_type = "column", source = nm,
            original_name = orig[changed], cleaned_name = cleaned[changed],
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(log_rows) > 0) {
        prev <- rename_log_rv()
        prev <- prev[!prev$source %in% names(file_map), , drop = FALSE]
        rename_log_rv(rbind(prev, do.call(rbind, log_rows)))
      }
      n <- length(file_map)
      showNotification(
        paste0(n, " table(s) added \u2014 ", length(merged), " total"),
        type = "message", duration = 4
      )
    }

    # ---- Conflict resolution modal ----
    .show_conflict_modal <- function(conflict) {
      nm <- conflict$name
      ex <- conflict$existing_meta
      inc <- conflict$incoming_meta

      showModal(modalDialog(
        title = tagList(
          tags$span(
            style = "color:#f59e0b;font-family:'IBM Plex Mono',monospace;",
            paste0("\u26a0 Duplicate table name: '", nm, "'")
          )
        ),
        tags$p(
          style = "font-size:13px;color:#94a3b8;",
          "A table named ", tags$b(nm),
          " is already loaded. The incoming file has different metadata:"
        ),
        tags$table(
          style = "width:100%;border-collapse:collapse;font-family:'IBM Plex Mono',monospace;font-size:12px;",
          tags$thead(
            tags$tr(
              tags$th(style = "padding:6px 10px;border-bottom:1px solid #334155;color:#64748b;text-align:left;", ""),
              tags$th(style = "padding:6px 10px;border-bottom:1px solid #334155;color:#60a5fa;text-align:left;", "Existing"),
              tags$th(style = "padding:6px 10px;border-bottom:1px solid #334155;color:#4ade80;text-align:left;", "Incoming")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(style = "padding:5px 10px;color:#64748b;", "Dimensions"),
              tags$td(style = "padding:5px 10px;",
                      if (!is.null(ex)) paste0(format(ex$nrow, big.mark = ","), " \u00d7 ", ex$ncol) else "\u2014"),
              tags$td(style = "padding:5px 10px;",
                      paste0(format(inc$nrow, big.mark = ","), " \u00d7 ", inc$ncol))
            ),
            tags$tr(
              tags$td(style = "padding:5px 10px;color:#64748b;", "File size"),
              tags$td(style = "padding:5px 10px;",
                      if (!is.null(ex)) paste0(round(ex$size / 1024, 1), " KB") else "\u2014"),
              tags$td(style = "padding:5px 10px;",
                      paste0(round(inc$size / 1024, 1), " KB"))
            )
          )
        ),
        br(),
        tags$p(style = "font-size:12px;color:#64748b;", "How should this be resolved?"),
        footer = tagList(
          actionButton(
            session$ns("conflict_overwrite"), "\u2b06 Replace existing",
            style = "background:#1e3a5f;color:#60a5fa;border:1px solid #334155;font-size:12px;"
          ),
          actionButton(
            session$ns("conflict_keep_both"),
            tagList("\u2795 Keep both (rename incoming to '", tags$code(paste0(nm, "_2")), "')"),
            style = "background:#0c2a1a;color:#4ade80;border:1px solid #14532d;font-size:12px;"
          ),
          actionButton(
            session$ns("conflict_skip"), "\u274c Skip incoming",
            style = "background:#1a0a0a;color:#f87171;border:1px solid #7f1d1d;font-size:12px;"
          )
        ),
        easyClose = FALSE, size = "m"
      ))
    }

    # ---- Conflict handlers ----
    resolve_conflict <- function(action) {
      conflicts <- pending_conflicts_rv()
      if (length(conflicts) == 0) return()
      conflict <- conflicts[[1]]
      nm <- conflict$name
      rc <- conflict$rc
      existing <- all_tables_rv()
      existing_meta <- table_meta_rv()

      if (action == "overwrite") {
        existing[[nm]] <- rc$clean
        existing_meta[[nm]] <- rc$meta
        all_tables_rv(existing)
        table_meta_rv(existing_meta)
        prev <- rename_log_rv()
        prev <- prev[prev$source != nm, , drop = FALSE]
        new_rows <- list()
        raw_tname <- rc$raw_tname %||% nm
        if (!is.null(raw_tname) && raw_tname != nm) {
          new_rows[[length(new_rows) + 1]] <- data.frame(
            object_type = "table", source = nm,
            original_name = raw_tname, cleaned_name = nm, stringsAsFactors = FALSE
          )
        }
        orig <- names(rc$raw); cleaned <- names(rc$clean); changed <- orig != cleaned
        if (any(changed)) {
          new_rows[[length(new_rows) + 1]] <- data.frame(
            object_type = "column", source = nm,
            original_name = orig[changed], cleaned_name = cleaned[changed], stringsAsFactors = FALSE
          )
        }
        if (length(new_rows) > 0) rename_log_rv(rbind(prev, do.call(rbind, new_rows)))
        showNotification(paste0("'", nm, "' replaced."), type = "message", duration = 3)
      } else if (action == "keep_both") {
        new_nm <- nm; suffix <- 2
        while (new_nm %in% names(existing)) { new_nm <- paste0(nm, "_", suffix); suffix <- suffix + 1 }
        existing[[new_nm]] <- rc$clean
        existing_meta[[new_nm]] <- rc$meta
        all_tables_rv(existing)
        table_meta_rv(existing_meta)
        prev <- rename_log_rv()
        new_rows <- list()
        raw_tname <- rc$raw_tname %||% new_nm
        if (!is.null(raw_tname) && raw_tname != new_nm) {
          new_rows[[length(new_rows) + 1]] <- data.frame(
            object_type = "table", source = new_nm,
            original_name = raw_tname, cleaned_name = new_nm, stringsAsFactors = FALSE
          )
        }
        orig <- names(rc$raw); cleaned <- names(rc$clean); changed <- orig != cleaned
        if (any(changed)) {
          new_rows[[length(new_rows) + 1]] <- data.frame(
            object_type = "column", source = new_nm,
            original_name = orig[changed], cleaned_name = cleaned[changed], stringsAsFactors = FALSE
          )
        }
        if (length(new_rows) > 0) rename_log_rv(rbind(prev, do.call(rbind, new_rows)))
        showNotification(paste0("Saved as '", new_nm, "'."), type = "message", duration = 3)
      } else {
        showNotification(paste0("'", nm, "' incoming file discarded."), type = "warning", duration = 3)
      }

      removeModal()
      remaining <- conflicts[-1]
      pending_conflicts_rv(remaining)
      if (length(remaining) > 0) .show_conflict_modal(remaining[[1]])
    }

    observeEvent(input$conflict_overwrite, resolve_conflict("overwrite"))
    observeEvent(input$conflict_keep_both, resolve_conflict("keep_both"))
    observeEvent(input$conflict_skip, resolve_conflict("skip"))

    # ---- Schema file import ----
    observeEvent(input$schema_file, {
      req(input$schema_file)
      path <- input$schema_file$datapath
      fname <- input$schema_file$name
      errors <- character(0)
      notify_fn <- function(msg) { errors <<- c(errors, msg) }

      result <- withProgress(
        message = paste0("Importing schema from ", fname, "..."),
        value = 0.3,
        parse_schema_file(path, fname, notify_fn)
      )
      for (e in errors) showNotification(e, type = "error", duration = 12)

      if (length(result$tables) > 0) {
        existing <- all_tables_rv()
        schema_renames <- list()
        for (nm in names(result$tables)) {
          clean_nm <- janitor::make_clean_names(nm)
          if (!clean_nm %in% names(existing)) {
            existing[[clean_nm]] <- janitor::clean_names(result$tables[[nm]])
            if (nm != clean_nm) {
              schema_renames[[length(schema_renames) + 1]] <- data.frame(
                object_type = "table", source = clean_nm,
                original_name = nm, cleaned_name = clean_nm, stringsAsFactors = FALSE
              )
            }
          }
        }
        all_tables_rv(existing)
        if (length(schema_renames) > 0) {
          prev <- rename_log_rv()
          rename_log_rv(rbind(prev, do.call(rbind, schema_renames)))
        }
      }

      if (length(result$relationships) > 0) {
        clean_rels <- lapply(result$relationships, function(r) {
          r$from_table <- janitor::make_clean_names(r$from_table)
          r$to_table <- janitor::make_clean_names(r$to_table)
          r
        })
        schema_rels_rv(clean_rels)
        showNotification(
          paste0("Schema imported: ", length(result$relationships),
                 " relationship(s), ", length(result$tables), " table(s)"),
          type = "message", duration = 5
        )
      } else {
        showNotification(
          paste0("Schema imported: ", length(result$tables), " table(s), no relationships found."),
          type = "message", duration = 5
        )
      }
    })

    # ---- Remove single table ----
    observeEvent(input$remove_table_name, {
      nm <- input$remove_table_name
      tbl <- all_tables_rv(); tbl[[nm]] <- NULL; all_tables_rv(tbl)
      meta <- table_meta_rv(); meta[[nm]] <- NULL; table_meta_rv(meta)
      log <- rename_log_rv()
      rename_log_rv(log[log$source != nm, , drop = FALSE])
      showNotification(paste0("Removed: ", nm), type = "message", duration = 3)
    })

    # ---- Clear all tables ----
    observeEvent(input$btn_clear_tables, {
      all_tables_rv(list())
      table_meta_rv(list())
      rename_log_rv(data.frame(
        object_type = character(), source = character(),
        original_name = character(), cleaned_name = character(),
        stringsAsFactors = FALSE
      ))
      fk_cache$key <- NULL
      fk_cache$result <- list()
      showNotification("All tables cleared.", type = "message", duration = 3)
    })

    # ---- Loaded tables list UI ----
    output$loaded_tables_ui <- renderUI({
      tbls <- all_tables_rv()
      if (length(tbls) == 0) {
        return(div(class = "loaded-tables-empty", "No tables loaded yet."))
      }
      tagList(
        div(
          style = "margin: 8px 0 4px;",
          lapply(names(tbls), function(nm) {
            df <- tbls[[nm]]
            div(
              class = "loaded-table-item",
              span(class = "tbl-name", nm),
              span(class = "tbl-meta",
                   paste0(format(nrow(df), big.mark = ","), "r \u00d7 ", ncol(df), "c")),
              tags$button(
                class = "btn-remove",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  session$ns("remove_table_name"), nm
                ),
                title = "Remove",
                "\u00d7"
              )
            )
          })
        )
      )
    })

    # ---- Manual relationship UI ----
    output$ui_man_from_table <- renderUI({
      req(length(all_tables_rv()) > 0)
      selectInput(session$ns("man_from_table"), "From Table:", choices = names(all_tables_rv()))
    })
    output$ui_man_from_col <- renderUI({
      tbls <- all_tables_rv()
      req(length(tbls) > 0, input$man_from_table, input$man_from_table %in% names(tbls))
      selectInput(session$ns("man_from_col"), "From Column:", choices = names(tbls[[input$man_from_table]]))
    })
    output$ui_man_to_table <- renderUI({
      req(length(all_tables_rv()) > 0)
      selectInput(session$ns("man_to_table"), "To Table:", choices = names(all_tables_rv()))
    })
    output$ui_man_to_col <- renderUI({
      tbls <- all_tables_rv()
      req(length(tbls) > 0, input$man_to_table, input$man_to_table %in% names(tbls))
      selectInput(session$ns("man_to_col"), "To Column (PK):", choices = names(tbls[[input$man_to_table]]))
    })

    observeEvent(input$btn_add_rel, {
      req(input$man_from_table, input$man_from_col, input$man_to_table, input$man_to_col)
      if (input$man_from_table == input$man_to_table) {
        showNotification("From and To tables must differ.", type = "warning")
        return()
      }
      manual_rels_rv(c(manual_rels_rv(), list(list(
        from_table = input$man_from_table, from_col = input$man_from_col,
        to_table = input$man_to_table, to_col = input$man_to_col,
        detected_by = "manual"
      ))))
      showNotification("Relationship added.", type = "message")
    })

    observeEvent(input$btn_clear_manual, {
      manual_rels_rv(list())
      showNotification("Manual relationships cleared.", type = "message")
    })

    # Return manual_rels_rv so app_server can include it in all_rels_rv
    list(manual_rels_rv = manual_rels_rv)
  })
}
