# ============================================================
# mod_relationships.R — Relationships Tab
# ============================================================

#' Relationships module UI
#' @noRd
mod_relationships_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("relationships_ui")),
    br(),
    downloadButton(ns("dl_rels"), "\u2b07  Export CSV", class = "dl-btn")
  )
}

#' Relationships module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables
#' @param all_rels_rv reactive returning all relationships
#' @param false_positives_rv reactiveVal holding suppressed relationship keys
#' @param conf_overrides_rv reactiveVal holding confidence overrides
#' @noRd
mod_relationships_server <- function(id, all_tables_rv, all_rels_rv,
                                      false_positives_rv, conf_overrides_rv) {
  moduleServer(id, function(input, output, session) {

    output$relationships_ui <- renderUI({
      tbls <- all_tables_rv()
      req(length(tbls) > 0)
      rels <- all_rels_rv()

      if (length(rels) == 0) {
        return(div(
          class = "empty-state",
          h4("No relationships detected"),
          p(style = "color:#334155; font-size:13px;",
            "Try uploading more tables or adjusting the detection method.")
        ))
      }

      methods <- list(
        list(key = "naming",          label = "Naming Convention",       cls = "m-naming"),
        list(key = "name_similarity", label = "Name Similarity",         cls = "m-name_similarity"),
        list(key = "value_overlap",   label = "Value Overlap",           cls = "m-value_overlap"),
        list(key = "cardinality",     label = "Cardinality Match",       cls = "m-cardinality"),
        list(key = "format",          label = "Format Fingerprint",      cls = "m-format"),
        list(key = "distribution",    label = "Distribution Similarity", cls = "m-distribution"),
        list(key = "null_pattern",    label = "Null Pattern",            cls = "m-null_pattern"),
        list(key = "content",         label = "Content Analysis",        cls = "m-content"),
        list(key = "schema",          label = "Schema Defined",          cls = "m-schema"),
        list(key = "manual",          label = "Manual",                  cls = "m-manual")
      )

      tagList(
        lapply(methods, function(m) {
          m_rels <- Filter(function(r) r$detected_by == m$key, rels)
          if (length(m_rels) == 0) return(NULL)
          tagList(
            div(class = "rel-section-hdr", paste0(m$label, " (", length(m_rels), ")")),
            lapply(m_rels, function(r) {
              to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "?"
              conf <- if (!is.null(r$confidence)) r$confidence else ""
              score_val <- if (!is.null(r$score)) r$score else NA
              conf_tags <- if (nzchar(conf)) {
                tagList(
                  span(class = paste("conf-dot", paste0("conf-", conf))),
                  span(class = "conf-label", paste0(
                    conf,
                    if (!is.na(score_val)) paste0(" ", round(score_val * 100), "%") else ""
                  ))
                )
              } else {
                NULL
              }
              signal_tags <- if (!is.null(r$signals) && length(r$signals) > 0) {
                div(class = "signal-chips",
                    lapply(names(r$signals), function(s) span(class = "signal-chip", s)))
              } else {
                NULL
              }
              rk <- paste(r$from_table, r$from_col, r$to_table, to_col, sep = "|")
              div(
                class = "rel-row",
                span(class = "rel-table", r$from_table),
                span(class = "rel-col", paste0(".", r$from_col)),
                span(class = "rel-arrow", "\u2192"),
                span(class = "rel-table", r$to_table),
                span(class = "rel-col", paste0(".", to_col)),
                span(class = paste("rel-method", m$cls), m$key),
                conf_tags,
                signal_tags,
                tags$button(
                  class = "btn btn-xs",
                  style = "font-size:9px;padding:1px 5px;margin-left:6px;background:#1a0a0a;color:#f87171;border:1px solid #7f1d1d;border-radius:4px;cursor:pointer;",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    session$ns("suppress_rel"), rk
                  ),
                  "\u2715"
                )
              )
            })
          )
        }),
        if (length(false_positives_rv()) > 0) {
          div(
            style = "margin-top:12px;padding-top:8px;border-top:1px solid var(--border);",
            span(style = "font-size:10px;color:var(--text-faint);",
                 paste0(length(false_positives_rv()), " relationship(s) suppressed")),
            tags$button(
              class = "btn btn-xs",
              style = "font-size:10px;padding:2px 8px;margin-left:8px;background:#0c2a1a;color:#4ade80;border:1px solid #15803d;border-radius:4px;cursor:pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
                session$ns("restore_suppressed")
              ),
              "Restore all"
            )
          )
        }
      )
    })

    observeEvent(input$suppress_rel, {
      rk <- input$suppress_rel
      current <- false_positives_rv()
      if (!rk %in% current) {
        false_positives_rv(c(current, rk))
        showNotification("Relationship suppressed.", type = "message", duration = 3)
      }
    })

    observeEvent(input$restore_suppressed, {
      false_positives_rv(character(0))
      showNotification("All suppressed relationships restored.", type = "message", duration = 3)
    })

    output$dl_rels <- downloadHandler(
      filename = "table_relationships.csv",
      content  = .rels_csv_content(all_rels_rv)
    )

    invisible(NULL)
  })
}

# Helper: build downloadHandler content function for relationships CSV
.rels_csv_content <- function(all_rels_rv) {
  function(file) {
    rels <- all_rels_rv()
    if (length(rels) == 0) {
      write.csv(
        data.frame(
          from_table = "", from_col = "", to_table = "", to_col = "",
          detected_by = "", confidence = "", score = numeric(0),
          signals = "", reasons = ""
        )[0, ],
        file, row.names = FALSE
      )
    } else {
      df <- do.call(rbind, lapply(rels, function(r) {
        data.frame(
          from_table  = r$from_table,
          from_col    = r$from_col,
          to_table    = r$to_table,
          to_col      = if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "",
          detected_by = r$detected_by,
          confidence  = if (!is.null(r$confidence)) r$confidence else "",
          score       = if (!is.null(r$score)) r$score else NA_real_,
          signals     = if (!is.null(r$signals)) paste(names(r$signals), collapse = "; ") else "",
          reasons     = if (!is.null(r$reasons)) paste(r$reasons, collapse = "; ") else "",
          stringsAsFactors = FALSE
        )
      }))
      write.csv(df, file, row.names = FALSE)
    }
  }
}
