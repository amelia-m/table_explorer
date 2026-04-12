# ============================================================
# mod_export.R — Export Panel
# ============================================================

#' Export module UI
#' @noRd
mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    div(
      class = "sidebar-box",
      style = "max-width:600px;",
      div(class = "section-title", "Export Formats"),
      div(
        style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:16px;",
        downloadButton(ns("dl_rels_csv"), "Relationships CSV", class = "dl-btn"),
        downloadButton(ns("dl_dbt_yaml"), "dbt schema.yml",    class = "dl-btn"),
        downloadButton(ns("dl_mermaid"),  "Mermaid ERD",       class = "dl-btn")
      ),
      tags$hr(),
      div(class = "section-title", "Session"),
      div(
        style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:8px;",
        downloadButton(ns("dl_session"), "Save Session", class = "dl-btn"),
        fileInput(ns("restore_session_file"), NULL,
                  accept = ".json",
                  buttonLabel = "Restore Session",
                  placeholder = "No file selected")
      )
    )
  )
}

#' Export module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables
#' @param all_rels_rv reactive returning all relationships
#' @param pk_map_rv reactive returning PK map
#' @param composite_pk_map_rv reactive returning composite PK map
#' @param manual_rels_rv reactiveVal holding manual relationships
#' @param schema_rels_rv reactiveVal holding schema relationships
#' @param detect_method reactive returning the current detect_method setting
#' @param min_confidence reactive returning the current min_confidence setting
#' @noRd
mod_export_server <- function(id, all_tables_rv, all_rels_rv,
                               pk_map_rv, composite_pk_map_rv,
                               manual_rels_rv, schema_rels_rv,
                               detect_method, min_confidence) {
  moduleServer(id, function(input, output, session) {

    output$dl_rels_csv <- downloadHandler(
      filename = "table_relationships.csv",
      content  = .rels_csv_content(all_rels_rv)
    )

    output$dl_dbt_yaml <- downloadHandler(
      filename = "schema.yml",
      content = function(file) {
        yaml_str <- generate_dbt_yaml(
          all_tables_rv(), all_rels_rv(), pk_map_rv(), composite_pk_map_rv()
        )
        writeLines(yaml_str, file)
      }
    )

    output$dl_mermaid <- downloadHandler(
      filename = "erd.mmd",
      content = function(file) {
        mmd_str <- generate_mermaid_erd(
          all_tables_rv(), all_rels_rv(), pk_map_rv(), composite_pk_map_rv()
        )
        writeLines(mmd_str, file)
      }
    )

    output$dl_session <- downloadHandler(
      filename = function() {
        paste0("table_explorer_session_", format(Sys.Date(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        json_str <- save_session_json(
          tables      = all_tables_rv(),
          rels        = all_rels_rv(),
          manual_rels = manual_rels_rv(),
          schema_rels = schema_rels_rv(),
          settings    = list(
            detect_method  = detect_method(),
            min_confidence = min_confidence()
          )
        )
        writeLines(json_str, file)
      }
    )

    observeEvent(input$restore_session_file, {
      req(input$restore_session_file)
      json_text <- paste(
        readLines(input$restore_session_file$datapath, warn = FALSE),
        collapse = "\n"
      )
      result <- tryCatch(
        restore_session_json(json_text),
        error = function(e) {
          showNotification(
            paste0("Could not restore session: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(result)) return()

      if (length(result$tables) > 0) {
        all_tables_rv(result$tables)
      }
      if (length(result$manual_relationships) > 0) {
        manual_rels_rv(result$manual_relationships)
      }
      if (length(result$schema_relationships) > 0) {
        schema_rels_rv(result$schema_relationships)
      }
      showNotification(
        paste0("Session restored: ", length(result$tables), " table(s)"),
        type = "message", duration = 5
      )
    })

    invisible(NULL)
  })
}
