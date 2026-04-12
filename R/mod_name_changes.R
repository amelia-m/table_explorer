# ============================================================
# mod_name_changes.R — Name Changes Tab
# ============================================================

#' Name changes module UI
#' @noRd
mod_name_changes_ui <- function(id) {
  ns <- NS(id)
  tagList(br(), uiOutput(ns("rename_log_ui")))
}

#' Name changes module server
#'
#' @param id Module id
#' @param rename_log_rv reactiveVal holding the rename log data frame
#' @noRd
mod_name_changes_server <- function(id, rename_log_rv) {
  moduleServer(id, function(input, output, session) {

    output$rename_log_ui <- renderUI({
      log <- rename_log_rv()
      if (nrow(log) == 0) {
        return(tagList(
          div(
            class = "rename-summary",
            "No name changes detected. All table and column names were already clean."
          ),
          div(
            class = "empty-state",
            div(style = "font-size:36px;margin-bottom:12px;", "\u2713"),
            h4("All names clean"),
            p(style = "font-size:13px;color:var(--text-faint);",
              "janitor::clean_names() found nothing to rename in the uploaded files.")
          )
        ))
      }
      n_tables     <- length(unique(log$source))
      n_tbl_renames <- sum(log$object_type == "table")
      n_col_renames <- sum(log$object_type == "column")
      tagList(
        div(
          class = "rename-summary",
          tags$b(nrow(log)),
          " name(s) renamed across ",
          tags$b(n_tables),
          " table(s)",
          if (n_tbl_renames > 0)
            paste0(" (", n_tbl_renames, " table, ", n_col_renames, " column)"),
          ". ",
          "Amber = PK-related, purple = FK-related columns."
        ),
        DTOutput(session$ns("dt_rename_log")),
        br(),
        downloadButton(session$ns("dl_rename_log"), "\u2b07  Export CSV", class = "dl-btn")
      )
    })

    output$dt_rename_log <- renderDT(
      {
        log <- rename_log_rv()
        req(nrow(log) > 0)
        datatable(
          log,
          colnames = c("Type", "Table", "Original Name", "Cleaned Name"),
          options  = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
          rownames = FALSE, selection = "none"
        )
      },
      server = FALSE
    )

    output$dl_rename_log <- downloadHandler(
      filename = "name_changes.csv",
      content  = function(file) write.csv(rename_log_rv(), file, row.names = FALSE)
    )

    invisible(NULL)
  })
}
