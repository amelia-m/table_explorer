# ============================================================
# mod_db_connect.R — Database Connection Panel
# ============================================================

#' Database connection module UI
#' @noRd
mod_db_connect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-title", "03 // Database Connection"),
    selectInput(
      ns("db_type"), NULL,
      choices = c("(select)" = "", db_types),
      selected = ""
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] != '' && input['%s'] != 'sqlite' && input['%s'] != 'bigquery'",
        ns("db_type"), ns("db_type"), ns("db_type")
      ),
      textInput(ns("db_host"), "Host:", placeholder = "localhost"),
      textInput(ns("db_port"), "Port:", placeholder = "auto"),
      textInput(ns("db_name"), "Database:", placeholder = "mydb"),
      textInput(ns("db_user"), "User:"),
      passwordInput(ns("db_pass"), "Password:"),
      textInput(ns("db_schema"), "Schema:", value = "public")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'sqlite'", ns("db_type")),
      fileInput(ns("db_sqlite_file"), NULL,
                accept = c(".db", ".sqlite", ".sqlite3"),
                buttonLabel = "Select SQLite")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'bigquery'", ns("db_type")),
      textInput(ns("db_bq_project"), "Project ID:"),
      textInput(ns("db_bq_dataset"), "Dataset:")
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'sqlserver' || input['%s'] == 'snowflake'",
        ns("db_type"), ns("db_type")
      ),
      textInput(ns("db_driver"), "ODBC Driver:", placeholder = "auto-detect")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != ''", ns("db_type")),
      div(
        style = "display:flex;gap:6px;margin-bottom:8px;",
        actionButton(ns("btn_db_connect"), "Connect", class = "btn-add"),
        actionButton(ns("btn_db_disconnect"), "Disconnect", class = "btn-danger-soft")
      ),
      uiOutput(ns("db_status_ui")),
      uiOutput(ns("db_tables_ui")),
      actionButton(ns("btn_db_load"), "Load Selected Tables", class = "btn-add")
    )
  )
}

#' Database connection module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables
#' @param rename_log_rv reactiveVal holding rename log
#' @param schema_rels_rv reactiveVal holding schema relationships
#' @param table_meta_rv reactiveVal holding table metadata
#' @noRd
mod_db_connect_server <- function(id, all_tables_rv, rename_log_rv,
                                   schema_rels_rv, table_meta_rv) {
  moduleServer(id, function(input, output, session) {
    db_conn_rv <- reactiveVal(NULL)
    db_meta_rv <- reactiveVal(NULL)

    output$db_status_ui <- renderUI({
      conn <- db_conn_rv()
      if (is.null(conn)) {
        div(style = "font-size:10px;color:var(--text-faint);margin-bottom:6px;", "Not connected")
      } else {
        div(style = "font-size:10px;color:#4ade80;margin-bottom:6px;", "\u2713 Connected")
      }
    })

    observeEvent(input$btn_db_connect, {
      req(input$db_type)
      type <- input$db_type

      port_val <- NULL
      if (nzchar(input$db_port %||% "")) port_val <- as.integer(input$db_port)

      path_val <- ""
      if (type == "sqlite" && !is.null(input$db_sqlite_file))
        path_val <- input$db_sqlite_file$datapath

      errors <- character(0)
      notify_fn <- function(msg) { errors <<- c(errors, msg) }

      conn <- withProgress(message = "Connecting...", value = 0.5, {
        db_connect(
          type = type,
          host = input$db_host %||% "",
          port = port_val,
          dbname = input$db_name %||% "",
          user = input$db_user %||% "",
          password = input$db_pass %||% "",
          schema = input$db_schema %||% "public",
          driver = input$db_driver %||% "",
          project = input$db_bq_project %||% "",
          dataset = input$db_bq_dataset %||% "",
          path = path_val,
          notify_fn = notify_fn
        )
      })

      for (e in errors) showNotification(e, type = "error", duration = 10)

      if (!is.null(conn)) {
        db_conn_rv(conn)
        meta <- withProgress(message = "Introspecting schema...", value = 0.5, {
          db_introspect(conn, type, input$db_schema %||% "public")
        })
        db_meta_rv(meta)
        if (length(meta$fks) > 0) schema_rels_rv(c(schema_rels_rv(), meta$fks))
        showNotification(
          paste0("Connected! Found ", length(meta$tables), " table(s)."),
          type = "message", duration = 5
        )
      }
    })

    observeEvent(input$btn_db_disconnect, {
      conn <- db_conn_rv()
      if (!is.null(conn)) {
        db_close(conn)
        db_conn_rv(NULL)
        db_meta_rv(NULL)
        showNotification("Disconnected.", type = "message", duration = 3)
      }
    })

    output$db_tables_ui <- renderUI({
      meta <- db_meta_rv()
      if (is.null(meta) || length(meta$tables) == 0) return(NULL)
      checkboxGroupInput(
        session$ns("db_selected_tables"), "Select tables:",
        choices = meta$tables,
        selected = meta$tables[seq_len(min(10, length(meta$tables)))]
      )
    })

    observeEvent(input$btn_db_load, {
      conn <- db_conn_rv()
      req(conn, input$db_selected_tables)

      existing <- all_tables_rv()
      existing_meta <- table_meta_rv()
      schema_val <- input$db_schema %||% "public"
      db_renames <- list()

      withProgress(message = "Loading tables...", value = 0, {
        n <- length(input$db_selected_tables)
        for (i in seq_along(input$db_selected_tables)) {
          raw_tname <- input$db_selected_tables[i]
          tname <- janitor::make_clean_names(raw_tname)
          incProgress(1 / n, detail = tname)
          df <- db_load_table(conn, raw_tname, schema_val)
          if (!is.null(df) && nrow(df) > 0) {
            clean_df <- janitor::clean_names(df)
            existing[[tname]] <- clean_df
            existing_meta[[tname]] <- list(
              size = object.size(clean_df),
              nrow = nrow(clean_df),
              ncol = ncol(clean_df)
            )
            if (raw_tname != tname) {
              db_renames[[length(db_renames) + 1]] <- data.frame(
                object_type = "table", source = tname,
                original_name = raw_tname, cleaned_name = tname,
                stringsAsFactors = FALSE
              )
            }
            col_orig <- names(df); col_clean <- names(clean_df)
            col_changed <- col_orig != col_clean
            if (any(col_changed)) {
              db_renames[[length(db_renames) + 1]] <- data.frame(
                object_type = "column", source = tname,
                original_name = col_orig[col_changed], cleaned_name = col_clean[col_changed],
                stringsAsFactors = FALSE
              )
            }
          }
        }
      })

      all_tables_rv(existing)
      table_meta_rv(existing_meta)
      if (length(db_renames) > 0) {
        prev <- rename_log_rv()
        rename_log_rv(rbind(prev, do.call(rbind, db_renames)))
      }
      showNotification(
        paste0("Loaded ", length(input$db_selected_tables), " table(s) from database."),
        type = "message", duration = 5
      )
    })

    invisible(NULL)
  })
}
