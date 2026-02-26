# ============================================================
# Table Relationship Explorer — Shiny App
# Deploy to Posit Connect (Free Plan)
# ============================================================

library(shiny)
library(visNetwork)
library(DT)
library(shinythemes)

# ============================================================
# Helper Functions
# ============================================================

clean_name <- function(name) tolower(gsub("[^a-zA-Z0-9]", "", name))

detect_pks <- function(df, table_name, method = "both") {
  cols <- names(df)
  candidates <- character(0)
  n <- nrow(df)

  if (method %in% c("naming", "both")) {
    tname <- clean_name(table_name)
    hits <- cols[
      tolower(cols) == "id" |
      tolower(cols) == paste0(tname, "_id") |
      tolower(cols) == paste0(tname, "id")
    ]
    candidates <- union(candidates, hits)
  }

  if (method %in% c("uniqueness", "both") && n > 0) {
    hits <- cols[vapply(cols, function(c) {
      v <- df[[c]]
      !anyNA(v) && length(unique(v)) == n
    }, logical(1))]
    candidates <- union(candidates, hits)
  }

  candidates
}

detect_fks <- function(tables, method = "both") {
  rels    <- list()
  tnames  <- names(tables)
  if (method == "manual" || length(tnames) < 2) return(rels)

  # Pre-compute PK columns (all-unique, no-NA) per table for uniqueness matching
  pk_map <- lapply(tnames, function(t) {
    df <- tables[[t]]
    n  <- nrow(df)
    if (n == 0) return(character(0))
    names(df)[vapply(names(df), function(c) !anyNA(df[[c]]) && length(unique(df[[c]])) == n, logical(1))]
  })
  names(pk_map) <- tnames

  for (t1 in tnames) {
    df1 <- tables[[t1]]
    n1  <- nrow(df1)

    for (col in names(df1)) {
      col_l <- tolower(col)
      if (col %in% pk_map[[t1]]) next   # skip PKs of the same table

      for (t2 in tnames) {
        if (t2 == t1) next

        # Skip if already have a rel for this (t1, col, t2) pair
        if (any(vapply(rels, function(r)
            r$from_table == t1 && r$from_col == col && r$to_table == t2, logical(1)))) next

        df2     <- tables[[t2]]
        t2clean <- clean_name(t2)

        # --- Naming convention ---
        if (method %in% c("naming", "both")) {
          if (col_l == paste0(t2clean, "_id") || col_l == paste0(t2clean, "id")) {
            pk_col <- if (length(pk_map[[t2]]) > 0) pk_map[[t2]][1] else NA_character_
            rels[[length(rels) + 1]] <- list(
              from_table  = t1, from_col   = col,
              to_table    = t2, to_col     = pk_col,
              detected_by = "naming"
            )
            next
          }
        }

        # --- Uniqueness / value-subset ---
        if (method %in% c("uniqueness", "both") && n1 > 0 && length(pk_map[[t2]]) > 0) {
          vals1 <- na.omit(df1[[col]])
          if (length(vals1) == 0) next
          for (pk_col in pk_map[[t2]]) {
            if (all(vals1 %in% df2[[pk_col]])) {
              rels[[length(rels) + 1]] <- list(
                from_table  = t1, from_col   = col,
                to_table    = t2, to_col     = pk_col,
                detected_by = "uniqueness"
              )
              break
            }
          }
        }
      }
    }
  }
  rels
}

build_network <- function(tables, rels, pk_map) {
  tnames     <- names(tables)
  name_to_id <- setNames(seq_along(tnames), tnames)

  nodes <- data.frame(
    id = seq_along(tnames),
    label = vapply(tnames, function(t) {
      df <- tables[[t]]
      paste0(t, "\n", format(nrow(df), big.mark = ","), " rows | ", ncol(df), " cols")
    }, character(1)),
    title = vapply(tnames, function(t) {
      df   <- tables[[t]]
      pks  <- pk_map[[t]]
      fkr  <- Filter(function(r) r$from_table == t, rels)
      fk_s <- if (length(fkr) > 0)
                paste(vapply(fkr, function(r) paste0(r$from_col, " → ", r$to_table), character(1)), collapse = "<br>")
              else "none"
      pk_s <- if (length(pks) > 0) paste(pks, collapse = ", ") else "none detected"
      paste0(
        "<div style='font-size:13px;padding:10px;min-width:200px;'>",
        "<b style='font-size:15px;color:#2b6cb0;'>", t, "</b><hr style='margin:5px 0;'>",
        "<b>Rows:</b> ", format(nrow(df), big.mark = ","), "<br>",
        "<b>Cols:</b> ", ncol(df), "<br>",
        "<b>PK(s):</b> <span style='color:#975a16;'>", pk_s, "</span><br>",
        "<b>FK(s):</b><br><span style='color:#553c9a;'>", fk_s, "</span>",
        "<hr style='margin:5px 0;'><b>All columns:</b><br>",
        "<span style='font-size:11px;color:#4a5568;'>", paste(names(df), collapse = ", "), "</span>",
        "</div>"
      )
    }, character(1)),
    color.background           = "#1E3A5F",
    color.border               = "#4E79A7",
    color.highlight.background = "#2A5298",
    color.highlight.border     = "#63B3ED",
    font.color = "white",
    font.size  = 13,
    shape      = "box",
    shadow     = TRUE,
    stringsAsFactors = FALSE
  )

  edge_method_color <- c(naming = "#4ADE80", uniqueness = "#FB923C", manual = "#F87171")

  if (length(rels) == 0) {
    edges <- data.frame(
      from = integer(0), to = integer(0),
      label = character(0), title = character(0),
      arrows = character(0), stringsAsFactors = FALSE
    )
  } else {
    edges <- do.call(rbind, lapply(rels, function(r) {
      to_col  <- if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "?"
      ecol    <- edge_method_color[r$detected_by]
      data.frame(
        from  = name_to_id[[r$from_table]],
        to    = name_to_id[[r$to_table]],
        label = r$from_col,
        title = paste0(
          "<div style='font-size:12px;padding:8px;'>",
          "<b>", r$from_col, "</b> in <i>", r$from_table, "</i><br>",
          "→ <b>", to_col, "</b> in <i>", r$to_table, "</i><br>",
          "<span style='color:", ecol, ";'>● </span>Detected by: <i>", r$detected_by, "</i>",
          "</div>"
        ),
        arrows            = "to",
        color.color       = ecol,
        color.highlight   = ecol,
        color.opacity     = 0.85,
        dashes            = (r$detected_by == "uniqueness"),
        font.size         = 10,
        font.color        = "#e2e8f0",
        font.strokeWidth  = 3,
        font.strokeColor  = "#1a202c",
        stringsAsFactors  = FALSE
      )
    }))
  }

  list(nodes = nodes, edges = edges)
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600;700&display=swap");

    body {
      background: #0f172a;
      font-family: "IBM Plex Sans", sans-serif;
      color: #e2e8f0;
    }
    .app-header {
      background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%);
      border-bottom: 1px solid #1e3a5f;
      padding: 20px 24px 16px;
      margin-bottom: 20px;
    }
    .app-header h2 {
      font-family: "IBM Plex Mono", monospace;
      color: #60a5fa;
      font-size: 22px;
      font-weight: 600;
      margin: 0 0 4px 0;
      letter-spacing: -0.5px;
    }
    .app-header p { color: #64748b; font-size: 13px; margin: 0; }

    /* Sidebar */
    .sidebar-box {
      background: #1e293b;
      border: 1px solid #1e3a5f;
      border-radius: 10px;
      padding: 18px;
    }
    .sidebar-box .section-title {
      font-family: "IBM Plex Mono", monospace;
      font-size: 11px;
      font-weight: 600;
      letter-spacing: 1.5px;
      text-transform: uppercase;
      color: #60a5fa;
      margin-bottom: 10px;
      margin-top: 4px;
    }
    .sidebar-box hr { border-color: #1e3a5f; margin: 14px 0; }

    /* Override shiny input labels */
    label { color: #94a3b8 !important; font-size: 12px !important; }
    .form-control, .selectize-input {
      background: #0f172a !important;
      border: 1px solid #334155 !important;
      color: #e2e8f0 !important;
      border-radius: 6px !important;
      font-size: 13px !important;
    }
    .form-control:focus { border-color: #60a5fa !important; box-shadow: 0 0 0 2px rgba(96,165,250,0.2) !important; }
    .selectize-dropdown { background: #1e293b !important; border: 1px solid #334155 !important; }
    .selectize-dropdown-content .option { color: #e2e8f0 !important; font-size: 13px; }
    .selectize-dropdown-content .option.active { background: #1e3a5f !important; }

    /* Buttons */
    .btn-add { background: #166534; border: none; color: #4ade80; font-size: 12px; border-radius: 6px; width: 100%; padding: 7px; margin-top: 6px; }
    .btn-add:hover { background: #14532d; color: #86efac; }
    .btn-danger-soft { background: #1a1a2e; border: 1px solid #7f1d1d; color: #f87171; font-size: 12px; border-radius: 6px; width: 100%; padding: 6px; margin-top: 4px; }
    .btn-danger-soft:hover { background: #450a0a; color: #fca5a5; }

    /* File input */
    .btn-file { background: #1e3a5f !important; border: none !important; color: #93c5fd !important; font-size: 12px !important; }
    .shiny-input-container > .input-group .form-control { font-size: 12px; }

    /* Tab panel */
    .nav-tabs { border-bottom: 1px solid #1e3a5f !important; background: transparent; }
    .nav-tabs > li > a {
      font-family: "IBM Plex Mono", monospace;
      font-size: 12px;
      color: #64748b !important;
      background: transparent !important;
      border: none !important;
      padding: 10px 16px;
      border-radius: 6px 6px 0 0 !important;
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
      color: #60a5fa !important;
      background: #1e293b !important;
      border-bottom: 2px solid #60a5fa !important;
    }
    .tab-content { background: #1e293b; border: 1px solid #1e3a5f; border-top: none; border-radius: 0 0 10px 10px; padding: 16px; }

    /* Empty state */
    .empty-state { text-align: center; padding: 80px 40px; color: #334155; }
    .empty-state h4 { color: #475569; font-family: "IBM Plex Mono", monospace; }

    /* Stat pills */
    .pill { display: inline-block; padding: 3px 10px; border-radius: 20px; font-size: 12px; font-family: "IBM Plex Mono", monospace; margin: 2px; }
    .pill-rows  { background: #0c2a1a; color: #4ade80; border: 1px solid #14532d; }
    .pill-cols  { background: #0c1a2e; color: #60a5fa; border: 1px solid #1e3a5f; }
    .pill-pk    { background: #2a1a00; color: #fbbf24; border: 1px solid #78350f; }
    .pill-fk    { background: #1a0a2e; color: #c084fc; border: 1px solid #4c1d95; }
    .pill-warn  { background: #1a0a0a; color: #f87171; border: 1px solid #7f1d1d; }

    /* Table cards */
    .tbl-card { background: #0f172a; border: 1px solid #1e3a5f; border-radius: 8px; padding: 16px; margin-bottom: 14px; }
    .tbl-card-title { font-family: "IBM Plex Mono", monospace; font-size: 15px; color: #93c5fd; margin: 0 0 10px 0; }

    /* DT dark overrides */
    table.dataTable { background: #0f172a !important; color: #cbd5e1 !important; border-color: #1e3a5f !important; }
    table.dataTable thead th { background: #1e293b !important; color: #94a3b8 !important; border-color: #334155 !important; font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 0.5px; }
    table.dataTable tbody tr { background: #0f172a !important; }
    table.dataTable tbody tr:hover { background: #1e293b !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { color: #64748b !important; }
    .dataTables_wrapper .dataTables_info { color: #475569 !important; font-size: 12px; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #1e3a5f !important; color: #60a5fa !important; border-color: #1e3a5f !important; }

    /* Relationships */
    .rel-row { display: flex; align-items: center; gap: 8px; padding: 8px 0; border-bottom: 1px solid #1e293b; font-size: 13px; }
    .rel-row:last-child { border-bottom: none; }
    .rel-table { font-weight: 600; color: #93c5fd; font-family: "IBM Plex Mono", monospace; }
    .rel-col   { color: #94a3b8; font-family: "IBM Plex Mono", monospace; font-size: 12px; }
    .rel-arrow { color: #334155; font-size: 16px; }
    .rel-method { font-size: 10px; padding: 2px 7px; border-radius: 10px; font-family: "IBM Plex Mono", monospace; margin-left: auto; }
    .m-naming    { background: #0c2a1a; color: #4ade80; }
    .m-uniqueness { background: #1a1000; color: #fb923c; }
    .m-manual    { background: #1a0a0a; color: #f87171; }
    .rel-section-hdr { font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 1.5px; text-transform: uppercase; padding: 10px 0 4px; color: #475569; }

    /* Legend */
    .legend { display: flex; gap: 16px; padding: 8px 0 12px; flex-wrap: wrap; }
    .legend-item { display: flex; align-items: center; gap: 6px; font-size: 12px; color: #64748b; font-family: "IBM Plex Mono", monospace; }
    .legend-dot { width: 10px; height: 10px; border-radius: 50%; }
    .vis-network { border-radius: 8px; }

    /* Download button */
    .dl-btn { background: #0c1a2e; border: 1px solid #1e3a5f; color: #60a5fa; font-size: 12px; border-radius: 6px; padding: 7px 14px; font-family: "IBM Plex Mono", monospace; }
    .dl-btn:hover { background: #1e3a5f; color: #93c5fd; }

    /* visNetwork container */
    #erd_plot { border-radius: 8px; overflow: hidden; }
  '))),

  # Header
  div(class = "app-header",
    h2("TABLE_RELATIONSHIP_EXPLORER"),
    p("Upload CSV files to discover primary keys, foreign keys, and inter-table links")
  ),

  fluidRow(
    style = "margin: 0 12px;",

    # ---- Sidebar ----
    column(3,
      div(class = "sidebar-box",

        div(class = "section-title", "01 // Upload Tables"),
        fileInput("csv_files", NULL, multiple = TRUE, accept = ".csv",
                  placeholder = "No files selected", buttonLabel = "Browse CSV"),

        tags$hr(),
        div(class = "section-title", "02 // Detection Method"),
        selectInput("detect_method", NULL,
                    choices = c(
                      "Both (naming + uniqueness)" = "both",
                      "Naming conventions only"    = "naming",
                      "Uniqueness / value subset"  = "uniqueness",
                      "Manual only"                = "manual"
                    ), selected = "both"),
        div(style = "font-size: 11px; color: #475569; margin-top: -6px; line-height: 1.5;",
          "Naming: detects columns like", tags$code(style="background:#0f172a;color:#60a5fa;padding:1px 4px;border-radius:3px;","id"),
          ",", tags$code(style="background:#0f172a;color:#60a5fa;padding:1px 4px;border-radius:3px;", "{table}_id"),
          "— Uniqueness: checks value overlap with other tables' PKs."
        ),

        tags$hr(),
        div(class = "section-title", "03 // Manual Override"),
        div(style = "font-size: 11px; color: #475569; margin-bottom: 8px;",
          "Add or override relationships directly."),
        uiOutput("ui_man_from_table"),
        uiOutput("ui_man_from_col"),
        uiOutput("ui_man_to_table"),
        uiOutput("ui_man_to_col"),
        actionButton("btn_add_rel", "＋ Add Relationship", class = "btn-add"),
        actionButton("btn_clear_manual", "✕  Clear Manual", class = "btn-danger-soft")
      )
    ),

    # ---- Main Panel ----
    column(9,
      tabsetPanel(id = "main_tabs",

        # --- ERD ---
        tabPanel("ERD Diagram",
          br(),
          conditionalPanel("output.has_tables == 'false'",
            div(class = "empty-state",
              div(style = "font-size: 48px; margin-bottom: 16px;", "◫"),
              h4("No tables loaded"),
              p(style = "color:#334155; font-size:13px;",
                "Upload one or more CSV files using the sidebar to begin.")
            )
          ),
          conditionalPanel("output.has_tables == 'true'",
            div(class = "legend",
              div(class = "legend-item", div(class = "legend-dot", style = "background:#4ADE80;"), "naming"),
              div(class = "legend-item", div(class = "legend-dot", style = "background:#FB923C; border-style:dashed;"), "uniqueness (dashed)"),
              div(class = "legend-item", div(class = "legend-dot", style = "background:#F87171;"), "manual")
            ),
            visNetworkOutput("erd_plot", height = "540px"),
            div(style = "font-size: 11px; color: #334155; margin-top: 8px;",
              "drag nodes · scroll to zoom · hover for details · click to highlight connections")
          )
        ),

        # --- Table Details ---
        tabPanel("Table Details",
          br(),
          uiOutput("table_details_ui")
        ),

        # --- Relationships ---
        tabPanel("Relationships",
          br(),
          uiOutput("relationships_ui"),
          br(),
          downloadButton("dl_rels", "⬇  Export CSV", class = "dl-btn")
        )
      )
    )
  ),
  br()
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ---- Load tables ----
  tables_rv <- reactive({
    req(input$csv_files)
    tbls <- lapply(seq_len(nrow(input$csv_files)), function(i) {
      tryCatch(
        read.csv(input$csv_files$datapath[i], stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
    })
    valid <- !vapply(tbls, is.null, logical(1))
    tbls  <- tbls[valid]
    names(tbls) <- tools::file_path_sans_ext(input$csv_files$name[valid])
    tbls
  })

  # ---- PKs per table ----
  pk_map_rv <- reactive({
    req(tables_rv())
    method <- if (input$detect_method == "manual") "both" else input$detect_method
    tnames <- names(tables_rv())
    setNames(
      lapply(tnames, function(t) detect_pks(tables_rv()[[t]], t, method)),
      tnames
    )
  })

  # ---- Auto FKs ----
  auto_rels_rv <- reactive({
    req(tables_rv())
    detect_fks(tables_rv(), input$detect_method)
  })

  # ---- Manual rels ----
  manual_rels_rv <- reactiveVal(list())

  all_rels_rv <- reactive({ c(auto_rels_rv(), manual_rels_rv()) })

  # ---- Helper for has_tables ----
  output$has_tables <- reactive({
    if (is.null(input$csv_files)) "false" else "true"
  })
  outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

  # ---- Manual UI ----
  output$ui_man_from_table <- renderUI({
    req(tables_rv())
    selectInput("man_from_table", "From Table:", choices = names(tables_rv()))
  })
  output$ui_man_from_col <- renderUI({
    req(tables_rv(), input$man_from_table, input$man_from_table %in% names(tables_rv()))
    selectInput("man_from_col", "From Column:", choices = names(tables_rv()[[input$man_from_table]]))
  })
  output$ui_man_to_table <- renderUI({
    req(tables_rv())
    selectInput("man_to_table", "To Table:", choices = names(tables_rv()))
  })
  output$ui_man_to_col <- renderUI({
    req(tables_rv(), input$man_to_table, input$man_to_table %in% names(tables_rv()))
    selectInput("man_to_col", "To Column (PK):", choices = names(tables_rv()[[input$man_to_table]]))
  })

  observeEvent(input$btn_add_rel, {
    req(input$man_from_table, input$man_from_col, input$man_to_table, input$man_to_col)
    if (input$man_from_table == input$man_to_table) {
      showNotification("From and To tables must differ.", type = "warning"); return()
    }
    manual_rels_rv(c(manual_rels_rv(), list(list(
      from_table = input$man_from_table, from_col = input$man_from_col,
      to_table   = input$man_to_table,   to_col   = input$man_to_col,
      detected_by = "manual"
    ))))
    showNotification("Relationship added.", type = "message")
  })

  observeEvent(input$btn_clear_manual, {
    manual_rels_rv(list())
    showNotification("Manual relationships cleared.", type = "message")
  })

  # ---- ERD ----
  output$erd_plot <- renderVisNetwork({
    req(tables_rv())
    net <- build_network(tables_rv(), all_rels_rv(), pk_map_rv())

    visNetwork(net$nodes, net$edges, background = "#0f172a") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = FALSE
      ) %>%
      visLayout(randomSeed = 42) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -80,
          springLength          = 220,
          springConstant        = 0.04,
          damping               = 0.9
        ),
        stabilization = list(iterations = 300, fit = TRUE)
      ) %>%
      visEdges(smooth = list(enabled = TRUE, type = "dynamic")) %>%
      visNodes(widthConstraint = list(minimum = 130, maximum = 230)) %>%
      visInteraction(navigationButtons = TRUE, tooltipDelay = 80, hover = TRUE)
  })

  # ---- Table Details ----
  output$table_details_ui <- renderUI({
    req(tables_rv(), pk_map_rv())
    tbls <- tables_rv()
    pks  <- pk_map_rv()
    rels <- all_rels_rv()

    lapply(names(tbls), function(t) {
      df    <- tbls[[t]]
      pk_v  <- pks[[t]]
      fk_rels <- Filter(function(r) r$from_table == t, rels)
      fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")

      pills <- tagList(
        span(class = "pill pill-rows", paste0(format(nrow(df), big.mark = ","), " rows")),
        span(class = "pill pill-cols", paste0(ncol(df), " cols")),
        if (length(pk_v) > 0)
          lapply(pk_v, function(p) span(class = "pill pill-pk", paste0("PK: ", p)))
        else
          span(class = "pill pill-warn", "⚠ no PK"),
        if (length(fk_cols) > 0)
          lapply(seq_along(fk_rels), function(i) {
            span(class = "pill pill-fk",
              paste0("FK: ", fk_rels[[i]]$from_col, " → ", fk_rels[[i]]$to_table))
          })
      )

      div(class = "tbl-card",
        p(class = "tbl-card-title", paste0("[ ", t, " ]")),
        div(style = "margin-bottom: 12px;", pills),
        DTOutput(paste0("dt_col_", make.names(t)))
      )
    })
  })

  # Dynamic DT outputs
  observe({
    req(tables_rv(), pk_map_rv())
    tbls <- tables_rv()
    pks  <- pk_map_rv()
    rels <- all_rels_rv()

    lapply(names(tbls), function(t) {
      local({
        tname <- t
        output_id <- paste0("dt_col_", make.names(tname))
        output[[output_id]] <- renderDT({
          df      <- tbls[[tname]]
          pk_v    <- pks[[tname]]
          fk_rels <- Filter(function(r) r$from_table == tname, rels)
          fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")

          smry <- data.frame(
            Column     = names(df),
            Type       = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
            `Non-null` = vapply(df, function(x) sum(!is.na(x)), integer(1)),
            `Unique`   = vapply(df, function(x) length(unique(na.omit(x))), integer(1)),
            `PK`       = ifelse(names(df) %in% pk_v, "✓", ""),
            `FK`       = ifelse(names(df) %in% fk_cols, "✓", ""),
            check.names = FALSE, stringsAsFactors = FALSE
          )

          datatable(smry,
            options = list(
              pageLength = 10, dom = "tp", scrollX = TRUE,
              columnDefs = list(list(className = "dt-center", targets = c(2,3,4,5)))
            ),
            rownames  = FALSE,
            selection = "none"
          ) %>%
            formatStyle("PK",
              color           = styleEqual("✓", "#fbbf24"),
              backgroundColor = styleEqual("✓", "#1a1000")) %>%
            formatStyle("FK",
              color           = styleEqual("✓", "#c084fc"),
              backgroundColor = styleEqual("✓", "#140a2e"))
        }, server = FALSE)
      })
    })
  })

  # ---- Relationships Tab ----
  output$relationships_ui <- renderUI({
    req(tables_rv())
    rels <- all_rels_rv()

    if (length(rels) == 0) {
      return(div(class = "empty-state",
        h4("No relationships detected"),
        p(style = "color:#334155; font-size:13px;",
          "Try uploading more tables or adjusting the detection method.")
      ))
    }

    methods <- list(
      list(key = "naming",    label = "Naming Convention",        cls = "m-naming"),
      list(key = "uniqueness", label = "Uniqueness / Value Subset", cls = "m-uniqueness"),
      list(key = "manual",    label = "Manual",                   cls = "m-manual")
    )

    tagList(
      lapply(methods, function(m) {
        m_rels <- Filter(function(r) r$detected_by == m$key, rels)
        if (length(m_rels) == 0) return(NULL)

        tagList(
          div(class = "rel-section-hdr",
            paste0(m$label, " (", length(m_rels), ")")),
          lapply(m_rels, function(r) {
            to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "?"
            div(class = "rel-row",
              span(class = "rel-table", r$from_table),
              span(class = "rel-col",   paste0(".", r$from_col)),
              span(class = "rel-arrow", "→"),
              span(class = "rel-table", r$to_table),
              span(class = "rel-col",   paste0(".", to_col)),
              span(class = paste("rel-method", m$cls), m$key)
            )
          })
        )
      })
    )
  })

  # ---- Download ----
  output$dl_rels <- downloadHandler(
    filename = "table_relationships.csv",
    content  = function(file) {
      rels <- all_rels_rv()
      if (length(rels) == 0) {
        write.csv(data.frame(from_table="",from_col="",to_table="",to_col="",detected_by="")[0,], file, row.names = FALSE)
      } else {
        df <- do.call(rbind, lapply(rels, function(r) {
          data.frame(
            from_table  = r$from_table,
            from_col    = r$from_col,
            to_table    = r$to_table,
            to_col      = if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "",
            detected_by = r$detected_by,
            stringsAsFactors = FALSE
          )
        }))
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
