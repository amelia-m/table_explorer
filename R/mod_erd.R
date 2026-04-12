# ============================================================
# mod_erd.R — ERD Visualization Panel
# ============================================================

#' ERD module UI
#' @noRd
mod_erd_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    conditionalPanel(
      "output.has_tables == 'false'",
      div(
        class = "empty-state",
        div(style = "font-size: 48px; margin-bottom: 16px;", "\u25eb"),
        h4("No tables loaded"),
        p(style = "color:#334155; font-size:13px;",
          "Upload one or more CSV files using the sidebar to begin.")
      )
    ),
    conditionalPanel(
      "output.has_tables == 'true'",
      div(
        class = "legend",
        div(class = "legend-item", div(class = "legend-dot", style = "background:#4ADE80;"), "naming"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#2DD4BF;"), "name similarity"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#FB923C;"), "value overlap"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#FACC15;"), "cardinality"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#60A5FA;"), "format"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#A78BFA;"), "distribution"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#F0ABFC;"), "null pattern"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#67E8F9;"), "schema"),
        div(class = "legend-item", div(class = "legend-dot", style = "background:#F87171;"), "manual"),
        div(class = "legend-item",
            div(class = "legend-dot", style = "background:#fff;border:1px dashed #64748b;"),
            "low confidence (dashed)")
      ),
      div(
        style = "display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:8px;",
        selectInput(ns("erd_layout"), "Layout:", width = "160px",
                    choices = c("Force" = "force", "Hierarchical" = "hierarchical",
                                "Circular" = "circular"),
                    selected = "force"),
        sliderInput(ns("spring_length"), "Spring length:", width = "200px",
                    min = 80, max = 600, value = 220, step = 20)
      ),
      div(class = "erd-container",
          visNetworkOutput(ns("erd_plot"), height = "540px")),
      div(class = "erd-hint",
          "drag nodes \u00b7 scroll to zoom \u00b7 hover for details \u00b7 click to highlight connections")
    ),
    # Sticky node detail panel overlay
    tags$div(
      id = "node-panel-overlay",
      style = paste0(
        "display:none;position:fixed;top:80px;right:20px;z-index:9999;",
        "width:360px;max-height:80vh;overflow-y:auto;",
        "background:#0f172a;border:1px solid #1e3a5f;border-radius:10px;",
        "box-shadow:0 8px 32px rgba(0,0,0,0.7);",
        "font-family:'IBM Plex Mono',monospace;"
      ),
      tags$div(
        style = "display:flex;justify-content:space-between;align-items:center;padding:12px 16px 8px;border-bottom:1px solid #1e3a5f;",
        tags$span(
          id = "node-panel-title",
          style = "color:#60a5fa;font-size:13px;font-weight:700;",
          "Table Details"
        ),
        tags$button(
          id = "node-panel-close",
          style = "background:none;border:none;color:#64748b;font-size:18px;cursor:pointer;padding:0;line-height:1;",
          onclick = sprintf(
            "document.getElementById('node-panel-overlay').style.display='none'; Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
            ns("vis_close_panel")
          ),
          "\u00d7"
        )
      ),
      uiOutput(ns("node_panel_ui"))
    )
  )
}

#' ERD module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables
#' @param all_rels_rv reactive returning all relationships
#' @param pk_map_rv reactive returning PK map
#' @param composite_pk_map_rv reactive returning composite PK map
#' @noRd
mod_erd_server <- function(id, all_tables_rv, all_rels_rv,
                            pk_map_rv, composite_pk_map_rv) {
  moduleServer(id, function(input, output, session) {
    selected_node_rv <- reactiveVal(NULL)

    output$erd_plot <- renderVisNetwork({
      tbls <- all_tables_rv()
      req(length(tbls) > 0)
      tryCatch(
        {
          net <- build_network(tbls, all_rels_rv(), pk_map_rv(), composite_pk_map_rv())
          layout_mode <- input$erd_layout %||% "force"
          spring_len  <- input$spring_length %||% 220

          vis <- visNetwork(net$nodes, net$edges, background = "#0f172a") |>
            visOptions(
              highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
              nodesIdSelection = FALSE
            ) |>
            visEdges(smooth = list(enabled = TRUE, type = "dynamic")) |>
            visNodes(widthConstraint = list(minimum = 130, maximum = 230)) |>
            visInteraction(navigationButtons = TRUE, tooltipDelay = 80, hover = TRUE) |>
            visEvents(
              click = sprintf(
                "function(params) {
                  if (params.nodes.length > 0) {
                    Shiny.setInputValue('%s', {id: params.nodes[0], ts: Date.now()}, {priority: 'event'});
                    showNodePanel(params.nodes[0]);
                  }
                }",
                session$ns("vis_clicked_node")
              )
            )

          if (layout_mode == "hierarchical") {
            vis <- vis |>
              visHierarchicalLayout(direction = "UD", sortMethod = "directed") |>
              visPhysics(enabled = FALSE)
          } else if (layout_mode == "circular") {
            n_nodes <- nrow(net$nodes)
            if (n_nodes > 0) {
              radius <- max(200, n_nodes * 50)
              angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[seq_len(n_nodes)]
              net$nodes$x <- cos(angles) * radius
              net$nodes$y <- sin(angles) * radius
            }
            vis <- visNetwork(net$nodes, net$edges, background = "#0f172a") |>
              visOptions(
                highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                nodesIdSelection = FALSE
              ) |>
              visEdges(smooth = list(enabled = TRUE, type = "dynamic")) |>
              visNodes(widthConstraint = list(minimum = 130, maximum = 230)) |>
              visInteraction(navigationButtons = TRUE, tooltipDelay = 80, hover = TRUE) |>
              visPhysics(enabled = FALSE) |>
              visEvents(
                click = sprintf(
                  "function(params) {
                    if (params.nodes.length > 0) {
                      Shiny.setInputValue('%s', {id: params.nodes[0], ts: Date.now()}, {priority: 'event'});
                      showNodePanel(params.nodes[0]);
                    }
                  }",
                  session$ns("vis_clicked_node")
                )
              )
          } else {
            vis <- vis |>
              visLayout(randomSeed = 42) |>
              visPhysics(
                solver = "forceAtlas2Based",
                forceAtlas2Based = list(
                  gravitationalConstant = -80,
                  springLength = spring_len,
                  springConstant = 0.04,
                  damping = 0.9
                ),
                stabilization = list(iterations = 300, fit = TRUE)
              )
          }
          vis
        },
        error = function(e) {
          showNotification(paste0("ERD error: ", conditionMessage(e)), type = "error", duration = 10)
          visNetwork(
            data.frame(id = 1, label = paste("Error:", conditionMessage(e)),
                       color = "#7f1d1d", font.color = "white"),
            data.frame(),
            background = "#0f172a"
          )
        }
      )
    })

    observeEvent(input$vis_clicked_node, {
      req(input$vis_clicked_node$id)
      selected_node_rv(input$vis_clicked_node$id)
    })

    observeEvent(input$vis_close_panel, {
      selected_node_rv(NULL)
    })

    output$node_panel_ui <- renderUI({
      node_id <- selected_node_rv()
      req(node_id)
      tbls <- all_tables_rv()
      pks  <- pk_map_rv()
      cpks <- composite_pk_map_rv()
      rels <- all_rels_rv()

      tnames <- names(tbls)
      req(node_id <= length(tnames))
      t <- tnames[[node_id]]
      df <- tbls[[t]]
      pk_v <- pks[[t]]
      cpk_groups <- cpks[[t]]
      cpk_cols <- unique(unlist(cpk_groups))
      fk_r <- Filter(function(r) r$from_table == t, rels)
      fk_cols <- vapply(fk_r, `[[`, character(1), "from_col")

      col_chips <- lapply(names(df), function(cn) {
        if (cn %in% pk_v || cn %in% cpk_cols) {
          tags$span(class = "col-chip",
                    style = "background:#2a1a00;color:#fbbf24;border-color:#78350f;", cn)
        } else if (cn %in% fk_cols) {
          tags$span(class = "col-chip",
                    style = "background:#1a0a2e;color:#c084fc;border-color:#4c1d95;", cn)
        } else {
          tags$span(class = "col-chip",
                    style = "background:rgba(255,255,255,0.04);color:#94a3b8;border-color:#1e3a5f;", cn)
        }
      })

      tagList(
        tags$div(
          class = "panel-section",
          tags$div(class = "panel-label", "Overview"),
          tags$div(
            style = "display:flex;gap:12px;flex-wrap:wrap;",
            tags$span(class = "pill pill-rows", paste0(format(nrow(df), big.mark = ","), " rows")),
            tags$span(class = "pill pill-cols", paste0(ncol(df), " cols"))
          )
        ),
        tags$div(
          class = "panel-section",
          tags$div(class = "panel-label", "Primary Key(s)"),
          if (length(pk_v) > 0) {
            tags$div(lapply(pk_v, function(p) tags$span(class = "pill pill-pk", p)))
          } else if (length(cpk_groups) > 0) {
            tags$div(lapply(cpk_groups, function(g) {
              tags$span(class = "pill pill-pk", paste0("CPK: ", paste(g, collapse = " + ")))
            }))
          } else {
            tags$span(style = "color:#f87171;font-size:12px;", "none detected")
          }
        ),
        tags$div(
          class = "panel-section",
          tags$div(class = "panel-label", "Foreign Key(s)"),
          if (length(fk_r) > 0) {
            tags$div(lapply(fk_r, function(r) {
              to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "?"
              tags$div(style = "font-size:11px;color:#c084fc;padding:1px 0;",
                       paste0(r$from_col, " \u2192 ", r$to_table, ".", to_col))
            }))
          } else {
            tags$span(style = "color:#64748b;font-size:12px;", "none")
          }
        ),
        tags$div(
          class = "panel-section",
          tags$div(class = "panel-label", paste0("Columns (", ncol(df), ")")),
          tags$div(style = "display:flex;flex-wrap:wrap;gap:3px;", col_chips)
        )
      )
    })

    invisible(NULL)
  })
}
