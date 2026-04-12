# ============================================================
# mod_detection.R — Detection Controls & Scan Triage
# ============================================================

#' Detection controls module UI
#' @noRd
mod_detection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "section-title", "02 // Detection Method"),
    selectInput(
      ns("detect_method"),
      NULL,
      choices = c(
        "All signals (naming + content)" = "both",
        "Naming conventions only"        = "naming",
        "Content analysis only"          = "content",
        "Manual only"                    = "manual"
      ),
      selected = "both"
    ),
    selectInput(
      ns("min_confidence"),
      "Minimum confidence:",
      choices = c("low", "medium", "high"),
      selected = "medium"
    ),
    div(
      style = "font-size: 11px; color: var(--text-faint); margin-top: -6px; line-height: 1.5;",
      "Naming: detects columns like ",
      tags$code("id"), ", ", tags$code("{table}_id"),
      " \u2014 Content: uses value overlap, distribution similarity, format fingerprint, and more."
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'both' || input['%s'] == 'content'",
        ns("detect_method"), ns("detect_method")
      ),
      tags$details(
        style = "margin-top: 8px;",
        tags$summary(
          style = "font-size: 11px; color: var(--text-muted); cursor: pointer; font-family: 'IBM Plex Mono', monospace;",
          "Signal toggles"
        ),
        div(
          style = "padding: 8px 0 0 4px;",
          checkboxInput(ns("fl_naming"), "Naming convention", TRUE),
          checkboxInput(ns("fl_overlap"), "Value overlap", TRUE),
          checkboxInput(ns("fl_card"), "Cardinality match", TRUE),
          checkboxInput(ns("fl_fmt"), "Format fingerprint", TRUE),
          checkboxInput(ns("fl_dist"), "Distribution similarity", TRUE),
          checkboxInput(ns("fl_null"), "Null-pattern correlation", FALSE)
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'manual'", ns("detect_method")),
      actionButton(
        ns("btn_run_detection"),
        "\u25b6  Run Detection",
        class = "btn-primary",
        style = "width:100%; margin-top:6px; margin-bottom:2px;"
      )
    ),
    checkboxInput(ns("enable_composite_pk"), "Detect composite keys", value = FALSE),
    div(
      style = "font-size: 11px; color: var(--text-faint); margin-top: -6px; line-height: 1.5;",
      "When no single-column PK is found, try combinations of 2\u20133 columns",
      "whose values together uniquely identify each row."
    )
  )
}

#' Detection controls module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables (read-only in this module)
#' @param fk_cache Mutable environment for FK detection cache
#' @return List of reactive values and counters needed by app_server
#' @noRd
mod_detection_server <- function(id, all_tables_rv, fk_cache) {
  moduleServer(id, function(input, output, session) {
    detection_settings_rv <- reactiveVal(NULL)
    detection_run_counter <- reactiveVal(0L)
    scan_strategy_rv <- reactiveVal("auto")
    last_triage_key <- reactiveVal(NULL)
    triage_btn_counter <- reactiveVal(0L)

    .snapshot_detection_settings <- function() {
      detection_settings_rv(list(
        method        = input$detect_method %||% "both",
        min_conf      = input$min_confidence %||% "medium",
        naming        = isTRUE(input$fl_naming),
        value_overlap = isTRUE(input$fl_overlap),
        cardinality   = isTRUE(input$fl_card),
        format        = isTRUE(input$fl_fmt),
        distribution  = isTRUE(input$fl_dist),
        null_pattern  = isTRUE(input$fl_null)
      ))
      detection_run_counter(detection_run_counter() + 1L)
    }

    observeEvent(input$btn_run_detection, {
      .snapshot_detection_settings()
      showNotification("\u25b6 Running detection with current settings...", type = "message", duration = 3)
    })

    # ---- Scan triage when tables change ----
    observeEvent(all_tables_rv(), {
      tbls <- all_tables_rv()
      if (length(tbls) < 2) {
        scan_strategy_rv("auto")
        .snapshot_detection_settings()
        return()
      }

      triage_key <- paste(sort(names(tbls)), collapse = ",")
      if (identical(triage_key, last_triage_key())) return()
      last_triage_key(triage_key)

      fk_cache$key <- NULL
      fk_cache$result <- list()

      est <- estimate_scan_complexity(tbls)
      if (est$tier == "fast") {
        scan_strategy_rv("auto")
        .snapshot_detection_settings()
        return()
      }

      scan_strategy_rv("pending")
      time_str <- if (est$est_time_sec < 60) {
        paste0("~", ceiling(est$est_time_sec), " seconds")
      } else {
        paste0("~", round(est$est_time_sec / 60, 1), " minutes")
      }
      tier_color <- if (est$tier == "moderate") "#facc15" else "#f87171"

      showModal(modalDialog(
        title = tagList(
          tags$span(
            style = paste0("color:", tier_color, ";font-family:'IBM Plex Mono',monospace;"),
            if (est$tier == "moderate") "\u26a0 Moderate schema size" else "\u26a0 Large schema detected"
          )
        ),
        tags$div(
          style = "font-family:'IBM Plex Mono',monospace;font-size:12px;line-height:1.8;",
          tags$table(
            style = "width:100%;border-collapse:collapse;margin-bottom:12px;",
            tags$tr(
              tags$td(style = "color:#64748b;padding:4px 10px;", "Tables"),
              tags$td(style = "color:#e2e8f0;padding:4px 10px;font-weight:700;", est$n_tables)
            ),
            tags$tr(
              tags$td(style = "color:#64748b;padding:4px 10px;", "Total columns"),
              tags$td(style = "color:#e2e8f0;padding:4px 10px;font-weight:700;", est$total_cols)
            ),
            tags$tr(
              tags$td(style = "color:#64748b;padding:4px 10px;", "Total rows"),
              tags$td(style = "color:#e2e8f0;padding:4px 10px;font-weight:700;",
                      format(est$total_rows, big.mark = ","))
            ),
            tags$tr(
              tags$td(style = "color:#64748b;padding:4px 10px;", "Est. pair comparisons"),
              tags$td(style = "color:#e2e8f0;padding:4px 10px;font-weight:700;",
                      format(est$est_pairs, big.mark = ","))
            ),
            tags$tr(
              tags$td(style = "color:#64748b;padding:4px 10px;", "Est. time (full scan)"),
              tags$td(style = paste0("color:", tier_color, ";padding:4px 10px;font-weight:700;"), time_str)
            )
          ),
          tags$p(style = "color:#94a3b8;font-size:11px;margin-top:8px;",
                 "How would you like to scan for relationships?")
        ),
        footer = tagList(
          actionButton(session$ns("triage_full"), "\u25b6 Full scan (all signals)",
                       class = "btn-add", style = "margin-right:6px;"),
          actionButton(session$ns("triage_naming"), "\u26a1 Quick scan (naming only)",
                       class = "btn-add",
                       style = "margin-right:6px;background:#1a1a00;color:#facc15;border:1px solid #854d0e;"),
          actionButton(session$ns("triage_skip"), "\u23ed Skip auto-detection",
                       class = "btn-danger-soft", style = "width:auto;")
        ),
        easyClose = FALSE, size = "s"
      ))
    })

    observeEvent(input$triage_full, {
      removeModal()
      scan_strategy_rv("full")
      .snapshot_detection_settings()
      triage_btn_counter(triage_btn_counter() + 1L)
      showNotification("\u25b6 Running full scan with all signals...", type = "message", duration = 3)
    })
    observeEvent(input$triage_naming, {
      removeModal()
      scan_strategy_rv("naming_only")
      .snapshot_detection_settings()
      triage_btn_counter(triage_btn_counter() + 1L)
      showNotification("\u26a1 Running quick scan (naming conventions only)...", type = "message", duration = 3)
    })
    observeEvent(input$triage_skip, {
      removeModal()
      scan_strategy_rv("skip")
      triage_btn_counter(triage_btn_counter() + 1L)
      showNotification("\u23ed Auto-detection skipped. Add relationships manually.", type = "warning", duration = 5)
    })

    list(
      scan_strategy_rv      = scan_strategy_rv,
      detection_settings_rv = detection_settings_rv,
      detection_run_counter = detection_run_counter,
      triage_btn_counter    = triage_btn_counter,
      enable_composite_pk   = reactive(isTRUE(input$enable_composite_pk)),
      detect_method         = reactive(input$detect_method %||% "both")
    )
  })
}
