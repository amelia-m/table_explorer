# ============================================================
# Table Relationship Explorer — Shiny App
# Deploy to Posit Connect (Free Plan)
# ============================================================

library(shiny)
library(visNetwork)
library(DT)
library(shinythemes)
library(janitor)

# Remove file upload size limit when running locally (Shiny default is 5 MB)
# On deployed servers (Posit Connect, etc.) the proxy may still impose its own limit
options(shiny.maxRequestSize = Inf)

# Source modular components
source("inference.R")
source("file_readers.R")
source("export_utils.R")
source("db_connectors.R")

# ============================================================
# Visualization
# ============================================================

build_network <- function(tables, rels, pk_map, composite_pk_map = NULL) {
  tnames <- names(tables)
  name_to_id <- setNames(seq_along(tnames), tnames)

  nodes <- data.frame(
    id = seq_along(tnames),
    label = vapply(
      tnames,
      function(t) {
        df <- tables[[t]]
        paste0(
          t,
          "\n",
          format(nrow(df), big.mark = ","),
          " rows | ",
          ncol(df),
          " cols"
        )
      },
      character(1)
    ),
    title = vapply(
      tnames,
      function(t) {
        df <- tables[[t]]
        pks <- pk_map[[t]]
        cpk_groups <- if (!is.null(composite_pk_map)) composite_pk_map[[t]] else list()
        cpk_cols <- unique(unlist(cpk_groups))
        fkr <- Filter(function(r) r$from_table == t, rels)
        fk_s <- if (length(fkr) > 0) {
          paste(
            vapply(
              fkr,
              function(r) paste0(r$from_col, " → ", r$to_table),
              character(1)
            ),
            collapse = "<br>"
          )
        } else {
          "none"
        }
        pk_s <- if (length(pks) > 0) {
          paste(pks, collapse = ", ")
        } else if (length(cpk_groups) > 0) {
          paste(
            vapply(cpk_groups, function(g) paste0("(", paste(g, collapse = " + "), ")"), character(1)),
            collapse = " | "
          )
        } else {
          "none detected"
        }
        paste0(
          "<div style='",
          "background:#0f172a;",
          "color:#e2e8f0;",
          "font-family:IBM Plex Mono,monospace;",
          "font-size:12px;",
          "padding:14px 16px;",
          "min-width:260px;",
          "max-width:420px;",
          "border:1px solid #1e3a5f;",
          "border-radius:8px;",
          "line-height:1.7;",
          "box-shadow:0 4px 24px rgba(0,0,0,0.6);",
          "'>",
          "<div style='font-size:14px;font-weight:700;color:#60a5fa;margin-bottom:8px;'>",
          t,
          "</div>",
          "<div style='display:flex;gap:14px;margin-bottom:8px;'>",
          "<span style='color:#4ade80;'>&#9632; ",
          format(nrow(df), big.mark = ","),
          " rows</span>",
          "<span style='color:#93c5fd;'>&#9632; ",
          ncol(df),
          " cols</span>",
          "</div>",
          "<div style='border-top:1px solid #1e3a5f;padding-top:8px;margin-bottom:6px;'>",
          "<span style='color:#64748b;font-size:10px;letter-spacing:1px;'>PRIMARY KEY</span><br>",
          "<span style='color:#fbbf24;'>",
          pk_s,
          "</span>",
          "</div>",
          "<div style='border-top:1px solid #1e3a5f;padding-top:8px;margin-bottom:6px;'>",
          "<span style='color:#64748b;font-size:10px;letter-spacing:1px;'>FOREIGN KEYS</span><br>",
          "<span style='color:#c084fc;'>",
          gsub("<br>", "<br>", fk_s),
          "</span>",
          "</div>",
          "<div style='border-top:1px solid #1e3a5f;padding-top:8px;'>",
          "<span style='color:#64748b;font-size:10px;letter-spacing:1px;'>COLUMNS</span>",
          "<div style='display:flex;flex-wrap:wrap;gap:3px;margin-top:6px;'>",
          {
            col_names <- names(df)
            show_cols <- if (length(col_names) > 12) col_names[1:12] else col_names
            overflow <- if (length(col_names) > 12) {
              paste0("<span style='color:#64748b;font-size:9px;'> +", length(col_names) - 12, " more</span>")
            } else ""
            paste0(paste(
              vapply(
                show_cols,
                function(cn) {
                  chip_bg <- if (cn %in% pks || cn %in% cpk_cols) {
                    "background:#2a1a00;color:#fbbf24;border-color:#78350f;"
                  } else if (
                    cn %in% vapply(fkr, `[[`, character(1), "from_col")
                  ) {
                    "background:#1a0a2e;color:#c084fc;border-color:#4c1d95;"
                  } else {
                    "background:rgba(255,255,255,0.04);color:#94a3b8;border-color:#1e3a5f;"
                  }
                  paste0(
                    "<span style='",
                    chip_bg,
                    "border:1px solid;border-radius:4px;padding:1px 6px;font-size:10px;white-space:nowrap;'>",
                    cn,
                    "</span>"
                  )
                },
                character(1)
              ),
              collapse = ""
            ), overflow)
          },
          "</div>",
          "</div>",
          "</div>"
        )
      },
      character(1)
    ),
    color.background = "#1E3A5F",
    color.border = "#4E79A7",
    color.highlight.background = "#2A5298",
    color.highlight.border = "#63B3ED",
    font.color = "white",
    font.size = 13,
    shape = "box",
    shadow = TRUE,
    stringsAsFactors = FALSE
  )

  edge_method_color <- c(
    naming          = "#4ADE80",
    name_similarity = "#2DD4BF",
    value_overlap   = "#FB923C",
    cardinality     = "#FACC15",
    format          = "#60A5FA",
    distribution    = "#A78BFA",
    null_pattern    = "#F0ABFC",
    content         = "#FB923C",
    manual          = "#F87171",
    schema          = "#67E8F9"
  )

  if (length(rels) == 0) {
    edges <- data.frame(
      from = integer(0),
      to = integer(0),
      label = character(0),
      title = character(0),
      arrows = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    edges <- do.call(
      rbind,
      lapply(rels, function(r) {
        to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "?"
        ecol <- edge_method_color[r$detected_by]
        if (is.na(ecol)) ecol <- "#FB923C"

        # Confidence-based edge styling
        conf <- if (!is.null(r$confidence)) r$confidence else "medium"
        score_val <- if (!is.null(r$score)) r$score else NA
        conf_opacity <- switch(conf, high = 0.95, medium = 0.60, low = 0.28, 0.60)
        conf_width <- switch(conf, high = 2.5, medium = 1.5, low = 0.8, 1.5)

        # Confidence line for tooltip
        conf_html <- ""
        if (!is.na(score_val)) {
          conf_color <- switch(conf, high = "#4ade80", medium = "#facc15", low = "#f87171", "#94a3b8")
          conf_html <- paste0(
            "<br><span style='color:", conf_color, ";font-size:10px;'>",
            "&#9679; ", conf, " (", round(score_val * 100), "%)</span>"
          )
        }

        # Reasons for tooltip
        reasons_html <- ""
        if (!is.null(r$reasons) && length(r$reasons) > 0) {
          reasons_html <- paste0(
            "<br><span style='color:#94a3b8;font-size:9px;'>",
            paste(r$reasons, collapse = " &bull; "),
            "</span>"
          )
        }

        data.frame(
          from = name_to_id[[r$from_table]],
          to = name_to_id[[r$to_table]],
          label = r$from_col,
          title = paste0(
            "<div style='",
            "background:#0f172a;color:#e2e8f0;",
            "font-family:IBM Plex Mono,monospace;font-size:12px;",
            "padding:10px 14px;border:1px solid #1e3a5f;",
            "border-radius:6px;line-height:1.8;",
            "box-shadow:0 4px 16px rgba(0,0,0,0.6);",
            "'>",
            "<span style='color:#94a3b8;'>",
            r$from_table,
            ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>",
            r$from_col,
            "</span>",
            "<span style='color:#475569;'> &#8594; </span>",
            "<span style='color:#94a3b8;'>",
            r$to_table,
            ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>",
            to_col,
            "</span><br>",
            "<span style='color:",
            ecol,
            ";font-size:10px;letter-spacing:0.5px;'>&#9632; ",
            toupper(r$detected_by),
            "</span>",
            conf_html,
            reasons_html,
            "</div>"
          ),
          arrows = "to",
          color.color = ecol,
          color.highlight = ecol,
          color.opacity = conf_opacity,
          width = conf_width,
          dashes = (conf == "low"),
          font.size = 10,
          font.color = "#e2e8f0",
          font.strokeWidth = 3,
          font.strokeColor = "#1a202c",
          stringsAsFactors = FALSE
        )
      })
    )
  }

  list(nodes = nodes, edges = edges)
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$style(HTML(
      '
    @import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600;700&display=swap");

    /* ── CSS variables: dark (default) ───────────────────────── */
    :root {
      --bg-base:       #0f172a;
      --bg-surface:    #1e293b;
      --bg-inset:      #0f172a;
      --border:        #1e3a5f;
      --border-sub:    #334155;
      --text-primary:  #e2e8f0;
      --text-secondary:#94a3b8;
      --text-muted:    #64748b;
      --text-faint:    #475569;
      --accent:        #60a5fa;
      --accent-soft:   #93c5fd;
      --accent-bg:     #0c1a2e;
      --nav-btn-bg:    #1e3a5f;
      --nav-btn-icon:  #60a5fa;
      --nav-btn-hover: #2a5298;
      --nav-btn-border:#334155;
      --empty-text:    #475569;
      --tbl-card-bg:   #0f172a;
      --tbl-row-hover: #1e293b;
      --rel-border:    #1e293b;
      --code-bg:       #0f172a;
    }
    /* ── CSS variables: light ─────────────────────────────────── */
    body.light-mode {
      --bg-base:       #f1f5f9;
      --bg-surface:    #ffffff;
      --bg-inset:      #f8fafc;
      --border:        #cbd5e1;
      --border-sub:    #e2e8f0;
      --text-primary:  #0f172a;
      --text-secondary:#475569;
      --text-muted:    #64748b;
      --text-faint:    #94a3b8;
      --accent:        #2563eb;
      --accent-soft:   #1d4ed8;
      --accent-bg:     #eff6ff;
      --nav-btn-bg:    #e2e8f0;
      --nav-btn-icon:  #2563eb;
      --nav-btn-hover: #bfdbfe;
      --nav-btn-border:#cbd5e1;
      --empty-text:    #94a3b8;
      --tbl-card-bg:   #f8fafc;
      --tbl-row-hover: #f1f5f9;
      --rel-border:    #e2e8f0;
      --code-bg:       #e2e8f0;
    }

    /* ── Base ─────────────────────────────────────────────────── */
    body {
      background: var(--bg-base);
      font-family: "IBM Plex Sans", sans-serif;
      color: var(--text-primary);
      transition: background 0.25s, color 0.25s;
    }

    /* ── Header ───────────────────────────────────────────────── */
    .app-header {
      background: var(--bg-surface);
      border-bottom: 1px solid var(--border);
      padding: 16px 24px;
      margin-bottom: 20px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .app-header h2 {
      font-family: "IBM Plex Mono", monospace;
      color: var(--accent);
      font-size: 22px;
      font-weight: 600;
      margin: 0 0 2px 0;
      letter-spacing: -0.5px;
    }
    .app-header p { color: var(--text-muted); font-size: 13px; margin: 0; }

    /* ── Theme toggle ─────────────────────────────────────────── */
    #theme-toggle {
      display: flex;
      align-items: center;
      gap: 8px;
      background: var(--bg-inset);
      border: 1px solid var(--border);
      border-radius: 20px;
      padding: 6px 14px 6px 10px;
      cursor: pointer;
      font-family: "IBM Plex Mono", monospace;
      font-size: 12px;
      color: var(--text-secondary);
      transition: all 0.2s;
      white-space: nowrap;
      flex-shrink: 0;
    }
    #theme-toggle:hover { background: var(--bg-surface); color: var(--accent); border-color: var(--accent); }
    #theme-toggle .toggle-track {
      width: 32px; height: 18px;
      background: var(--border-sub);
      border-radius: 9px;
      position: relative;
      transition: background 0.2s;
      flex-shrink: 0;
    }
    body.light-mode #theme-toggle .toggle-track { background: var(--accent); }
    #theme-toggle .toggle-thumb {
      width: 12px; height: 12px;
      background: #fff;
      border-radius: 50%;
      position: absolute;
      top: 3px; left: 3px;
      transition: left 0.2s;
      box-shadow: 0 1px 3px rgba(0,0,0,0.3);
    }
    body.light-mode #theme-toggle .toggle-thumb { left: 17px; }
    #theme-toggle .toggle-icon { font-size: 14px; line-height: 1; }

    /* ── Sidebar ──────────────────────────────────────────────── */
    .sidebar-box {
      background: var(--bg-surface);
      border: 1px solid var(--border);
      border-radius: 10px;
      padding: 18px;
    }
    .sidebar-box .section-title {
      font-family: "IBM Plex Mono", monospace;
      font-size: 11px; font-weight: 600;
      letter-spacing: 1.5px; text-transform: uppercase;
      color: var(--accent);
      margin-bottom: 10px; margin-top: 4px;
    }
    .sidebar-box hr { border-color: var(--border); margin: 14px 0; }

    /* ── Form inputs ──────────────────────────────────────────── */
    label { color: var(--text-secondary) !important; font-size: 12px !important; }
    .form-control, .selectize-input {
      background: var(--bg-inset) !important;
      border: 1px solid var(--border-sub) !important;
      color: var(--text-primary) !important;
      border-radius: 6px !important; font-size: 13px !important;
    }
    .form-control:focus { border-color: var(--accent) !important; box-shadow: 0 0 0 2px rgba(96,165,250,0.2) !important; }
    .selectize-dropdown { background: var(--bg-surface) !important; border: 1px solid var(--border-sub) !important; }
    .selectize-dropdown-content .option { color: var(--text-primary) !important; font-size: 13px; }
    .selectize-dropdown-content .option.active { background: var(--border) !important; }

    /* ── Sidebar buttons ──────────────────────────────────────── */
    .btn-add { background: #166534; border: none; color: #4ade80; font-size: 12px; border-radius: 6px; width: 100%; padding: 7px; margin-top: 6px; }
    .btn-add:hover { background: #14532d; color: #86efac; }
    .btn-danger-soft { background: var(--bg-inset); border: 1px solid #7f1d1d; color: #f87171; font-size: 12px; border-radius: 6px; width: 100%; padding: 6px; margin-top: 4px; }
    .btn-danger-soft:hover { background: #450a0a; color: #fca5a5; }
    .btn-file { background: var(--border) !important; border: none !important; color: var(--accent-soft) !important; font-size: 12px !important; }
    .shiny-input-container > .input-group .form-control { font-size: 12px; }

    /* ── Tabs ─────────────────────────────────────────────────── */
    .nav-tabs { border-bottom: 1px solid var(--border) !important; background: transparent; }
    .nav-tabs > li > a {
      font-family: "IBM Plex Mono", monospace; font-size: 12px;
      color: var(--text-muted) !important; background: transparent !important;
      border: none !important; padding: 10px 16px; border-radius: 6px 6px 0 0 !important;
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
      color: var(--accent) !important;
      background: var(--bg-surface) !important;
      border-bottom: 2px solid var(--accent) !important;
    }
    .tab-content {
      background: var(--bg-surface); border: 1px solid var(--border);
      border-top: none; border-radius: 0 0 10px 10px; padding: 16px;
    }

    /* ── Empty state ──────────────────────────────────────────── */
    .empty-state { text-align: center; padding: 80px 40px; color: var(--text-faint); }
    .empty-state h4 { color: var(--empty-text); font-family: "IBM Plex Mono", monospace; }

    /* ── Stat pills ───────────────────────────────────────────── */
    .pill { display: inline-block; padding: 3px 10px; border-radius: 20px; font-size: 12px; font-family: "IBM Plex Mono", monospace; margin: 2px; }
    .pill-rows  { background: #0c2a1a; color: #4ade80; border: 1px solid #14532d; }
    .pill-cols  { background: #0c1a2e; color: #60a5fa; border: 1px solid #1e3a5f; }
    .pill-pk    { background: #2a1a00; color: #fbbf24; border: 1px solid #78350f; }
    .pill-fk    { background: #1a0a2e; color: #c084fc; border: 1px solid #4c1d95; }
    .pill-warn  { background: #1a0a0a; color: #f87171; border: 1px solid #7f1d1d; }
    body.light-mode .pill-rows { background:#dcfce7; color:#15803d; border-color:#bbf7d0; }
    body.light-mode .pill-cols { background:#dbeafe; color:#1d4ed8; border-color:#bfdbfe; }
    body.light-mode .pill-pk   { background:#fef9c3; color:#854d0e; border-color:#fde68a; }
    body.light-mode .pill-fk   { background:#f3e8ff; color:#6b21a8; border-color:#d8b4fe; }
    body.light-mode .pill-warn { background:#fee2e2; color:#b91c1c; border-color:#fca5a5; }

    /* ── Table cards ──────────────────────────────────────────── */
    .tbl-card { background: var(--tbl-card-bg); border: 1px solid var(--border); border-radius: 8px; padding: 16px; margin-bottom: 14px; }
    .tbl-card-title { font-family: "IBM Plex Mono", monospace; font-size: 15px; color: var(--accent); margin: 0 0 10px 0; }

    /* ── DataTables ───────────────────────────────────────────── */
    table.dataTable { background: var(--tbl-card-bg) !important; color: var(--text-secondary) !important; border-color: var(--border) !important; }
    table.dataTable thead th { background: var(--bg-surface) !important; color: var(--text-secondary) !important; border-color: var(--border-sub) !important; font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 0.5px; }
    table.dataTable tbody tr { background: var(--tbl-card-bg) !important; }
    table.dataTable tbody tr:hover { background: var(--tbl-row-hover) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--text-muted) !important; }
    .dataTables_wrapper .dataTables_info { color: var(--text-faint) !important; font-size: 12px; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: var(--border) !important; color: var(--accent) !important; border-color: var(--border) !important; }

    /* ── Relationships ────────────────────────────────────────── */
    .rel-row { display: flex; align-items: center; gap: 8px; padding: 8px 0; border-bottom: 1px solid var(--rel-border); font-size: 13px; }
    .rel-row:last-child { border-bottom: none; }
    .rel-table { font-weight: 600; color: var(--accent); font-family: "IBM Plex Mono", monospace; }
    .rel-col   { color: var(--text-secondary); font-family: "IBM Plex Mono", monospace; font-size: 12px; }
    .rel-arrow { color: var(--border-sub); font-size: 16px; }
    .rel-method { font-size: 10px; padding: 2px 7px; border-radius: 10px; font-family: "IBM Plex Mono", monospace; margin-left: auto; }
    .m-naming          { background: #0c2a1a; color: #4ade80; }
    .m-name_similarity { background: #0c2a2a; color: #2dd4bf; }
    .m-value_overlap   { background: #1a1000; color: #fb923c; }
    .m-cardinality     { background: #1a1a00; color: #facc15; }
    .m-format          { background: #0a1a2e; color: #60a5fa; }
    .m-distribution    { background: #1a0a2e; color: #a78bfa; }
    .m-null_pattern    { background: #1a1020; color: #f0abfc; }
    .m-content         { background: #1a1000; color: #fb923c; }
    .m-manual          { background: #1a0a0a; color: #f87171; }
    .m-schema          { background: #0a1a1a; color: #67e8f9; }
    body.light-mode .m-naming          { background:#dcfce7; color:#15803d; }
    body.light-mode .m-name_similarity { background:#ccfbf1; color:#0d9488; }
    body.light-mode .m-value_overlap   { background:#fff7ed; color:#c2410c; }
    body.light-mode .m-cardinality     { background:#fefce8; color:#a16207; }
    body.light-mode .m-format          { background:#dbeafe; color:#1d4ed8; }
    body.light-mode .m-distribution    { background:#ede9fe; color:#6d28d9; }
    body.light-mode .m-null_pattern    { background:#fae8ff; color:#a21caf; }
    body.light-mode .m-content         { background:#fff7ed; color:#c2410c; }
    body.light-mode .m-manual          { background:#fee2e2; color:#b91c1c; }
    body.light-mode .m-schema          { background:#cffafe; color:#0e7490; }

    /* Confidence indicators */
    .conf-dot { display:inline-block; width:8px; height:8px; border-radius:50%; margin-right:5px; }
    .conf-high   { background:#4ade80; }
    .conf-medium { background:#facc15; }
    .conf-low    { background:#f87171; }
    .conf-label  { font-size:10px; color:#94a3b8; margin-left:2px; font-family:"IBM Plex Mono",monospace; }
    .signal-chips { display:inline-flex; gap:4px; flex-wrap:wrap; margin-left:8px; }
    .signal-chip  { font-size:9px; padding:1px 5px; border-radius:6px; background:rgba(255,255,255,0.06); color:#94a3b8; font-family:"IBM Plex Mono",monospace; }
    body.light-mode .signal-chip { background:rgba(0,0,0,0.06); color:#475569; }
    body.light-mode .conf-label  { color:#475569; }
    .rel-section-hdr { font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 1.5px; text-transform: uppercase; padding: 10px 0 4px; color: var(--text-faint); }

    /* ── Legend ───────────────────────────────────────────────── */
    .legend { display: flex; gap: 16px; padding: 8px 0 12px; flex-wrap: wrap; }
    .legend-item { display: flex; align-items: center; gap: 6px; font-size: 12px; color: var(--text-muted); font-family: "IBM Plex Mono", monospace; }
    .legend-dot { width: 10px; height: 10px; border-radius: 50%; }

    /* ── Download button ──────────────────────────────────────── */
    .dl-btn { background: var(--accent-bg); border: 1px solid var(--border); color: var(--accent); font-size: 12px; border-radius: 6px; padding: 7px 14px; font-family: "IBM Plex Mono", monospace; }
    .dl-btn:hover { background: var(--border); color: var(--accent-soft); }

    /* ── visNetwork container ─────────────────────────────────── */
    #erd_plot { border-radius: 8px; overflow: hidden; }

    /* ── visNetwork navigation buttons ───────────────────────── */
    .vis-navigation .vis-button {
      width: 30px !important;
      height: 30px !important;
      border-radius: 6px !important;
      background-color: var(--nav-btn-bg) !important;
      border: 1px solid var(--nav-btn-border) !important;
      background-image: none !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      cursor: pointer !important;
      transition: background 0.15s, border-color 0.15s !important;
      box-shadow: none !important;
      opacity: 1 !important;
    }
    .vis-navigation .vis-button:hover {
      background-color: var(--nav-btn-hover) !important;
      border-color: var(--accent) !important;
    }
    /* Replace default background-image icons with pseudo-element glyphs */
    .vis-navigation .vis-button::after {
      content: "";
      display: block;
      width: 10px; height: 10px;
      border: 2px solid var(--nav-btn-icon);
      border-radius: 1px;
    }
    .vis-navigation .vis-button.vis-up::after    { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(-135deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-down::after  { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(45deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-left::after  { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(135deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-right::after { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(-45deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-zoomIn::after  { content: "+"; border: none; color: var(--nav-btn-icon); font-size: 18px; font-weight: 700; font-family: monospace; line-height: 1; width: auto; height: auto; }
    .vis-navigation .vis-button.vis-zoomOut::after { content: "−"; border: none; color: var(--nav-btn-icon); font-size: 20px; font-weight: 700; font-family: monospace; line-height: 1; width: auto; height: auto; }
    .vis-navigation .vis-button.vis-zoomExtends::after { content: "⊡"; border: none; color: var(--nav-btn-icon); font-size: 16px; line-height: 1; width: auto; height: auto; }

    /* hint text below graph */
    .erd-hint { font-size: 11px; color: var(--text-faint); margin-top: 8px; }

    /* code tags in sidebar */
    code { background: var(--code-bg) !important; color: var(--accent) !important; padding: 1px 4px; border-radius: 3px; }

    /* Loaded tables list */
    .loaded-table-item {
      display: flex; align-items: center; justify-content: space-between;
      padding: 5px 8px; margin: 3px 0;
      background: var(--bg-inset); border: 1px solid var(--border);
      border-radius: 5px; font-size: 11px; font-family: "IBM Plex Mono", monospace;
      color: var(--text-secondary);
    }
    .loaded-table-item .tbl-name { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .loaded-table-item .tbl-meta { color: var(--text-faint); font-size: 10px; margin: 0 6px; white-space: nowrap; }
    .loaded-table-item .btn-remove {
      background: none; border: none; color: #f87171; cursor: pointer;
      font-size: 13px; padding: 0 2px; line-height: 1; flex-shrink: 0;
    }
    .loaded-table-item .btn-remove:hover { color: #fca5a5; }
    .loaded-tables-empty { font-size: 11px; color: var(--text-faint); font-family: "IBM Plex Mono", monospace; padding: 4px 0; }
    .cache-badge {
      display: inline-block; font-size: 10px; font-family: "IBM Plex Mono", monospace;
      padding: 1px 6px; border-radius: 10px; margin-left: 6px;
      background: var(--bg-inset); color: var(--text-faint); border: 1px solid var(--border);
      vertical-align: middle;
    }
    .cache-badge.fresh { color: #4ade80; border-color: #14532d; background: #0c2a1a; }

    /* ── Sticky node panel internals ─────────────────────────── */
    #node-panel-overlay .panel-section {
      padding: 12px 16px; border-bottom: 1px solid #1e3a5f;
    }
    #node-panel-overlay .panel-section:last-child { border-bottom: none; }
    #node-panel-overlay .panel-label {
      font-size: 10px; letter-spacing: 1px; color: #64748b;
      text-transform: uppercase; margin-bottom: 6px;
    }
    #node-panel-overlay .col-chip {
      display: inline-block; margin: 2px; padding: 2px 7px;
      border-radius: 4px; font-size: 11px; border: 1px solid;
    }
    body.light-mode #node-panel-overlay { background: #ffffff !important; border-color: #cbd5e1 !important; }
    body.light-mode #node-panel-overlay .panel-section { border-color: #e2e8f0 !important; }
    body.light-mode #node-panel-overlay .panel-label { color: #94a3b8; }

    /* ── Name Changes tab ─────────────────────────────────────── */
    .rename-summary {
      font-size: 12px; color: var(--text-faint); font-family: "IBM Plex Mono", monospace;
      margin-bottom: 12px; padding: 8px 12px;
      background: var(--bg-inset); border: 1px solid var(--border); border-radius: 6px;
    }
    .rename-summary b { color: var(--accent); }
  '
    )),
    tags$script(HTML(
      '
      document.addEventListener("DOMContentLoaded", function() {
        // Theme toggle
        var btn = document.getElementById("theme-toggle");
        if (btn) {
          btn.addEventListener("click", function() {
            document.body.classList.toggle("light-mode");
            var isLight = document.body.classList.contains("light-mode");
            document.getElementById("toggle-label").textContent = isLight ? "Dark mode" : "Light mode";
          });
        }
        // Per-table remove buttons (delegated — buttons are rendered dynamically)
        document.body.addEventListener("click", function(e) {
          if (e.target.classList.contains("btn-remove")) {
            Shiny.setInputValue("remove_table_name", e.target.dataset.tname, {priority: "event"});
          }
        });
        // Sticky node panel close button
        document.body.addEventListener("click", function(e) {
          if (e.target.id === "node-panel-close") {
            document.getElementById("node-panel-overlay").style.display = "none";
            Shiny.setInputValue("vis_close_panel", Math.random(), {priority: "event"});
          }
        });
        // Light mode: flip node panel to light
        var observer = new MutationObserver(function() {
          var panel = document.getElementById("node-panel-overlay");
          if (!panel) return;
          if (document.body.classList.contains("light-mode")) {
            panel.style.background = "#ffffff";
            panel.style.borderColor = "#cbd5e1";
            panel.style.color = "#0f172a";
          } else {
            panel.style.background = "#0f172a";
            panel.style.borderColor = "#1e3a5f";
            panel.style.color = "#e2e8f0";
          }
        });
        observer.observe(document.body, {attributes: true, attributeFilter: ["class"]});
      });
      // Called by visNetwork click event via Shiny.setInputValue
      function showNodePanel(nodeId) {
        if (!nodeId) return;
        var panel = document.getElementById("node-panel-overlay");
        if (panel) panel.style.display = "block";
      }
    '
    ))
  ),

  # Header
  div(
    class = "app-header",
    div(
      h2("TABLE_RELATIONSHIP_EXPLORER"),
      p(
        "Upload CSV files to discover primary keys, foreign keys, and inter-table links"
      )
    ),
    tags$button(
      id = "theme-toggle",
      div(class = "toggle-track", div(class = "toggle-thumb")),
      span(class = "toggle-icon", "\U0001F319"),
      span(id = "toggle-label", "Light mode")
    )
  ),

  fluidRow(
    style = "margin: 0 12px;",

    # ---- Sidebar ----
    column(
      3,
      div(
        class = "sidebar-box",

        div(class = "section-title", "01 // Upload Tables"),
        fileInput(
          "data_files",
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
          "schema_file",
          NULL,
          multiple = FALSE,
          accept = c(".json", ".yaml", ".yml"),
          placeholder = "No file selected",
          buttonLabel = "Import Schema"
        ),
        uiOutput("loaded_tables_ui"),
        actionButton(
          "btn_clear_tables",
          "✕  Remove All Tables",
          class = "btn-danger-soft"
        ),

        tags$hr(),
        div(class = "section-title", "02 // Detection Method"),
        selectInput(
          "detect_method",
          NULL,
          choices = c(
            "All signals (naming + content)" = "both",
            "Naming conventions only" = "naming",
            "Content analysis only" = "content",
            "Manual only" = "manual"
          ),
          selected = "both"
        ),
        selectInput(
          "min_confidence",
          "Minimum confidence:",
          choices = c("low", "medium", "high"),
          selected = "medium"
        ),
        div(
          style = "font-size: 11px; color: var(--text-faint); margin-top: -6px; line-height: 1.5;",
          "Naming: detects columns like",
          tags$code("id"),
          ",",
          tags$code("{table}_id"),
          "— Content: uses value overlap, distribution similarity, format fingerprint, and more."
        ),
        conditionalPanel(
          "input.detect_method == 'both' || input.detect_method == 'content'",
          tags$details(
            style = "margin-top: 8px;",
            tags$summary(
              style = "font-size: 11px; color: var(--text-muted); cursor: pointer; font-family: 'IBM Plex Mono', monospace;",
              "Signal toggles"
            ),
            div(
              style = "padding: 8px 0 0 4px;",
              checkboxInput("fl_naming", "Naming convention", TRUE),
              checkboxInput("fl_overlap", "Value overlap", TRUE),
              checkboxInput("fl_card", "Cardinality match", TRUE),
              checkboxInput("fl_fmt", "Format fingerprint", TRUE),
              checkboxInput("fl_dist", "Distribution similarity", TRUE),
              checkboxInput("fl_null", "Null-pattern correlation", FALSE)
            )
          )
        ),
        conditionalPanel(
          "input.detect_method != 'manual'",
          actionButton(
            "btn_run_detection",
            "\u25b6  Run Detection",
            class = "btn-primary",
            style = "width:100%; margin-top:6px; margin-bottom:2px;"
          )
        ),
        checkboxInput(
          "enable_composite_pk",
          "Detect composite keys",
          value = FALSE
        ),
        div(
          style = "font-size: 11px; color: var(--text-faint); margin-top: -6px; line-height: 1.5;",
          "When no single-column PK is found, try combinations of 2\u20133 columns",
          "whose values together uniquely identify each row."
        ),

        tags$hr(),
        div(class = "section-title", "03 // Database Connection"),
        selectInput(
          "db_type", NULL,
          choices = c("(select)" = "", db_types),
          selected = ""
        ),
        conditionalPanel(
          "input.db_type != '' && input.db_type != 'sqlite' && input.db_type != 'bigquery'",
          textInput("db_host", "Host:", placeholder = "localhost"),
          textInput("db_port", "Port:", placeholder = "auto"),
          textInput("db_name", "Database:", placeholder = "mydb"),
          textInput("db_user", "User:"),
          passwordInput("db_pass", "Password:"),
          textInput("db_schema", "Schema:", value = "public")
        ),
        conditionalPanel(
          "input.db_type == 'sqlite'",
          fileInput("db_sqlite_file", NULL, accept = c(".db", ".sqlite", ".sqlite3"),
                    buttonLabel = "Select SQLite")
        ),
        conditionalPanel(
          "input.db_type == 'bigquery'",
          textInput("db_bq_project", "Project ID:"),
          textInput("db_bq_dataset", "Dataset:")
        ),
        conditionalPanel(
          "input.db_type == 'sqlserver' || input.db_type == 'snowflake'",
          textInput("db_driver", "ODBC Driver:", placeholder = "auto-detect")
        ),
        conditionalPanel(
          "input.db_type != ''",
          div(
            style = "display:flex;gap:6px;margin-bottom:8px;",
            actionButton("btn_db_connect", "Connect", class = "btn-add"),
            actionButton("btn_db_disconnect", "Disconnect", class = "btn-danger-soft")
          ),
          uiOutput("db_status_ui"),
          uiOutput("db_tables_ui"),
          actionButton("btn_db_load", "Load Selected Tables", class = "btn-add")
        ),

        tags$hr(),
        div(class = "section-title", "04 // Manual Override"),
        div(
          style = "font-size: 11px; color: #475569; margin-bottom: 8px;",
          "Add or override relationships directly."
        ),
        uiOutput("ui_man_from_table"),
        uiOutput("ui_man_from_col"),
        uiOutput("ui_man_to_table"),
        uiOutput("ui_man_to_col"),
        actionButton("btn_add_rel", "＋ Add Relationship", class = "btn-add"),
        actionButton(
          "btn_clear_manual",
          "✕  Clear Manual",
          class = "btn-danger-soft"
        )
      )
    ),

    # ---- Main Panel ----
    column(
      9,
      tabsetPanel(
        id = "main_tabs",

        # --- ERD ---
        tabPanel(
          "ERD Diagram",
          br(),
          conditionalPanel(
            "output.has_tables == 'false'",
            div(
              class = "empty-state",
              div(style = "font-size: 48px; margin-bottom: 16px;", "◫"),
              h4("No tables loaded"),
              p(
                style = "color:#334155; font-size:13px;",
                "Upload one or more CSV files using the sidebar to begin."
              )
            )
          ),
          conditionalPanel(
            "output.has_tables == 'true'",
            div(
              class = "legend",
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#4ADE80;"), "naming"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#2DD4BF;"), "name similarity"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#FB923C;"), "value overlap"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#FACC15;"), "cardinality"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#60A5FA;"), "format"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#A78BFA;"), "distribution"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#F0ABFC;"), "null pattern"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#67E8F9;"), "schema"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#F87171;"), "manual"),
              div(class = "legend-item",
                  div(class = "legend-dot", style = "background:#fff;border:1px dashed #64748b;"), "low confidence (dashed)")
            ),
            div(
              style = "display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:8px;",
              selectInput("erd_layout", "Layout:", width = "160px",
                          choices = c("Force" = "force", "Hierarchical" = "hierarchical", "Circular" = "circular"),
                          selected = "force"),
              sliderInput("spring_length", "Spring length:", width = "200px",
                          min = 80, max = 600, value = 220, step = 20)
            ),
            visNetworkOutput("erd_plot", height = "540px"),
            div(
              class = "erd-hint",
              "drag nodes \u00b7 scroll to zoom \u00b7 hover for details \u00b7 click to highlight connections"
            )
          )
        ),

        # --- Table Details ---
        tabPanel("Table Details", br(), uiOutput("table_details_ui")),

        # --- Relationships ---
        tabPanel(
          "Relationships",
          br(),
          uiOutput("relationships_ui"),
          br(),
          downloadButton("dl_rels", "⬇  Export CSV", class = "dl-btn")
        ),

        # --- Name Changes ---
        tabPanel("Name Changes", br(), uiOutput("rename_log_ui")),

        # --- Export ---
        tabPanel(
          "Export",
          br(),
          div(
            class = "sidebar-box",
            style = "max-width:600px;",
            div(class = "section-title", "Export Formats"),
            div(
              style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:16px;",
              downloadButton("dl_rels_csv", "Relationships CSV", class = "dl-btn"),
              downloadButton("dl_dbt_yaml", "dbt schema.yml", class = "dl-btn"),
              downloadButton("dl_mermaid", "Mermaid ERD", class = "dl-btn")
            ),
            tags$hr(),
            div(class = "section-title", "Session"),
            div(
              style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:8px;",
              downloadButton("dl_session", "Save Session", class = "dl-btn"),
              fileInput("restore_session_file", NULL,
                        accept = ".json",
                        buttonLabel = "Restore Session",
                        placeholder = "No file selected")
            )
          )
        )
      )
    )
  ),

  # ---- Sticky node detail panel (overlay) ----
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
        "\u00d7"
      )
    ),
    uiOutput("node_panel_ui")
  ),
  br()
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  # ============================================================
  # State: accumulated tables (name -> data.frame)
  # ============================================================
  all_tables_rv <- reactiveVal(list())
  rename_log_rv <- reactiveVal(data.frame(
    object_type = character(),
    source = character(),
    original_name = character(),
    cleaned_name = character(),
    stringsAsFactors = FALSE
  ))
  selected_node_rv <- reactiveVal(NULL)
  # metadata store: name -> list(size, mtime, nrow, ncol)
  table_meta_rv <- reactiveVal(list())
  # pending conflict queue: list of conflict descriptor lists
  pending_conflicts_rv <- reactiveVal(list())

  # ---- Add files when fileInput fires ----
  observeEvent(input$data_files, {
    req(input$data_files)
    existing <- all_tables_rv()
    existing_meta <- table_meta_rv()
    n_files <- nrow(input$data_files)

    errors <- character(0)
    notify_fn <- function(msg) { errors <<- c(errors, msg) }

    # Read all files with the multi-format dispatcher
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
            if (length(raw_tbls) == 0) return(list(ok = FALSE, name = fname))
            # Clean table + column names for each table
            entries <- lapply(names(raw_tbls), function(tname) {
              raw_df <- raw_tbls[[tname]]
              clean_df <- janitor::clean_names(raw_df)
              clean_tname <- janitor::make_clean_names(tname)
              list(
                ok = TRUE,
                raw = raw_df,
                clean = clean_df,
                tname = clean_tname,
                raw_tname = tname,
                meta = list(
                  size = fsize,
                  nrow = nrow(clean_df),
                  ncol = ncol(clean_df)
                )
              )
            })
            entries
          },
          error = function(e) {
            notify_fn(paste0("Read error on ", fname, ": ", conditionMessage(e)))
            list(list(ok = FALSE, name = fname))
          }
        )
      })
    })

    # Flatten: each file may produce multiple tables
    flat <- do.call(c, all_results)
    valid <- vapply(flat, function(x) isTRUE(x$ok), logical(1))
    valid_entries <- flat[valid]
    failed_entries <- flat[!valid]

    for (e in errors) {
      showNotification(e, type = "error", duration = 8)
    }
    for (fe in failed_entries) {
      if (!is.null(fe$name)) {
        showNotification(paste0("Could not read: ", fe$name), type = "error", duration = 8)
      }
    }
    if (length(valid_entries) == 0) {
      showNotification("No valid files could be loaded.", type = "error", duration = 8)
      return()
    }

    # ── Classify each incoming table ─────────────────────────────────
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
        same_size <- !is.null(ex) &&
          identical(as.numeric(inc$size), as.numeric(ex$size))
        same_dims <- !is.null(ex) &&
          identical(inc$nrow, ex$nrow) &&
          identical(inc$ncol, ex$ncol)

        if (same_size && same_dims) {
          exact_dupes <- c(exact_dupes, nm)
        } else {
          conflicts[[length(conflicts) + 1]] <- list(
            name = nm,
            rc = entry,
            existing_meta = ex,
            incoming_meta = inc
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
      .apply_new_files(new_files, existing, existing_meta, valid_entries,
                       vapply(valid_entries, `[[`, character(1), "tname"))
    }

    if (length(conflicts) > 0) {
      pending_conflicts_rv(conflicts)
      .show_conflict_modal(conflicts[[1]])
    }
  })

  # ── Helper: apply a named list of rc entries to state ─────────────
  .apply_new_files <- function(
    file_map,
    existing,
    existing_meta,
    valid_rc,
    new_names
  ) {
    merged <- existing
    merged_meta <- existing_meta

    for (nm in names(file_map)) {
      rc <- file_map[[nm]]
      merged[[nm]] <- rc$clean
      merged_meta[[nm]] <- rc$meta
    }
    all_tables_rv(merged)
    table_meta_rv(merged_meta)

    # Rename log — table names + column names
    log_rows <- list()

    # Table name renames
    for (nm in names(file_map)) {
      rc <- file_map[[nm]]
      raw_tname <- rc$raw_tname %||% nm
      if (!is.null(raw_tname) && raw_tname != nm) {
        log_rows[[length(log_rows) + 1]] <- data.frame(
          object_type = "table",
          source = nm,
          original_name = raw_tname,
          cleaned_name = nm,
          stringsAsFactors = FALSE
        )
      }
    }

    # Column name renames
    for (nm in names(file_map)) {
      rc <- file_map[[nm]]
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (any(changed)) {
        log_rows[[length(log_rows) + 1]] <- data.frame(
          object_type = "column",
          source = nm,
          original_name = orig[changed],
          cleaned_name = cleaned[changed],
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
      type = "message",
      duration = 4
    )
  }

  # ── Show conflict resolution modal ────────────────────────────────
  .show_conflict_modal <- function(conflict) {
    nm <- conflict$name
    ex <- conflict$existing_meta
    inc <- conflict$incoming_meta

    fmt_meta <- function(m) {
      if (is.null(m)) {
        return("unknown")
      }
      paste0(
        format(m$nrow, big.mark = ","),
        " rows \u00d7 ",
        m$ncol,
        " cols",
        " | ",
        round(m$size / 1024, 1),
        " KB"
      )
    }

    showModal(modalDialog(
      title = tagList(
        tags$span(
          style = "color:#f59e0b;font-family:'IBM Plex Mono',monospace;",
          paste0("\u26a0 Duplicate table name: '", nm, "'")
        )
      ),
      tags$p(
        style = "font-size:13px;color:#94a3b8;",
        "A table named ",
        tags$b(nm),
        " is already loaded. The incoming file has different metadata:"
      ),
      tags$table(
        style = "width:100%;border-collapse:collapse;font-family:'IBM Plex Mono',monospace;font-size:12px;",
        tags$thead(
          tags$tr(
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#64748b;text-align:left;",
              ""
            ),
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#60a5fa;text-align:left;",
              "Existing"
            ),
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#4ade80;text-align:left;",
              "Incoming"
            )
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(style = "padding:5px 10px;color:#64748b;", "Dimensions"),
            tags$td(
              style = "padding:5px 10px;",
              if (!is.null(ex)) {
                paste0(format(ex$nrow, big.mark = ","), " \u00d7 ", ex$ncol)
              } else {
                "—"
              }
            ),
            tags$td(
              style = "padding:5px 10px;",
              paste0(format(inc$nrow, big.mark = ","), " \u00d7 ", inc$ncol)
            )
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;color:#64748b;", "File size"),
            tags$td(
              style = "padding:5px 10px;",
              if (!is.null(ex)) paste0(round(ex$size / 1024, 1), " KB") else "—"
            ),
            tags$td(
              style = "padding:5px 10px;",
              paste0(round(inc$size / 1024, 1), " KB")
            )
          )
        )
      ),
      br(),
      tags$p(
        style = "font-size:12px;color:#64748b;",
        "How should this be resolved?"
      ),
      footer = tagList(
        actionButton(
          "conflict_overwrite",
          "\u2b06 Replace existing",
          style = "background:#1e3a5f;color:#60a5fa;border:1px solid #334155;font-size:12px;"
        ),
        actionButton(
          "conflict_keep_both",
          "\u2795 Keep both (rename incoming to '",
          tags$code(paste0(nm, "_2")),
          "')",
          style = "background:#0c2a1a;color:#4ade80;border:1px solid #14532d;font-size:12px;"
        ),
        actionButton(
          "conflict_skip",
          "\u274c Skip incoming",
          style = "background:#1a0a0a;color:#f87171;border:1px solid #7f1d1d;font-size:12px;"
        )
      ),
      easyClose = FALSE,
      size = "m"
    ))
  }

  # ── Conflict resolution handlers ──────────────────────────────────
  resolve_conflict <- function(action) {
    conflicts <- pending_conflicts_rv()
    if (length(conflicts) == 0) {
      return()
    }

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
      # Update rename log — table name + column names
      prev <- rename_log_rv()
      prev <- prev[prev$source != nm, , drop = FALSE]
      new_rows <- list()
      raw_tname <- rc$raw_tname %||% nm
      if (!is.null(raw_tname) && raw_tname != nm) {
        new_rows[[length(new_rows) + 1]] <- data.frame(
          object_type = "table", source = nm,
          original_name = raw_tname, cleaned_name = nm,
          stringsAsFactors = FALSE
        )
      }
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (any(changed)) {
        new_rows[[length(new_rows) + 1]] <- data.frame(
          object_type = "column", source = nm,
          original_name = orig[changed], cleaned_name = cleaned[changed],
          stringsAsFactors = FALSE
        )
      }
      if (length(new_rows) > 0) {
        rename_log_rv(rbind(prev, do.call(rbind, new_rows)))
      }
      showNotification(
        paste0("'", nm, "' replaced."),
        type = "message",
        duration = 3
      )
    } else if (action == "keep_both") {
      # Find a free name: nm_2, nm_3, ...
      new_nm <- nm
      suffix <- 2
      while (new_nm %in% names(existing)) {
        new_nm <- paste0(nm, "_", suffix)
        suffix <- suffix + 1
      }
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
          original_name = raw_tname, cleaned_name = new_nm,
          stringsAsFactors = FALSE
        )
      }
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (any(changed)) {
        new_rows[[length(new_rows) + 1]] <- data.frame(
          object_type = "column", source = new_nm,
          original_name = orig[changed], cleaned_name = cleaned[changed],
          stringsAsFactors = FALSE
        )
      }
      if (length(new_rows) > 0) {
        rename_log_rv(rbind(prev, do.call(rbind, new_rows)))
      }
      showNotification(
        paste0("Saved as '", new_nm, "'."),
        type = "message",
        duration = 3
      )
    } else {
      # skip
      showNotification(
        paste0("'", nm, "' incoming file discarded."),
        type = "warning",
        duration = 3
      )
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
  schema_rels_rv <- reactiveVal(list())

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

    for (e in errors) {
      showNotification(e, type = "error", duration = 12)
    }

    # Add schema-defined tables (empty data frames with column names)
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
              original_name = nm, cleaned_name = clean_nm,
              stringsAsFactors = FALSE
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

    # Store schema relationships (clean table names to match)
    if (length(result$relationships) > 0) {
      clean_rels <- lapply(result$relationships, function(r) {
        r$from_table <- janitor::make_clean_names(r$from_table)
        r$to_table <- janitor::make_clean_names(r$to_table)
        r
      })
      schema_rels_rv(clean_rels)
      showNotification(
        paste0("Schema imported: ", length(result$relationships), " relationship(s), ",
               length(result$tables), " table(s)"),
        type = "message", duration = 5
      )
    } else {
      showNotification(
        paste0("Schema imported: ", length(result$tables), " table(s), no relationships found."),
        type = "message", duration = 5
      )
    }
  })
  # ---- Database connection ----
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
    if (nzchar(input$db_port %||% "")) {
      port_val <- as.integer(input$db_port)
    }

    path_val <- ""
    if (type == "sqlite" && !is.null(input$db_sqlite_file)) {
      path_val <- input$db_sqlite_file$datapath
    }

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

      # Store declared FKs
      if (length(meta$fks) > 0) {
        schema_rels_rv(c(schema_rels_rv(), meta$fks))
      }

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
      "db_selected_tables", "Select tables:",
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
    db_tname_renames <- list()

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
            db_tname_renames[[length(db_tname_renames) + 1]] <- data.frame(
              object_type = "table", source = tname,
              original_name = raw_tname, cleaned_name = tname,
              stringsAsFactors = FALSE
            )
          }
          # Column renames
          col_orig <- names(df)
          col_clean <- names(clean_df)
          col_changed <- col_orig != col_clean
          if (any(col_changed)) {
            db_tname_renames[[length(db_tname_renames) + 1]] <- data.frame(
              object_type = "column", source = tname,
              original_name = col_orig[col_changed],
              cleaned_name = col_clean[col_changed],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    })

    all_tables_rv(existing)
    table_meta_rv(existing_meta)

    if (length(db_tname_renames) > 0) {
      prev <- rename_log_rv()
      rename_log_rv(rbind(prev, do.call(rbind, db_tname_renames)))
    }

    showNotification(
      paste0("Loaded ", length(input$db_selected_tables), " table(s) from database."),
      type = "message", duration = 5
    )
  })

  observeEvent(input$remove_table_name, {
    nm <- input$remove_table_name
    tbl <- all_tables_rv()
    tbl[[nm]] <- NULL
    all_tables_rv(tbl)
    meta <- table_meta_rv()
    meta[[nm]] <- NULL
    table_meta_rv(meta)
    log <- rename_log_rv()
    rename_log_rv(log[log$source != nm, , drop = FALSE])
    showNotification(paste0("Removed: ", nm), type = "message", duration = 3)
  })

  # ---- Clear all tables ----
  observeEvent(input$btn_clear_tables, {
    all_tables_rv(list())
    table_meta_rv(list())
    rename_log_rv(data.frame(
      object_type = character(),
      source = character(),
      original_name = character(),
      cleaned_name = character(),
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
            span(
              class = "tbl-meta",
              paste0(format(nrow(df), big.mark = ","), "r × ", ncol(df), "c")
            ),
            tags$button(
              class = "btn-remove",
              `data-tname` = nm,
              title = "Remove",
              "×"
            )
          )
        })
      )
    )
  })

  # ============================================================
  # Caching: FK detection is the expensive step.
  # We store the last digest + result; only recompute on change.
  # ============================================================
  fk_cache <- new.env(parent = emptyenv())
  fk_cache$key <- NULL
  fk_cache$result <- list()

  # ---- Detection settings snapshot ----
  # Stores a snapshot of detection settings; updated on button click or first load.
  # auto_rels_rv reads from this instead of directly from inputs, so changing
  # dropdowns/checkboxes does NOT immediately re-trigger detection.
  detection_settings_rv <- reactiveVal(NULL)
  detection_run_counter <- reactiveVal(0L)

  # Snapshot current UI settings into detection_settings_rv
  .snapshot_detection_settings <- function() {
    detection_settings_rv(list(
      method       = input$detect_method %||% "both",
      min_conf     = input$min_confidence %||% "medium",
      naming       = isTRUE(input$fl_naming),
      value_overlap = isTRUE(input$fl_overlap),
      cardinality  = isTRUE(input$fl_card),
      format       = isTRUE(input$fl_fmt),
      distribution = isTRUE(input$fl_dist),
      null_pattern = isTRUE(input$fl_null)
    ))
    detection_run_counter(detection_run_counter() + 1L)
  }

  # "Run Detection" button click

  observeEvent(input$btn_run_detection, {
    .snapshot_detection_settings()
    showNotification(
      "\u25b6 Running detection with current settings...",
      type = "message", duration = 3
    )
  })

  # ---- Scan triage: estimate complexity and prompt user on large schemas ----
  # States: "auto" (small schema, run immediately), "pending" (waiting for user),
  #         "full", "naming_only", "skip" (user chose)
  scan_strategy_rv <- reactiveVal("auto")
  last_triage_key <- reactiveVal(NULL)
  triage_btn_counter <- reactiveVal(0L)  # forces re-render after each triage choice

  observeEvent(all_tables_rv(), {
    tbls <- all_tables_rv()
    if (length(tbls) < 2) {
      scan_strategy_rv("auto")
      .snapshot_detection_settings()
      return()
    }

    # Only show triage once per table set
    triage_key <- paste(sort(names(tbls)), collapse = ",")
    if (identical(triage_key, last_triage_key())) return()
    last_triage_key(triage_key)

    # Invalidate cache so the new tables get a fresh scan
    fk_cache$key <- NULL
    fk_cache$result <- list()

    est <- estimate_scan_complexity(tbls)

    if (est$tier == "fast") {
      scan_strategy_rv("auto")
      .snapshot_detection_settings()
      return()
    }

    # Block detection until user decides
    scan_strategy_rv("pending")

    # Build time estimate string
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
            tags$td(style = paste0("color:", tier_color, ";padding:4px 10px;font-weight:700;"),
                    time_str)
          )
        ),
        tags$p(style = "color:#94a3b8;font-size:11px;margin-top:8px;",
               "How would you like to scan for relationships?")
      ),
      footer = tagList(
        actionButton("triage_full", "\u25b6 Full scan (all signals)", class = "btn-add",
                     style = "margin-right:6px;"),
        actionButton("triage_naming", "\u26a1 Quick scan (naming only)", class = "btn-add",
                     style = "margin-right:6px;background:#1a1a00;color:#facc15;border:1px solid #854d0e;"),
        actionButton("triage_skip", "\u23ed Skip auto-detection", class = "btn-danger-soft",
                     style = "width:auto;")
      ),
      easyClose = FALSE,
      size = "s"
    ))
  })

  observeEvent(input$triage_full, {
    removeModal()
    scan_strategy_rv("full")
    .snapshot_detection_settings()
    triage_btn_counter(triage_btn_counter() + 1L)
    showNotification(
      "\u25b6 Running full scan with all signals...",
      type = "message", duration = 3
    )
  })
  observeEvent(input$triage_naming, {
    removeModal()
    scan_strategy_rv("naming_only")
    .snapshot_detection_settings()
    triage_btn_counter(triage_btn_counter() + 1L)
    showNotification(
      "\u26a1 Running quick scan (naming conventions only)...",
      type = "message", duration = 3
    )
  })
  observeEvent(input$triage_skip, {
    removeModal()
    scan_strategy_rv("skip")
    triage_btn_counter(triage_btn_counter() + 1L)
    showNotification(
      "\u23ed Auto-detection skipped. Add relationships manually.",
      type = "warning", duration = 5
    )
  })

  # ---- PKs (fast — no caching needed) ----
  pk_map_rv <- reactive({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    method <- if (input$detect_method == "manual") {
      "both"
    } else {
      input$detect_method
    }
    setNames(
      lapply(names(tbls), function(t) detect_pks(tbls[[t]], t, method)),
      names(tbls)
    )
  })

  # ---- Composite PKs (optional, off by default) ----
  composite_pk_map_rv <- reactive({
    if (!isTRUE(input$enable_composite_pk)) {
      return(setNames(vector("list", length(all_tables_rv())), names(all_tables_rv())))
    }
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    setNames(
      lapply(names(tbls), function(t) detect_composite_pks(tbls[[t]], t)),
      names(tbls)
    )
  })

  # ---- FK detection with cache ----
  # Detection only runs when detection_settings_rv is snapshotted (via button
  # click, initial table load, or triage choice). Changing UI dropdowns/toggles
  # does NOT immediately re-trigger detection.
  auto_rels_rv <- reactive({
    tbls <- all_tables_rv()
    strategy <- scan_strategy_rv()
    settings <- detection_settings_rv()
    detection_run_counter()  # explicit dependency: re-fire on snapshot
    triage_btn_counter()     # dependency: re-fire when user picks a triage option
    req(length(tbls) > 0)

    # Block until user decides on large schemas
    if (identical(strategy, "pending")) return(list())

    # If user chose to skip, return empty
    if (identical(strategy, "skip")) return(list())

    # No settings snapshot yet — nothing to run
    if (is.null(settings)) return(list())

    # Determine effective method based on strategy
    method <- if (identical(strategy, "naming_only")) {
      "naming"
    } else {
      settings$method
    }
    min_conf <- settings$min_conf %||% "medium"

    # Build enable_flags from snapshotted settings (overridden by naming_only strategy)
    if (identical(strategy, "naming_only")) {
      flags <- list(
        naming = TRUE, value_overlap = FALSE, cardinality = FALSE,
        format = FALSE, distribution = FALSE, null_pattern = FALSE
      )
    } else {
      flags <- list(
        naming       = isTRUE(settings$naming),
        value_overlap = isTRUE(settings$value_overlap),
        cardinality  = isTRUE(settings$cardinality),
        format       = isTRUE(settings$format),
        distribution = isTRUE(settings$distribution),
        null_pattern = isTRUE(settings$null_pattern)
      )
    }

    # Cache key: table structure + method + confidence + flags + strategy
    key_parts <- paste(
      names(tbls),
      vapply(tbls, nrow, integer(1)),
      vapply(tbls, ncol, integer(1)),
      sep = ":",
      collapse = "|"
    )
    flag_str <- paste(vapply(flags, as.character, character(1)), collapse = "")
    cache_key <- paste0(key_parts, "//", method, "//", min_conf, "//", flag_str, "//", strategy)

    if (!is.null(fk_cache$key) && identical(fk_cache$key, cache_key)) {
      return(fk_cache$result)
    }

    # Auto-sample large tables for FK detection performance
    sampled_tbls <- lapply(tbls, function(df) {
      if (nrow(df) > 10000) {
        set.seed(42)
        sampled <- df[sample(nrow(df), min(10000, nrow(df))), , drop = FALSE]
        attr(sampled, "sampled") <- TRUE
        sampled
      } else {
        df
      }
    })
    names(sampled_tbls) <- names(tbls)

    # Also cap the number of columns per table to avoid combinatorial blow-up
    for (nm in names(sampled_tbls)) {
      df <- sampled_tbls[[nm]]
      if (ncol(df) > 60) {
        # Keep id-like columns + first N columns
        id_cols <- grep("(_id|_key|id$|key$|_code|_num)", names(df), value = TRUE)
        other_cols <- setdiff(names(df), id_cols)
        keep <- union(id_cols, head(other_cols, 60 - length(id_cols)))
        sampled_tbls[[nm]] <- df[, keep, drop = FALSE]
      }
    }

    result <- withProgress(
      message = "Detecting relationships...",
      value = 0.1,
      {
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
              type = "error",
              duration = 8
            )
            list()
          }
        )
      }
    )

    fk_cache$key <- cache_key
    fk_cache$result <- result
    result
  })

  # ---- Manual rels ----
  manual_rels_rv <- reactiveVal(list())

  # ---- Relationship management state ----
  false_positives_rv <- reactiveVal(character(0))
  conf_overrides_rv <- reactiveVal(list())

  rel_key <- function(r) paste(r$from_table, r$from_col, r$to_table, r$to_col, sep = "|")

  all_rels_rv <- reactive({
    raw <- c(auto_rels_rv(), manual_rels_rv(), schema_rels_rv())
    suppressed <- false_positives_rv()
    overrides <- conf_overrides_rv()

    # Filter out suppressed relationships
    filtered <- Filter(function(r) !rel_key(r) %in% suppressed, raw)

    # Apply confidence overrides
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

  # ---- has_tables ----
  output$has_tables <- reactive({
    if (length(all_tables_rv()) > 0) "true" else "false"
  })
  outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

  # ---- Manual relationship UI ----
  output$ui_man_from_table <- renderUI({
    req(length(all_tables_rv()) > 0)
    selectInput(
      "man_from_table",
      "From Table:",
      choices = names(all_tables_rv())
    )
  })
  output$ui_man_from_col <- renderUI({
    tbls <- all_tables_rv()
    req(
      length(tbls) > 0,
      input$man_from_table,
      input$man_from_table %in% names(tbls)
    )
    selectInput(
      "man_from_col",
      "From Column:",
      choices = names(tbls[[input$man_from_table]])
    )
  })
  output$ui_man_to_table <- renderUI({
    req(length(all_tables_rv()) > 0)
    selectInput("man_to_table", "To Table:", choices = names(all_tables_rv()))
  })
  output$ui_man_to_col <- renderUI({
    tbls <- all_tables_rv()
    req(
      length(tbls) > 0,
      input$man_to_table,
      input$man_to_table %in% names(tbls)
    )
    selectInput(
      "man_to_col",
      "To Column (PK):",
      choices = names(tbls[[input$man_to_table]])
    )
  })

  observeEvent(input$btn_add_rel, {
    req(
      input$man_from_table,
      input$man_from_col,
      input$man_to_table,
      input$man_to_col
    )
    if (input$man_from_table == input$man_to_table) {
      showNotification("From and To tables must differ.", type = "warning")
      return()
    }
    manual_rels_rv(c(
      manual_rels_rv(),
      list(list(
        from_table = input$man_from_table,
        from_col = input$man_from_col,
        to_table = input$man_to_table,
        to_col = input$man_to_col,
        detected_by = "manual"
      ))
    ))
    showNotification("Relationship added.", type = "message")
  })

  observeEvent(input$btn_clear_manual, {
    manual_rels_rv(list())
    showNotification("Manual relationships cleared.", type = "message")
  })

  # ---- ERD ----
  output$erd_plot <- renderVisNetwork({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    tryCatch(
      {
        net <- build_network(tbls, all_rels_rv(), pk_map_rv(), composite_pk_map_rv())
        layout_mode <- input$erd_layout %||% "force"
        spring_len <- input$spring_length %||% 220

        vis <- visNetwork(net$nodes, net$edges, background = "#0f172a") %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
            nodesIdSelection = FALSE
          ) %>%
          visEdges(smooth = list(enabled = TRUE, type = "dynamic")) %>%
          visNodes(widthConstraint = list(minimum = 130, maximum = 230)) %>%
          visInteraction(
            navigationButtons = TRUE,
            tooltipDelay = 80,
            hover = TRUE
          ) %>%
          visEvents(
            click = "function(params) {
          if (params.nodes.length > 0) {
            Shiny.setInputValue('vis_clicked_node', {
              id: params.nodes[0],
              ts: Date.now()
            }, {priority: 'event'});
            showNodePanel(params.nodes[0]);
          }
        }"
          )

        if (layout_mode == "hierarchical") {
          vis <- vis %>%
            visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
            visPhysics(enabled = FALSE)
        } else if (layout_mode == "circular") {
          n_nodes <- nrow(net$nodes)
          if (n_nodes > 0) {
            radius <- max(200, n_nodes * 50)
            angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[seq_len(n_nodes)]
            net$nodes$x <- cos(angles) * radius
            net$nodes$y <- sin(angles) * radius
          }
          vis <- visNetwork(net$nodes, net$edges, background = "#0f172a") %>%
            visOptions(
              highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
              nodesIdSelection = FALSE
            ) %>%
            visEdges(smooth = list(enabled = TRUE, type = "dynamic")) %>%
            visNodes(widthConstraint = list(minimum = 130, maximum = 230)) %>%
            visInteraction(navigationButtons = TRUE, tooltipDelay = 80, hover = TRUE) %>%
            visPhysics(enabled = FALSE) %>%
            visEvents(
              click = "function(params) {
            if (params.nodes.length > 0) {
              Shiny.setInputValue('vis_clicked_node', {
                id: params.nodes[0],
                ts: Date.now()
              }, {priority: 'event'});
              showNodePanel(params.nodes[0]);
            }
          }"
            )
        } else {
          vis <- vis %>%
            visLayout(randomSeed = 42) %>%
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
        showNotification(
          paste0("ERD error: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        visNetwork(
          data.frame(
            id = 1,
            label = paste("Error:", conditionMessage(e)),
            color = "#7f1d1d",
            font.color = "white"
          ),
          data.frame(),
          background = "#0f172a"
        )
      }
    )
  })

  # ---- Table Details ----
  output$table_details_ui <- renderUI({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    cpks <- composite_pk_map_rv()
    rels <- all_rels_rv()

    tagList(
      # Export All button at the top
      div(
        style = "display:flex; justify-content:flex-end; margin-bottom:12px;",
        downloadButton(
          "dl_all_table_details",
          "\u2b07  Export All Tables (CSV)",
          class = "dl-btn"
        )
      ),
      lapply(names(tbls), function(t) {
        df <- tbls[[t]]
        pk_v <- pks[[t]]
        cpk_groups <- cpks[[t]]  # list of character vectors
        fk_rels <- Filter(function(r) r$from_table == t, rels)
        fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")
        dl_id <- paste0("dl_tbl_", make.names(t))

        pills <- tagList(
          span(
            class = "pill pill-rows",
            paste0(format(nrow(df), big.mark = ","), " rows")
          ),
          span(class = "pill pill-cols", paste0(ncol(df), " cols")),
          if (length(pk_v) > 0) {
            lapply(pk_v, function(p) {
              span(class = "pill pill-pk", paste0("PK: ", p))
            })
          } else if (length(cpk_groups) > 0) {
            lapply(cpk_groups, function(g) {
              span(class = "pill pill-pk", paste0("CPK: ", paste(g, collapse = " + ")))
            })
          } else {
            span(class = "pill pill-warn", "\u26a0 no PK")
          },
          if (length(fk_cols) > 0) {
            lapply(seq_along(fk_rels), function(i) {
              span(
                class = "pill pill-fk",
                paste0(
                  "FK: ",
                  fk_rels[[i]]$from_col,
                  " \u2192 ",
                  fk_rels[[i]]$to_table
                )
              )
            })
          }
        )

        div(
          class = "tbl-card",
          div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
            p(
              class = "tbl-card-title",
              style = "margin:0;",
              paste0("[ ", t, " ]")
            ),
            downloadButton(
              dl_id,
              "\u2b07 CSV",
              class = "dl-btn",
              style = "padding:4px 10px; font-size:11px;"
            )
          ),
          div(style = "margin-bottom: 12px;", pills),
          DTOutput(paste0("dt_col_", make.names(t)))
        )
      })
    )
  })

  # Dynamic DT outputs
  observe({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    cpks <- composite_pk_map_rv()
    rels <- all_rels_rv()

    lapply(names(tbls), function(t) {
      local({
        tname <- t
        output_id <- paste0("dt_col_", make.names(tname))
        output[[output_id]] <- renderDT(
          {
            df <- tbls[[tname]]
            pk_v <- pks[[tname]]
            cpk_groups <- cpks[[tname]]
            cpk_cols <- unique(unlist(cpk_groups))
            fk_rels <- Filter(function(r) r$from_table == tname, rels)
            fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")

            pk_marker <- vapply(names(df), function(col) {
              if (col %in% pk_v) "✓"
              else if (col %in% cpk_cols) "CPK"
              else ""
            }, character(1L))

            smry <- data.frame(
              Column = names(df),
              Type = vapply(
                df,
                function(x) paste(class(x), collapse = "/"),
                character(1)
              ),
              `Non-null` = vapply(df, function(x) sum(!is.na(x)), integer(1)),
              `Unique` = vapply(
                df,
                function(x) length(unique(na.omit(x))),
                integer(1)
              ),
              `PK` = pk_marker,
              `FK` = ifelse(names(df) %in% fk_cols, "✓", ""),
              check.names = FALSE,
              stringsAsFactors = FALSE
            )

            datatable(
              smry,
              options = list(
                pageLength = 10,
                dom = "tp",
                scrollX = TRUE,
                columnDefs = list(list(
                  className = "dt-center",
                  targets = c(2, 3, 4, 5)
                ))
              ),
              rownames = FALSE,
              selection = "none"
            ) %>%
              formatStyle(
                "PK",
                color = styleEqual(c("✓", "CPK"), c("#fbbf24", "#fbbf24")),
                backgroundColor = styleEqual(c("✓", "CPK"), c("#1a1000", "#1a1000"))
              ) %>%
              formatStyle(
                "FK",
                color = styleEqual("✓", "#c084fc"),
                backgroundColor = styleEqual("✓", "#140a2e")
              )
          },
          server = FALSE
        )
      })
    })
  })

  # Helper: build column-summary data.frame for one table
  make_tbl_summary <- function(t, tbls, pks, rels) {
    df <- tbls[[t]]
    pk_v <- pks[[t]]
    fk_rels <- Filter(function(r) r$from_table == t, rels)
    fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")
    nc <- ncol(df)
    data.frame(
      table = t,
      table_rows = rep(nrow(df), nc),
      table_cols = rep(nc, nc),
      column = names(df),
      type = vapply(
        df,
        function(x) paste(class(x), collapse = "/"),
        character(1)
      ),
      non_null = vapply(df, function(x) sum(!is.na(x)), integer(1)),
      unique_vals = vapply(
        df,
        function(x) length(unique(na.omit(x))),
        integer(1)
      ),
      is_pk = names(df) %in% pk_v,
      is_fk = names(df) %in% fk_cols,
      stringsAsFactors = FALSE
    )
  }

  # Per-table download handlers (created dynamically)
  observe({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    rels <- all_rels_rv()
    lapply(names(tbls), function(t) {
      local({
        tname <- t
        dl_id <- paste0("dl_tbl_", make.names(tname))
        output[[dl_id]] <- downloadHandler(
          filename = function() paste0(tname, "_details.csv"),
          content = function(file) {
            write.csv(
              make_tbl_summary(
                tname,
                all_tables_rv(),
                pk_map_rv(),
                all_rels_rv()
              ),
              file,
              row.names = FALSE
            )
          }
        )
      })
    })
  })

  # Export All: one CSV with all tables stacked, table column first
  output$dl_all_table_details <- downloadHandler(
    filename = "all_table_details.csv",
    content = function(file) {
      tbls <- all_tables_rv()
      pks <- pk_map_rv()
      rels <- all_rels_rv()
      combined <- do.call(
        rbind,
        lapply(
          names(tbls),
          make_tbl_summary,
          tbls = tbls,
          pks = pks,
          rels = rels
        )
      )
      write.csv(combined, file, row.names = FALSE)
    }
  )

  # ---- Relationships Tab ----
  output$relationships_ui <- renderUI({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    rels <- all_rels_rv()

    if (length(rels) == 0) {
      return(div(
        class = "empty-state",
        h4("No relationships detected"),
        p(
          style = "color:#334155; font-size:13px;",
          "Try uploading more tables or adjusting the detection method."
        )
      ))
    }

    methods <- list(
      list(key = "naming",          label = "Naming Convention",    cls = "m-naming"),
      list(key = "name_similarity", label = "Name Similarity",      cls = "m-name_similarity"),
      list(key = "value_overlap",   label = "Value Overlap",        cls = "m-value_overlap"),
      list(key = "cardinality",     label = "Cardinality Match",    cls = "m-cardinality"),
      list(key = "format",          label = "Format Fingerprint",   cls = "m-format"),
      list(key = "distribution",    label = "Distribution Similarity", cls = "m-distribution"),
      list(key = "null_pattern",    label = "Null Pattern",         cls = "m-null_pattern"),
      list(key = "content",         label = "Content Analysis",     cls = "m-content"),
      list(key = "schema",          label = "Schema Defined",       cls = "m-schema"),
      list(key = "manual",          label = "Manual",               cls = "m-manual")
    )

    tagList(
      lapply(methods, function(m) {
        m_rels <- Filter(function(r) r$detected_by == m$key, rels)
        if (length(m_rels) == 0) {
          return(NULL)
        }
        tagList(
          div(
            class = "rel-section-hdr",
            paste0(m$label, " (", length(m_rels), ")")
          ),
          lapply(m_rels, function(r) {
            to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) {
              r$to_col
            } else {
              "?"
            }

            # Confidence indicator
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

            # Signal chips
            signal_tags <- NULL
            if (!is.null(r$signals) && length(r$signals) > 0) {
              signal_tags <- div(
                class = "signal-chips",
                lapply(names(r$signals), function(s) {
                  span(class = "signal-chip", s)
                })
              )
            }

            rk <- paste(r$from_table, r$from_col, r$to_table, to_col, sep = "|")
            suppress_id <- paste0("suppress_", gsub("[^a-zA-Z0-9]", "_", rk))

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
                  "Shiny.setInputValue('suppress_rel', '%s', {priority: 'event'})",
                  rk
                ),
                "\u2715"
              )
            )
          })
        )
      }),
      # Global relationship management controls
      if (length(false_positives_rv()) > 0) {
        div(
          style = "margin-top:12px;padding-top:8px;border-top:1px solid var(--border);",
          span(style = "font-size:10px;color:var(--text-faint);",
               paste0(length(false_positives_rv()), " relationship(s) suppressed")),
          tags$button(
            class = "btn btn-xs",
            style = "font-size:10px;padding:2px 8px;margin-left:8px;background:#0c2a1a;color:#4ade80;border:1px solid #15803d;border-radius:4px;cursor:pointer;",
            onclick = "Shiny.setInputValue('restore_suppressed', Date.now(), {priority: 'event'})",
            "Restore all"
          )
        )
      }
    )
  })

  # ---- Relationship suppress/restore ----
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

  # ---- Sticky node detail panel ----
  observeEvent(input$vis_clicked_node, {
    req(input$vis_clicked_node$id)
    selected_node_rv(input$vis_clicked_node$id)
  })

  output$node_panel_ui <- renderUI({
    node_id <- selected_node_rv()
    req(node_id)
    tbls <- all_tables_rv()
    pks <- pk_map_rv()
    cpks <- composite_pk_map_rv()
    rels <- all_rels_rv()

    # node_id is the integer index; map back to table name
    tnames <- names(tbls)
    req(node_id <= length(tnames))
    t <- tnames[[node_id]]
    df <- tbls[[t]]
    pk_v <- pks[[t]]
    cpk_groups <- cpks[[t]]
    cpk_cols <- unique(unlist(cpk_groups))
    fk_r <- Filter(function(r) r$from_table == t, rels)
    fk_cols <- vapply(fk_r, `[[`, character(1), "from_col")

    # Column chips: PK/CPK = amber, FK = purple, plain = muted
    col_chips <- lapply(names(df), function(cn) {
      if (cn %in% pk_v || cn %in% cpk_cols) {
        tags$span(
          class = "col-chip",
          style = "background:#2a1a00;color:#fbbf24;border-color:#78350f;",
          cn
        )
      } else if (cn %in% fk_cols) {
        tags$span(
          class = "col-chip",
          style = "background:#1a0a2e;color:#c084fc;border-color:#4c1d95;",
          cn
        )
      } else {
        tags$span(
          class = "col-chip",
          style = "background:rgba(255,255,255,0.04);color:#94a3b8;border-color:#1e3a5f;",
          cn
        )
      }
    })

    tagList(
      # Stats
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Overview"),
        tags$div(
          style = "display:flex;gap:12px;flex-wrap:wrap;",
          tags$span(
            class = "pill pill-rows",
            paste0(format(nrow(df), big.mark = ","), " rows")
          ),
          tags$span(class = "pill pill-cols", paste0(ncol(df), " cols"))
        )
      ),
      # PKs
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Primary Key(s)"),
        if (length(pk_v) > 0) {
          tags$div(lapply(pk_v, function(p) {
            tags$span(class = "pill pill-pk", p)
          }))
        } else if (length(cpk_groups) > 0) {
          tags$div(lapply(cpk_groups, function(g) {
            tags$span(class = "pill pill-pk",
                      paste0("CPK: ", paste(g, collapse = " + ")))
          }))
        } else {
          tags$span(style = "color:#f87171;font-size:12px;", "none detected")
        }
      ),
      # FKs
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Foreign Key(s)"),
        if (length(fk_r) > 0) {
          tags$div(lapply(fk_r, function(r) {
            to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) {
              r$to_col
            } else {
              "?"
            }
            tags$div(
              style = "font-size:11px;color:#c084fc;padding:1px 0;",
              paste0(r$from_col, " → ", r$to_table, ".", to_col)
            )
          }))
        } else {
          tags$span(style = "color:#64748b;font-size:12px;", "none")
        }
      ),
      # All columns
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", paste0("Columns (", ncol(df), ")")),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:3px;", col_chips)
      )
    )
  })

  # ---- Name Changes tab ----
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
          p(
            style = "font-size:13px;color:var(--text-faint);",
            "janitor::clean_names() found nothing to rename in the uploaded files."
          )
        )
      ))
    }

    n_tables <- length(unique(log$source))
    n_tbl_renames <- sum(log$object_type == "table")
    n_col_renames <- sum(log$object_type == "column")
    tagList(
      div(
        class = "rename-summary",
        tags$b(nrow(log)),
        " name(s) renamed across ",
        tags$b(n_tables),
        " table(s)",
        if (n_tbl_renames > 0) paste0(" (", n_tbl_renames, " table, ", n_col_renames, " column)"),
        ". ",
        "Amber = PK-related, purple = FK-related columns."
      ),
      DTOutput("dt_rename_log"),
      br(),
      downloadButton("dl_rename_log", "\u2b07  Export CSV", class = "dl-btn")
    )
  })

  output$dt_rename_log <- renderDT(
    {
      log <- rename_log_rv()
      req(nrow(log) > 0)
      datatable(
        log,
        colnames = c("Type", "Table", "Original Name", "Cleaned Name"),
        options = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
        rownames = FALSE,
        selection = "none"
      )
    },
    server = FALSE
  )

  output$dl_rename_log <- downloadHandler(
    filename = "name_changes.csv",
    content = function(file) write.csv(rename_log_rv(), file, row.names = FALSE)
  )

  # ---- Download ----
  output$dl_rels <- downloadHandler(
    filename = "table_relationships.csv",
    content = function(file) {
      rels <- all_rels_rv()
      if (length(rels) == 0) {
        write.csv(
          data.frame(
            from_table = "", from_col = "", to_table = "", to_col = "",
            detected_by = "", confidence = "", score = numeric(0),
            signals = "", reasons = ""
          )[0, ],
          file,
          row.names = FALSE
        )
      } else {
        df <- do.call(
          rbind,
          lapply(rels, function(r) {
            data.frame(
              from_table = r$from_table,
              from_col = r$from_col,
              to_table = r$to_table,
              to_col = if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "",
              detected_by = r$detected_by,
              confidence = if (!is.null(r$confidence)) r$confidence else "",
              score = if (!is.null(r$score)) r$score else NA_real_,
              signals = if (!is.null(r$signals)) paste(names(r$signals), collapse = "; ") else "",
              reasons = if (!is.null(r$reasons)) paste(r$reasons, collapse = "; ") else "",
              stringsAsFactors = FALSE
            )
          })
        )
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  # ---- Export: Relationships CSV (from Export tab) ----
  output$dl_rels_csv <- downloadHandler(
    filename = "table_relationships.csv",
    content = function(file) {
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
            from_table = r$from_table, from_col = r$from_col,
            to_table = r$to_table,
            to_col = if (!is.na(r$to_col) && !is.null(r$to_col)) r$to_col else "",
            detected_by = r$detected_by,
            confidence = if (!is.null(r$confidence)) r$confidence else "",
            score = if (!is.null(r$score)) r$score else NA_real_,
            signals = if (!is.null(r$signals)) paste(names(r$signals), collapse = "; ") else "",
            reasons = if (!is.null(r$reasons)) paste(r$reasons, collapse = "; ") else "",
            stringsAsFactors = FALSE
          )
        }))
        write.csv(df, file, row.names = FALSE)
      }
    }
  )

  # ---- Export: dbt schema.yml ----
  output$dl_dbt_yaml <- downloadHandler(
    filename = "schema.yml",
    content = function(file) {
      yaml_str <- generate_dbt_yaml(all_tables_rv(), all_rels_rv(), pk_map_rv(), composite_pk_map_rv())
      writeLines(yaml_str, file)
    }
  )

  # ---- Export: Mermaid ERD ----
  output$dl_mermaid <- downloadHandler(
    filename = "erd.mmd",
    content = function(file) {
      mmd_str <- generate_mermaid_erd(all_tables_rv(), all_rels_rv(), pk_map_rv(), composite_pk_map_rv())
      writeLines(mmd_str, file)
    }
  )

  # ---- Export: Session save ----
  output$dl_session <- downloadHandler(
    filename = function() {
      paste0("table_explorer_session_", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      json_str <- save_session_json(
        tables = all_tables_rv(),
        rels = all_rels_rv(),
        manual_rels = manual_rels_rv(),
        schema_rels = schema_rels_rv(),
        settings = list(
          detect_method = input$detect_method,
          min_confidence = input$min_confidence
        )
      )
      writeLines(json_str, file)
    }
  )

  # ---- Session restore ----
  observeEvent(input$restore_session_file, {
    req(input$restore_session_file)
    json_text <- readLines(input$restore_session_file$datapath, warn = FALSE)
    json_text <- paste(json_text, collapse = "\n")

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
      # Rebuild metadata
      meta <- lapply(result$tables, function(df) {
        list(size = object.size(df), nrow = nrow(df), ncol = ncol(df))
      })
      table_meta_rv(meta)
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
}

shinyApp(ui, server)
