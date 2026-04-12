# ============================================================
# utils_vis.R — ERD Network Builder
# ============================================================

#' @noRd
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
              function(r) paste0(r$from_col, " \u2192 ", r$to_table),
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
        col_names <- names(df)
        show_cols <- if (length(col_names) > 12) col_names[1:12] else col_names
        overflow <- if (length(col_names) > 12) {
          paste0("<span style='color:#64748b;font-size:9px;'> +", length(col_names) - 12, " more</span>")
        } else {
          ""
        }
        fkr_from_cols <- vapply(fkr, `[[`, character(1), "from_col")
        chips <- paste(
          vapply(
            show_cols,
            function(cn) {
              chip_bg <- if (cn %in% pks || cn %in% cpk_cols) {
                "background:#2a1a00;color:#fbbf24;border-color:#78350f;"
              } else if (cn %in% fkr_from_cols) {
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
        )
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
          fk_s,
          "</span>",
          "</div>",
          "<div style='border-top:1px solid #1e3a5f;padding-top:8px;'>",
          "<span style='color:#64748b;font-size:10px;letter-spacing:1px;'>COLUMNS</span>",
          "<div style='display:flex;flex-wrap:wrap;gap:3px;margin-top:6px;'>",
          chips,
          overflow,
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

        conf <- if (!is.null(r$confidence)) r$confidence else "medium"
        score_val <- if (!is.null(r$score)) r$score else NA
        conf_opacity <- switch(conf, high = 0.95, medium = 0.60, low = 0.28, 0.60)
        conf_width <- switch(conf, high = 2.5, medium = 1.5, low = 0.8, 1.5)

        conf_html <- ""
        if (!is.na(score_val)) {
          conf_color <- switch(conf, high = "#4ade80", medium = "#facc15", low = "#f87171", "#94a3b8")
          conf_html <- paste0(
            "<br><span style='color:", conf_color, ";font-size:10px;'>",
            "&#9679; ", conf, " (", round(score_val * 100), "%)</span>"
          )
        }

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
            "<span style='color:#94a3b8;'>", r$from_table, ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>", r$from_col, "</span>",
            "<span style='color:#475569;'> &#8594; </span>",
            "<span style='color:#94a3b8;'>", r$to_table, ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>", to_col, "</span><br>",
            "<span style='color:", ecol, ";font-size:10px;letter-spacing:0.5px;'>&#9632; ",
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
