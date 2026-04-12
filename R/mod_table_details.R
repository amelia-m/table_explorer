# ============================================================
# mod_table_details.R — Table Details Tab
# ============================================================

#' Table details module UI
#' @noRd
mod_table_details_ui <- function(id) {
  ns <- NS(id)
  tagList(br(), uiOutput(ns("table_details_ui")))
}

#' Table details module server
#'
#' @param id Module id
#' @param all_tables_rv reactiveVal holding tables
#' @param pk_map_rv reactive returning PK map
#' @param composite_pk_map_rv reactive returning composite PK map
#' @param all_rels_rv reactive returning all relationships
#' @noRd
mod_table_details_server <- function(id, all_tables_rv, pk_map_rv,
                                      composite_pk_map_rv, all_rels_rv) {
  moduleServer(id, function(input, output, session) {

    .make_tbl_summary <- function(t, tbls, pks, rels) {
      df <- tbls[[t]]
      pk_v <- pks[[t]]
      fk_rels <- Filter(function(r) r$from_table == t, rels)
      fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")
      nc <- ncol(df)
      data.frame(
        table       = t,
        table_rows  = rep(nrow(df), nc),
        table_cols  = rep(nc, nc),
        column      = names(df),
        type        = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
        non_null    = vapply(df, function(x) sum(!is.na(x)), integer(1)),
        unique_vals = vapply(df, function(x) length(unique(na.omit(x))), integer(1)),
        is_pk       = names(df) %in% pk_v,
        is_fk       = names(df) %in% fk_cols,
        stringsAsFactors = FALSE
      )
    }

    output$table_details_ui <- renderUI({
      tbls <- all_tables_rv()
      req(length(tbls) > 0)
      pks  <- pk_map_rv()
      cpks <- composite_pk_map_rv()
      rels <- all_rels_rv()

      tagList(
        div(
          style = "display:flex; justify-content:flex-end; margin-bottom:12px;",
          downloadButton(
            session$ns("dl_all_table_details"),
            "\u2b07  Export All Tables (CSV)",
            class = "dl-btn"
          )
        ),
        lapply(names(tbls), function(t) {
          df <- tbls[[t]]
          pk_v      <- pks[[t]]
          cpk_groups <- cpks[[t]]
          fk_rels   <- Filter(function(r) r$from_table == t, rels)
          fk_cols   <- vapply(fk_rels, `[[`, character(1), "from_col")
          dl_id     <- session$ns(paste0("dl_tbl_", make.names(t)))

          pills <- tagList(
            span(class = "pill pill-rows",
                 paste0(format(nrow(df), big.mark = ","), " rows")),
            span(class = "pill pill-cols", paste0(ncol(df), " cols")),
            if (length(pk_v) > 0) {
              lapply(pk_v, function(p) span(class = "pill pill-pk", paste0("PK: ", p)))
            } else if (length(cpk_groups) > 0) {
              lapply(cpk_groups, function(g) {
                span(class = "pill pill-pk", paste0("CPK: ", paste(g, collapse = " + ")))
              })
            } else {
              span(class = "pill pill-warn", "\u26a0 no PK")
            },
            if (length(fk_cols) > 0) {
              lapply(seq_along(fk_rels), function(i) {
                span(class = "pill pill-fk",
                     paste0("FK: ", fk_rels[[i]]$from_col, " \u2192 ", fk_rels[[i]]$to_table))
              })
            }
          )

          div(
            class = "tbl-card",
            div(
              style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
              p(class = "tbl-card-title", style = "margin:0;", paste0("[ ", t, " ]")),
              downloadButton(dl_id, "\u2b07 CSV", class = "dl-btn",
                             style = "padding:4px 10px; font-size:11px;")
            ),
            div(style = "margin-bottom: 12px;", pills),
            DTOutput(session$ns(paste0("dt_col_", make.names(t))))
          )
        })
      )
    })

    # Dynamic per-table DT outputs
    observe({
      tbls <- all_tables_rv()
      req(length(tbls) > 0)
      pks  <- pk_map_rv()
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
                if (col %in% pk_v) "\u2713"
                else if (col %in% cpk_cols) "CPK"
                else ""
              }, character(1L))

              smry <- data.frame(
                Column    = names(df),
                Type      = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
                `Non-null` = vapply(df, function(x) sum(!is.na(x)), integer(1)),
                `Unique`   = vapply(df, function(x) length(unique(na.omit(x))), integer(1)),
                `PK`      = pk_marker,
                `FK`      = ifelse(names(df) %in% fk_cols, "\u2713", ""),
                check.names = FALSE,
                stringsAsFactors = FALSE
              )

              datatable(
                smry,
                options = list(
                  pageLength = 10, dom = "tp", scrollX = TRUE,
                  columnDefs = list(list(className = "dt-center", targets = c(2, 3, 4, 5)))
                ),
                rownames = FALSE, selection = "none"
              ) |>
                formatStyle("PK",
                             color = styleEqual(c("\u2713", "CPK"), c("#fbbf24", "#fbbf24")),
                             backgroundColor = styleEqual(c("\u2713", "CPK"), c("#1a1000", "#1a1000"))) |>
                formatStyle("FK",
                             color = styleEqual("\u2713", "#c084fc"),
                             backgroundColor = styleEqual("\u2713", "#140a2e"))
            },
            server = FALSE
          )
        })
      })
    })

    # Dynamic per-table download handlers
    observe({
      tbls <- all_tables_rv()
      req(length(tbls) > 0)
      lapply(names(tbls), function(t) {
        local({
          tname <- t
          dl_id <- paste0("dl_tbl_", make.names(tname))
          output[[dl_id]] <- downloadHandler(
            filename = function() paste0(tname, "_details.csv"),
            content = function(file) {
              write.csv(
                .make_tbl_summary(tname, all_tables_rv(), pk_map_rv(), all_rels_rv()),
                file, row.names = FALSE
              )
            }
          )
        })
      })
    })

    output$dl_all_table_details <- downloadHandler(
      filename = "all_table_details.csv",
      content = function(file) {
        tbls <- all_tables_rv()
        pks  <- pk_map_rv()
        rels <- all_rels_rv()
        combined <- do.call(
          rbind,
          lapply(names(tbls), .make_tbl_summary, tbls = tbls, pks = pks, rels = rels)
        )
        write.csv(combined, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
