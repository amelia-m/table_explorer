#' Launch the Table Relationship Explorer
#'
#' @param ... Arguments passed to \code{golem::with_golem_options()}.
#' @export
run_app <- function(...) {
  # Register static assets (CSS, JS) under the package resource path
  addResourcePath(
    "tableexplorer",
    system.file("app/www", package = "tableexplorer")
  )
  golem::with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
