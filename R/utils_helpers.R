# ============================================================
# utils_helpers.R — Shared helper utilities
# ============================================================

#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !(is.character(a) && length(a) == 1L && !nzchar(a))) a else b
}
