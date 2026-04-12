# ── Test setup ──────────────────────────────────────────────
# When run via devtools::test() / testthat::test_package(), all R/ files are
# loaded automatically by pkgload before setup.R runs, so no explicit
# source() calls are needed.
#
# For direct testthat::test_dir() usage (legacy), fall back to sourcing R/.
if (!requireNamespace("tableexplorer", quietly = TRUE)) {
  # Best-effort: locate the package root and source R/ files
  candidates <- c(
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."),       mustWork = FALSE),
    getwd()
  )
  pkg_root <- NULL
  for (d in candidates) {
    if (file.exists(file.path(d, "R", "utils_inference.R"))) {
      pkg_root <- d
      break
    }
  }
  if (!is.null(pkg_root)) {
    r_files <- c(
      "utils_helpers.R", "utils_inference.R", "utils_file_readers.R",
      "utils_db_connectors.R", "utils_export.R"
    )
    for (f in r_files) source(file.path(pkg_root, "R", f))
  }
}

# ── Helper: paths to sample data (works in package and source modes) ──
sample_data_dir <- tryCatch(
  system.file("extdata", "sample_data", package = "tableexplorer", mustWork = TRUE),
  error = function(e) {
    # Fallback: look for original sample_data/ directory
    candidates <- c(
      normalizePath(file.path(getwd(), "..", "..", "sample_data"), mustWork = FALSE),
      normalizePath(file.path(getwd(), "..", "sample_data"),       mustWork = FALSE)
    )
    found <- Filter(dir.exists, candidates)
    if (length(found)) found[[1]] else ""
  }
)

schema_json_path <- tryCatch(
  system.file("extdata", "schema.json", package = "tableexplorer", mustWork = TRUE),
  error = function(e) {
    candidates <- c(
      normalizePath(file.path(getwd(), "..", "..", "schema.json"), mustWork = FALSE),
      normalizePath(file.path(getwd(), "..", "schema.json"),       mustWork = FALSE)
    )
    found <- Filter(file.exists, candidates)
    if (length(found)) found[[1]] else ""
  }
)
