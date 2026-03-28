# ── Test setup: source all modules ─────────────────────────────
app_dir <- normalizePath(file.path(dirname(getwd()), ".."), mustWork = FALSE)
# When run via testthat::test_dir, getwd() is tests/testthat
# Try multiple strategies to find the project root
candidates <- c(
  normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
  normalizePath(file.path(getwd(), ".."), mustWork = FALSE),
  getwd()
)
app_dir <- NULL
for (d in candidates) {
  if (file.exists(file.path(d, "inference.R"))) {
    app_dir <- d
    break
  }
}
if (is.null(app_dir)) stop("Could not locate project root with inference.R")

source(file.path(app_dir, "file_readers.R"))
source(file.path(app_dir, "inference.R"))
source(file.path(app_dir, "export_utils.R"))
source(file.path(app_dir, "db_connectors.R"))

# ── Helper: path to sample data ──────────────────────────────
sample_data_dir <- file.path(app_dir, "sample_data")
schema_json_path <- file.path(app_dir, "schema.json")
