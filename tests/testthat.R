# Run all testthat tests
library(testthat)

# Source the modules under test (not a package, so source directly)
app_dir <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = TRUE)

source(file.path(app_dir, "file_readers.R"))
source(file.path(app_dir, "inference.R"))
source(file.path(app_dir, "export_utils.R"))
source(file.path(app_dir, "db_connectors.R"))

test_dir(file.path(app_dir, "tests", "testthat"))
