# ============================================================
# dev/01_start.R — One-time project setup
# Run these lines interactively in a fresh R session.
# ============================================================

# 1. Fill in DESCRIPTION metadata
# golem::fill_desc(
#   pkg_name       = "tableexplorer",
#   pkg_title      = "Table Relationship Explorer",
#   pkg_description = "Interactive Shiny app for exploring table relationships.",
#   author_first   = "Your",
#   author_last    = "Name",
#   author_email   = "you@example.com",
#   repo_url       = NULL
# )

# 2. Set a licence
usethis::use_mit_license()

# 3. Auto-detect and add all Imports/Suggests to DESCRIPTION
# Requires: install.packages("attachment")
# attachment::att_amend_desc()

# 4. Initialize git (if not already)
# usethis::use_git()
