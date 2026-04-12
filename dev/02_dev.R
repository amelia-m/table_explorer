# ============================================================
# dev/02_dev.R — Ongoing development helpers
# Run interactively during development.
# ============================================================

# ── Load the package for interactive use ─────────────────────
pkgload::load_all(export_all = FALSE)

# ── Add a new module ─────────────────────────────────────────
# golem::add_module(name = "my_module")

# ── Add a package dependency ─────────────────────────────────
# usethis::use_package("pkg_name")
# usethis::use_package("pkg_name", type = "Suggests")

# ── Generate documentation (NAMESPACE + .Rd files) ───────────
devtools::document()

# ── Run tests ────────────────────────────────────────────────
devtools::test()

# ── Check the package ─────────────────────────────────────────
# devtools::check()

# ── Launch the app locally ───────────────────────────────────
# tableexplorer::run_app()
