# ============================================================
# dev/03_deploy.R — Deployment helpers
# Run interactively when deploying.
# ============================================================

# ── Deploy to Posit Connect / shinyapps.io ───────────────────
# rsconnect::deployApp(appName = "tableexplorer")

# ── Generate a Dockerfile ────────────────────────────────────
# golem::add_dockerfile()
# golem::add_dockerfile_with_renv()

# ── Build source package for CRAN/distribution ───────────────
# devtools::build()
