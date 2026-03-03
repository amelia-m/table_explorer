# ============================================================
# Table Relationship Explorer — Shiny App
# Deploy to Posit Connect (Free Plan)
# ============================================================

library(shiny)
library(visNetwork)
library(DT)
library(shinythemes)
library(janitor)

# ============================================================
# Helper Functions
# ============================================================

# Normalise one or more names the same way janitor::clean_names() does:
# lowercase, non-alphanumeric -> "_", collapse/trim underscores.
clean_name <- function(name) janitor::make_clean_names(name)

# ---- Access database reader ----------------------------------------
# Strategy 1 (all platforms): RJDBC + UCanAccess pure-Java JDBC driver.
#   Requires: install.packages("RJDBC") and Java (usually pre-installed).
#   JARs are downloaded once to a local cache folder on first use.
# Strategy 2 (Windows fallback): RODBC.
#   Requires: install.packages("RODBC") + Microsoft Access Database Engine.
# Returns a named list of data.frames, one per user table.
# Errors are passed to notify_fn(msg) rather than thrown.

# Default JAR cache location (alongside app.R, or in user cache dir)
access_jar_dir <- function() {
  app_jars <- file.path(dirname(sys.frame(1)$ofile %||% "."), "access_jars")
  if (dir.exists(app_jars) || dir.create(app_jars, showWarnings = FALSE)) {
    return(app_jars)
  }
  tools::R_user_dir("table_explorer_access_jars", "cache")
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(a)) a else b

# UCanAccess JARs from Maven Central (stable long-lived URLs)
ucanaccess_jars <- list(
  list(
    file = "ucanaccess-5.0.1.jar",
    url = "https://repo1.maven.org/maven2/net/sf/ucanaccess/ucanaccess/5.0.1/ucanaccess-5.0.1.jar"
  ),
  list(
    file = "jackcess-4.0.1.jar",
    url = "https://repo1.maven.org/maven2/com/healthmarketscience/jackcess/jackcess/4.0.1/jackcess-4.0.1.jar"
  ),
  list(
    file = "commons-lang3-3.12.0.jar",
    url = "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
  ),
  list(
    file = "commons-logging-1.2.jar",
    url = "https://repo1.maven.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.jar"
  ),
  list(
    file = "hsqldb-2.7.1.jar",
    url = "https://repo1.maven.org/maven2/org/hsqldb/hsqldb/2.7.1/hsqldb-2.7.1.jar"
  )
)

ensure_ucanaccess_jars <- function(jar_dir, notify_fn) {
  if (!dir.exists(jar_dir)) {
    dir.create(jar_dir, recursive = TRUE, showWarnings = FALSE)
  }
  for (j in ucanaccess_jars) {
    dest <- file.path(jar_dir, j$file)
    if (!file.exists(dest)) {
      notify_fn(paste0("Downloading ", j$file, "..."))
      tryCatch(
        utils::download.file(j$url, dest, mode = "wb", quiet = TRUE),
        error = function(e) {
          notify_fn(paste0(
            "Failed to download ",
            j$file,
            ": ",
            conditionMessage(e)
          ))
        }
      )
    }
  }
  jar_paths <- file.path(
    jar_dir,
    vapply(ucanaccess_jars, `[[`, character(1), "file")
  )
  all(file.exists(jar_paths))
}

read_access_db <- function(path, notify_fn = message) {
  # ── Strategy 1: RJDBC + UCanAccess (all platforms, needs Java) ──
  if (requireNamespace("RJDBC", quietly = TRUE)) {
    jar_dir <- tryCatch(
      file.path(dirname(normalizePath(path, mustWork = FALSE)), "access_jars"),
      error = function(e) tools::R_user_dir("table_explorer", "cache")
    )
    jars_ok <- tryCatch(
      ensure_ucanaccess_jars(jar_dir, notify_fn),
      error = function(e) FALSE
    )

    if (jars_ok) {
      jar_paths <- file.path(
        jar_dir,
        vapply(ucanaccess_jars, `[[`, character(1), "file")
      )
      result <- tryCatch(
        {
          drv <- RJDBC::JDBC(
            driverClass = "net.ucanaccess.jdbc.UcanaccessDriver",
            classPath = jar_paths
          )
          jdbc_url <- paste0(
            "jdbc:ucanaccess://",
            normalizePath(path, winslash = "/")
          )
          con <- DBI::dbConnect(drv, jdbc_url)
          on.exit(
            tryCatch(DBI::dbDisconnect(con), error = function(e) NULL),
            add = TRUE
          )

          # List user tables only (skip system tables starting with "MSys")
          all_tables <- DBI::dbListTables(con)
          user_tables <- all_tables[!grepl("^MSys|^USys|^~", all_tables)]

          tbls <- setNames(
            lapply(user_tables, function(tname) {
              tryCatch(
                DBI::dbReadTable(con, tname),
                error = function(e) {
                  notify_fn(paste0(
                    "Could not read table '",
                    tname,
                    "': ",
                    conditionMessage(e)
                  ))
                  NULL
                }
              )
            }),
            user_tables
          )
          Filter(Negate(is.null), tbls)
        },
        error = function(e) {
          notify_fn(paste0(
            "RJDBC/UCanAccess error: ",
            conditionMessage(e),
            " — is Java installed? Run `Sys.getenv('JAVA_HOME')` to check."
          ))
          NULL
        }
      )
      if (!is.null(result) && length(result) > 0) return(result)
    }
  }

  # ── Strategy 2: RODBC (Windows — needs Access Database Engine) ───
  if (
    requireNamespace("RODBC", quietly = TRUE) && .Platform$OS.type == "windows"
  ) {
    result <- tryCatch(
      {
        con <- RODBC::odbcConnectAccess2007(path)
        if (inherits(con, "RODBC")) {
          on.exit(RODBC::odbcClose(con), add = TRUE)
          tnames <- RODBC::sqlTables(con, tableType = "TABLE")$TABLE_NAME
          tbls <- setNames(
            lapply(tnames, function(tname) {
              tryCatch(
                RODBC::sqlFetch(con, tname, stringsAsFactors = FALSE),
                error = function(e) {
                  notify_fn(paste0(
                    "RODBC: could not fetch '",
                    tname,
                    "': ",
                    conditionMessage(e)
                  ))
                  NULL
                }
              )
            }),
            tnames
          )
          Filter(Negate(is.null), tbls)
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
    if (!is.null(result) && length(result) > 0) return(result)
  }

  # ── No strategy succeeded ────────────────────────────────────────
  notify_fn(paste0(
    "Could not read Access file. ",
    "Install the RJDBC package (install.packages('RJDBC')) and ensure Java is available. ",
    "The required UCanAccess JARs will be downloaded automatically on first use. ",
    "On Windows, RODBC + the Microsoft Access Database Engine are also accepted."
  ))
  list()
}

# Extract the stem from an _id column: "document_id" -> "document"
id_stem <- function(col_clean) sub("_id$", "", col_clean)

# Does a cleaned column name look like a PK for this table?
# Handles: exact "id", exact "{table}_id", and stem-prefix match
# e.g. "document_id" matches table "documents" because "documents" starts with "document"
is_pk_name <- function(col_clean, tname_clean) {
  col_clean == "id" ||
    col_clean == paste0(tname_clean, "_id") ||
    (grepl("_id$", col_clean) && startsWith(tname_clean, id_stem(col_clean)))
}

# Does a cleaned column name look like a FK pointing at table t2?
# Same stem-prefix logic in reverse.
is_fk_for <- function(col_clean, t2clean) {
  col_clean == paste0(t2clean, "_id") ||
    (grepl("_id$", col_clean) && startsWith(t2clean, id_stem(col_clean)))
}

detect_pks <- function(df, table_name, method = "both") {
  cols <- names(df)
  candidates <- character(0)
  n <- nrow(df)

  if (method %in% c("naming", "both")) {
    tname <- clean_name(table_name)
    cols_clean <- clean_name(cols)
    hits <- cols[vapply(
      cols_clean,
      is_pk_name,
      logical(1),
      tname_clean = tname
    )]
    candidates <- union(candidates, hits)
  }

  if (method %in% c("uniqueness", "both") && n > 0) {
    hits <- cols[vapply(
      cols,
      function(c) {
        v <- df[[c]]
        !anyNA(v) && length(unique(v)) == n
      },
      logical(1)
    )]
    candidates <- union(candidates, hits)
  }

  candidates
}

detect_fks <- function(tables, method = "both") {
  rels <- list()
  seen <- character(0) # fast set: "t1|col|t2" keys already added
  tnames <- names(tables)
  if (method == "manual" || length(tnames) < 2) {
    return(rels)
  }

  # Pre-compute PK columns (all-unique, no-NA) per table for uniqueness matching
  pk_map <- lapply(tnames, function(t) {
    df <- tables[[t]]
    n <- nrow(df)
    if (n == 0) {
      return(character(0))
    }
    names(df)[vapply(
      names(df),
      function(c) !anyNA(df[[c]]) && length(unique(df[[c]])) == n,
      logical(1)
    )]
  })
  names(pk_map) <- tnames

  for (t1 in tnames) {
    df1 <- tables[[t1]]
    n1 <- nrow(df1)

    for (col in names(df1)) {
      if (col %in% pk_map[[t1]]) {
        next
      } # skip PKs of the same table

      for (t2 in tnames) {
        if (t2 == t1) {
          next
        }

        # Skip if already have a rel for this (t1, col, t2) pair
        rel_key <- paste(t1, col, t2, sep = "|")
        if (rel_key %in% seen) {
          next
        }

        df2 <- tables[[t2]]
        t2clean <- clean_name(t2)

        # --- Naming convention ---
        if (method %in% c("naming", "both")) {
          col_clean <- clean_name(col)
          if (is_fk_for(col_clean, t2clean)) {
            pk_col <- if (length(pk_map[[t2]]) > 0) {
              pk_map[[t2]][1]
            } else {
              NA_character_
            }
            rels[[length(rels) + 1]] <- list(
              from_table = t1,
              from_col = col,
              to_table = t2,
              to_col = pk_col,
              detected_by = "naming"
            )
            seen <- c(seen, rel_key)
            next
          }
        }

        # --- Uniqueness / value-subset ---
        # Guard: only consider columns whose name ends in _id/_key.
        # Also skip if the source table is very large (>100k rows) or very wide (>200 cols)
        # to avoid O(n) scans that freeze the app.
        col_clean_u <- clean_name(col)
        looks_like_key <- grepl("(_id|_key|id$|key$)", col_clean_u)
        too_large <- n1 > 100000 || ncol(df1) > 200
        if (
          method %in%
            c("uniqueness", "both") &&
            looks_like_key &&
            !too_large &&
            n1 > 0 &&
            length(pk_map[[t2]]) > 0
        ) {
          vals1 <- na.omit(df1[[col]])
          if (length(vals1) == 0) {
            next
          }
          for (pk_col in pk_map[[t2]]) {
            if (all(vals1 %in% df2[[pk_col]])) {
              rels[[length(rels) + 1]] <- list(
                from_table = t1,
                from_col = col,
                to_table = t2,
                to_col = pk_col,
                detected_by = "uniqueness"
              )
              seen <- c(seen, rel_key)
              break
            }
          }
        }
      }
    }
  }
  rels
}


build_network <- function(tables, rels, pk_map) {
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
        fkr <- Filter(function(r) r$from_table == t, rels)
        fk_s <- if (length(fkr) > 0) {
          paste(
            vapply(
              fkr,
              function(r) paste0(r$from_col, " → ", r$to_table),
              character(1)
            ),
            collapse = "<br>"
          )
        } else {
          "none"
        }
        pk_s <- if (length(pks) > 0) {
          paste(pks, collapse = ", ")
        } else {
          "none detected"
        }
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
          gsub("<br>", "<br>", fk_s),
          "</span>",
          "</div>",
          "<div style='border-top:1px solid #1e3a5f;padding-top:8px;'>",
          "<span style='color:#64748b;font-size:10px;letter-spacing:1px;'>COLUMNS</span>",
          "<div style='display:flex;flex-wrap:wrap;gap:3px;margin-top:6px;'>",
          paste(
            vapply(
              names(df),
              function(cn) {
                chip_bg <- if (cn %in% pks) {
                  "background:#2a1a00;color:#fbbf24;border-color:#78350f;"
                } else if (
                  cn %in% vapply(fkr, `[[`, character(1), "from_col")
                ) {
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
          ),
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
    naming = "#4ADE80",
    uniqueness = "#FB923C",
    manual = "#F87171"
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
            "<span style='color:#94a3b8;'>",
            r$from_table,
            ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>",
            r$from_col,
            "</span>",
            "<span style='color:#475569;'> &#8594; </span>",
            "<span style='color:#94a3b8;'>",
            r$to_table,
            ".</span>",
            "<span style='color:#fbbf24;font-weight:700;'>",
            to_col,
            "</span><br>",
            "<span style='color:",
            ecol,
            ";font-size:10px;letter-spacing:0.5px;'>&#9632; ",
            toupper(r$detected_by),
            "</span>",
            "</div>"
          ),
          arrows = "to",
          color.color = ecol,
          color.highlight = ecol,
          color.opacity = 0.85,
          dashes = (r$detected_by == "uniqueness"),
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

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$style(HTML(
      '
    @import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600;700&display=swap");

    /* ── CSS variables: dark (default) ───────────────────────── */
    :root {
      --bg-base:       #0f172a;
      --bg-surface:    #1e293b;
      --bg-inset:      #0f172a;
      --border:        #1e3a5f;
      --border-sub:    #334155;
      --text-primary:  #e2e8f0;
      --text-secondary:#94a3b8;
      --text-muted:    #64748b;
      --text-faint:    #475569;
      --accent:        #60a5fa;
      --accent-soft:   #93c5fd;
      --accent-bg:     #0c1a2e;
      --nav-btn-bg:    #1e3a5f;
      --nav-btn-icon:  #60a5fa;
      --nav-btn-hover: #2a5298;
      --nav-btn-border:#334155;
      --empty-text:    #475569;
      --tbl-card-bg:   #0f172a;
      --tbl-row-hover: #1e293b;
      --rel-border:    #1e293b;
      --code-bg:       #0f172a;
    }
    /* ── CSS variables: light ─────────────────────────────────── */
    body.light-mode {
      --bg-base:       #f1f5f9;
      --bg-surface:    #ffffff;
      --bg-inset:      #f8fafc;
      --border:        #cbd5e1;
      --border-sub:    #e2e8f0;
      --text-primary:  #0f172a;
      --text-secondary:#475569;
      --text-muted:    #64748b;
      --text-faint:    #94a3b8;
      --accent:        #2563eb;
      --accent-soft:   #1d4ed8;
      --accent-bg:     #eff6ff;
      --nav-btn-bg:    #e2e8f0;
      --nav-btn-icon:  #2563eb;
      --nav-btn-hover: #bfdbfe;
      --nav-btn-border:#cbd5e1;
      --empty-text:    #94a3b8;
      --tbl-card-bg:   #f8fafc;
      --tbl-row-hover: #f1f5f9;
      --rel-border:    #e2e8f0;
      --code-bg:       #e2e8f0;
    }

    /* ── Base ─────────────────────────────────────────────────── */
    body {
      background: var(--bg-base);
      font-family: "IBM Plex Sans", sans-serif;
      color: var(--text-primary);
      transition: background 0.25s, color 0.25s;
    }

    /* ── Header ───────────────────────────────────────────────── */
    .app-header {
      background: var(--bg-surface);
      border-bottom: 1px solid var(--border);
      padding: 16px 24px;
      margin-bottom: 20px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .app-header h2 {
      font-family: "IBM Plex Mono", monospace;
      color: var(--accent);
      font-size: 22px;
      font-weight: 600;
      margin: 0 0 2px 0;
      letter-spacing: -0.5px;
    }
    .app-header p { color: var(--text-muted); font-size: 13px; margin: 0; }

    /* ── Theme toggle ─────────────────────────────────────────── */
    #theme-toggle {
      display: flex;
      align-items: center;
      gap: 8px;
      background: var(--bg-inset);
      border: 1px solid var(--border);
      border-radius: 20px;
      padding: 6px 14px 6px 10px;
      cursor: pointer;
      font-family: "IBM Plex Mono", monospace;
      font-size: 12px;
      color: var(--text-secondary);
      transition: all 0.2s;
      white-space: nowrap;
      flex-shrink: 0;
    }
    #theme-toggle:hover { background: var(--bg-surface); color: var(--accent); border-color: var(--accent); }
    #theme-toggle .toggle-track {
      width: 32px; height: 18px;
      background: var(--border-sub);
      border-radius: 9px;
      position: relative;
      transition: background 0.2s;
      flex-shrink: 0;
    }
    body.light-mode #theme-toggle .toggle-track { background: var(--accent); }
    #theme-toggle .toggle-thumb {
      width: 12px; height: 12px;
      background: #fff;
      border-radius: 50%;
      position: absolute;
      top: 3px; left: 3px;
      transition: left 0.2s;
      box-shadow: 0 1px 3px rgba(0,0,0,0.3);
    }
    body.light-mode #theme-toggle .toggle-thumb { left: 17px; }
    #theme-toggle .toggle-icon { font-size: 14px; line-height: 1; }

    /* ── Sidebar ──────────────────────────────────────────────── */
    .sidebar-box {
      background: var(--bg-surface);
      border: 1px solid var(--border);
      border-radius: 10px;
      padding: 18px;
    }
    .sidebar-box .section-title {
      font-family: "IBM Plex Mono", monospace;
      font-size: 11px; font-weight: 600;
      letter-spacing: 1.5px; text-transform: uppercase;
      color: var(--accent);
      margin-bottom: 10px; margin-top: 4px;
    }
    .sidebar-box hr { border-color: var(--border); margin: 14px 0; }

    /* ── Form inputs ──────────────────────────────────────────── */
    label { color: var(--text-secondary) !important; font-size: 12px !important; }
    .form-control, .selectize-input {
      background: var(--bg-inset) !important;
      border: 1px solid var(--border-sub) !important;
      color: var(--text-primary) !important;
      border-radius: 6px !important; font-size: 13px !important;
    }
    .form-control:focus { border-color: var(--accent) !important; box-shadow: 0 0 0 2px rgba(96,165,250,0.2) !important; }
    .selectize-dropdown { background: var(--bg-surface) !important; border: 1px solid var(--border-sub) !important; }
    .selectize-dropdown-content .option { color: var(--text-primary) !important; font-size: 13px; }
    .selectize-dropdown-content .option.active { background: var(--border) !important; }

    /* ── Sidebar buttons ──────────────────────────────────────── */
    .btn-add { background: #166534; border: none; color: #4ade80; font-size: 12px; border-radius: 6px; width: 100%; padding: 7px; margin-top: 6px; }
    .btn-add:hover { background: #14532d; color: #86efac; }
    .btn-danger-soft { background: var(--bg-inset); border: 1px solid #7f1d1d; color: #f87171; font-size: 12px; border-radius: 6px; width: 100%; padding: 6px; margin-top: 4px; }
    .btn-danger-soft:hover { background: #450a0a; color: #fca5a5; }
    .btn-file { background: var(--border) !important; border: none !important; color: var(--accent-soft) !important; font-size: 12px !important; }
    .shiny-input-container > .input-group .form-control { font-size: 12px; }

    /* ── Tabs ─────────────────────────────────────────────────── */
    .nav-tabs { border-bottom: 1px solid var(--border) !important; background: transparent; }
    .nav-tabs > li > a {
      font-family: "IBM Plex Mono", monospace; font-size: 12px;
      color: var(--text-muted) !important; background: transparent !important;
      border: none !important; padding: 10px 16px; border-radius: 6px 6px 0 0 !important;
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
      color: var(--accent) !important;
      background: var(--bg-surface) !important;
      border-bottom: 2px solid var(--accent) !important;
    }
    .tab-content {
      background: var(--bg-surface); border: 1px solid var(--border);
      border-top: none; border-radius: 0 0 10px 10px; padding: 16px;
    }

    /* ── Empty state ──────────────────────────────────────────── */
    .empty-state { text-align: center; padding: 80px 40px; color: var(--text-faint); }
    .empty-state h4 { color: var(--empty-text); font-family: "IBM Plex Mono", monospace; }

    /* ── Stat pills ───────────────────────────────────────────── */
    .pill { display: inline-block; padding: 3px 10px; border-radius: 20px; font-size: 12px; font-family: "IBM Plex Mono", monospace; margin: 2px; }
    .pill-rows  { background: #0c2a1a; color: #4ade80; border: 1px solid #14532d; }
    .pill-cols  { background: #0c1a2e; color: #60a5fa; border: 1px solid #1e3a5f; }
    .pill-pk    { background: #2a1a00; color: #fbbf24; border: 1px solid #78350f; }
    .pill-fk    { background: #1a0a2e; color: #c084fc; border: 1px solid #4c1d95; }
    .pill-warn  { background: #1a0a0a; color: #f87171; border: 1px solid #7f1d1d; }
    body.light-mode .pill-rows { background:#dcfce7; color:#15803d; border-color:#bbf7d0; }
    body.light-mode .pill-cols { background:#dbeafe; color:#1d4ed8; border-color:#bfdbfe; }
    body.light-mode .pill-pk   { background:#fef9c3; color:#854d0e; border-color:#fde68a; }
    body.light-mode .pill-fk   { background:#f3e8ff; color:#6b21a8; border-color:#d8b4fe; }
    body.light-mode .pill-warn { background:#fee2e2; color:#b91c1c; border-color:#fca5a5; }

    /* ── Table cards ──────────────────────────────────────────── */
    .tbl-card { background: var(--tbl-card-bg); border: 1px solid var(--border); border-radius: 8px; padding: 16px; margin-bottom: 14px; }
    .tbl-card-title { font-family: "IBM Plex Mono", monospace; font-size: 15px; color: var(--accent); margin: 0 0 10px 0; }

    /* ── DataTables ───────────────────────────────────────────── */
    table.dataTable { background: var(--tbl-card-bg) !important; color: var(--text-secondary) !important; border-color: var(--border) !important; }
    table.dataTable thead th { background: var(--bg-surface) !important; color: var(--text-secondary) !important; border-color: var(--border-sub) !important; font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 0.5px; }
    table.dataTable tbody tr { background: var(--tbl-card-bg) !important; }
    table.dataTable tbody tr:hover { background: var(--tbl-row-hover) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--text-muted) !important; }
    .dataTables_wrapper .dataTables_info { color: var(--text-faint) !important; font-size: 12px; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: var(--border) !important; color: var(--accent) !important; border-color: var(--border) !important; }

    /* ── Relationships ────────────────────────────────────────── */
    .rel-row { display: flex; align-items: center; gap: 8px; padding: 8px 0; border-bottom: 1px solid var(--rel-border); font-size: 13px; }
    .rel-row:last-child { border-bottom: none; }
    .rel-table { font-weight: 600; color: var(--accent); font-family: "IBM Plex Mono", monospace; }
    .rel-col   { color: var(--text-secondary); font-family: "IBM Plex Mono", monospace; font-size: 12px; }
    .rel-arrow { color: var(--border-sub); font-size: 16px; }
    .rel-method { font-size: 10px; padding: 2px 7px; border-radius: 10px; font-family: "IBM Plex Mono", monospace; margin-left: auto; }
    .m-naming    { background: #0c2a1a; color: #4ade80; }
    .m-uniqueness { background: #1a1000; color: #fb923c; }
    .m-manual    { background: #1a0a0a; color: #f87171; }
    body.light-mode .m-naming    { background:#dcfce7; color:#15803d; }
    body.light-mode .m-uniqueness { background:#fff7ed; color:#c2410c; }
    body.light-mode .m-manual    { background:#fee2e2; color:#b91c1c; }
    .rel-section-hdr { font-family: "IBM Plex Mono", monospace; font-size: 11px; letter-spacing: 1.5px; text-transform: uppercase; padding: 10px 0 4px; color: var(--text-faint); }

    /* ── Legend ───────────────────────────────────────────────── */
    .legend { display: flex; gap: 16px; padding: 8px 0 12px; flex-wrap: wrap; }
    .legend-item { display: flex; align-items: center; gap: 6px; font-size: 12px; color: var(--text-muted); font-family: "IBM Plex Mono", monospace; }
    .legend-dot { width: 10px; height: 10px; border-radius: 50%; }

    /* ── Download button ──────────────────────────────────────── */
    .dl-btn { background: var(--accent-bg); border: 1px solid var(--border); color: var(--accent); font-size: 12px; border-radius: 6px; padding: 7px 14px; font-family: "IBM Plex Mono", monospace; }
    .dl-btn:hover { background: var(--border); color: var(--accent-soft); }

    /* ── visNetwork container ─────────────────────────────────── */
    #erd_plot { border-radius: 8px; overflow: hidden; }

    /* ── visNetwork navigation buttons ───────────────────────── */
    .vis-navigation .vis-button {
      width: 30px !important;
      height: 30px !important;
      border-radius: 6px !important;
      background-color: var(--nav-btn-bg) !important;
      border: 1px solid var(--nav-btn-border) !important;
      background-image: none !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      cursor: pointer !important;
      transition: background 0.15s, border-color 0.15s !important;
      box-shadow: none !important;
      opacity: 1 !important;
    }
    .vis-navigation .vis-button:hover {
      background-color: var(--nav-btn-hover) !important;
      border-color: var(--accent) !important;
    }
    /* Replace default background-image icons with pseudo-element glyphs */
    .vis-navigation .vis-button::after {
      content: "";
      display: block;
      width: 10px; height: 10px;
      border: 2px solid var(--nav-btn-icon);
      border-radius: 1px;
    }
    .vis-navigation .vis-button.vis-up::after    { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(-135deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-down::after  { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(45deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-left::after  { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(135deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-right::after { border-color: transparent; border-bottom-color: var(--nav-btn-icon); border-right-color: var(--nav-btn-icon); transform: rotate(-45deg); border-radius: 0; }
    .vis-navigation .vis-button.vis-zoomIn::after  { content: "+"; border: none; color: var(--nav-btn-icon); font-size: 18px; font-weight: 700; font-family: monospace; line-height: 1; width: auto; height: auto; }
    .vis-navigation .vis-button.vis-zoomOut::after { content: "−"; border: none; color: var(--nav-btn-icon); font-size: 20px; font-weight: 700; font-family: monospace; line-height: 1; width: auto; height: auto; }
    .vis-navigation .vis-button.vis-zoomExtends::after { content: "⊡"; border: none; color: var(--nav-btn-icon); font-size: 16px; line-height: 1; width: auto; height: auto; }

    /* hint text below graph */
    .erd-hint { font-size: 11px; color: var(--text-faint); margin-top: 8px; }

    /* code tags in sidebar */
    code { background: var(--code-bg) !important; color: var(--accent) !important; padding: 1px 4px; border-radius: 3px; }

    /* Loaded tables list */
    .loaded-table-item {
      display: flex; align-items: center; justify-content: space-between;
      padding: 5px 8px; margin: 3px 0;
      background: var(--bg-inset); border: 1px solid var(--border);
      border-radius: 5px; font-size: 11px; font-family: "IBM Plex Mono", monospace;
      color: var(--text-secondary);
    }
    .loaded-table-item .tbl-name { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .loaded-table-item .tbl-meta { color: var(--text-faint); font-size: 10px; margin: 0 6px; white-space: nowrap; }
    .loaded-table-item .btn-remove {
      background: none; border: none; color: #f87171; cursor: pointer;
      font-size: 13px; padding: 0 2px; line-height: 1; flex-shrink: 0;
    }
    .loaded-table-item .btn-remove:hover { color: #fca5a5; }
    .loaded-tables-empty { font-size: 11px; color: var(--text-faint); font-family: "IBM Plex Mono", monospace; padding: 4px 0; }
    .cache-badge {
      display: inline-block; font-size: 10px; font-family: "IBM Plex Mono", monospace;
      padding: 1px 6px; border-radius: 10px; margin-left: 6px;
      background: var(--bg-inset); color: var(--text-faint); border: 1px solid var(--border);
      vertical-align: middle;
    }
    .cache-badge.fresh { color: #4ade80; border-color: #14532d; background: #0c2a1a; }

    /* ── Sticky node panel internals ─────────────────────────── */
    #node-panel-overlay .panel-section {
      padding: 12px 16px; border-bottom: 1px solid #1e3a5f;
    }
    #node-panel-overlay .panel-section:last-child { border-bottom: none; }
    #node-panel-overlay .panel-label {
      font-size: 10px; letter-spacing: 1px; color: #64748b;
      text-transform: uppercase; margin-bottom: 6px;
    }
    #node-panel-overlay .col-chip {
      display: inline-block; margin: 2px; padding: 2px 7px;
      border-radius: 4px; font-size: 11px; border: 1px solid;
    }
    body.light-mode #node-panel-overlay { background: #ffffff !important; border-color: #cbd5e1 !important; }
    body.light-mode #node-panel-overlay .panel-section { border-color: #e2e8f0 !important; }
    body.light-mode #node-panel-overlay .panel-label { color: #94a3b8; }

    /* ── Name Changes tab ─────────────────────────────────────── */
    .rename-summary {
      font-size: 12px; color: var(--text-faint); font-family: "IBM Plex Mono", monospace;
      margin-bottom: 12px; padding: 8px 12px;
      background: var(--bg-inset); border: 1px solid var(--border); border-radius: 6px;
    }
    .rename-summary b { color: var(--accent); }
  '
    )),
    tags$script(HTML(
      '
      document.addEventListener("DOMContentLoaded", function() {
        // Theme toggle
        var btn = document.getElementById("theme-toggle");
        if (btn) {
          btn.addEventListener("click", function() {
            document.body.classList.toggle("light-mode");
            var isLight = document.body.classList.contains("light-mode");
            document.getElementById("toggle-label").textContent = isLight ? "Dark mode" : "Light mode";
          });
        }
        // Per-table remove buttons (delegated — buttons are rendered dynamically)
        document.body.addEventListener("click", function(e) {
          if (e.target.classList.contains("btn-remove")) {
            Shiny.setInputValue("remove_table_name", e.target.dataset.tname, {priority: "event"});
          }
        });
        // Sticky node panel close button
        document.body.addEventListener("click", function(e) {
          if (e.target.id === "node-panel-close") {
            document.getElementById("node-panel-overlay").style.display = "none";
            Shiny.setInputValue("vis_close_panel", Math.random(), {priority: "event"});
          }
        });
        // Light mode: flip node panel to light
        var observer = new MutationObserver(function() {
          var panel = document.getElementById("node-panel-overlay");
          if (!panel) return;
          if (document.body.classList.contains("light-mode")) {
            panel.style.background = "#ffffff";
            panel.style.borderColor = "#cbd5e1";
            panel.style.color = "#0f172a";
          } else {
            panel.style.background = "#0f172a";
            panel.style.borderColor = "#1e3a5f";
            panel.style.color = "#e2e8f0";
          }
        });
        observer.observe(document.body, {attributes: true, attributeFilter: ["class"]});
      });
      // Called by visNetwork click event via Shiny.setInputValue
      function showNodePanel(nodeId) {
        if (!nodeId) return;
        var panel = document.getElementById("node-panel-overlay");
        if (panel) panel.style.display = "block";
      }
    '
    ))
  ),

  # Header
  div(
    class = "app-header",
    div(
      h2("TABLE_RELATIONSHIP_EXPLORER"),
      p(
        "Upload CSV files to discover primary keys, foreign keys, and inter-table links"
      )
    ),
    tags$button(
      id = "theme-toggle",
      div(class = "toggle-track", div(class = "toggle-thumb")),
      span(class = "toggle-icon", "\U0001F319"),
      span(id = "toggle-label", "Light mode")
    )
  ),

  fluidRow(
    style = "margin: 0 12px;",

    # ---- Sidebar ----
    column(
      3,
      div(
        class = "sidebar-box",

        div(class = "section-title", "01 // Upload Tables"),
        fileInput(
          "csv_files",
          NULL,
          multiple = TRUE,
          accept = ".csv",
          placeholder = "No files selected",
          buttonLabel = "Add CSV(s)"
        ),
        fileInput(
          "mdb_file",
          NULL,
          multiple = FALSE,
          accept = c(".mdb", ".accdb"),
          placeholder = "No file selected",
          buttonLabel = "Add Access DB"
        ),
        div(
          style = "font-size:10px;color:var(--text-faint);margin-top:-8px;margin-bottom:4px;line-height:1.5;",
          "Needs ",
          tags$code("RJDBC"),
          " package + Java.",
          " JARs auto-download on first use."
        ),
        uiOutput("loaded_tables_ui"),
        actionButton(
          "btn_clear_tables",
          "✕  Remove All Tables",
          class = "btn-danger-soft"
        ),

        tags$hr(),
        div(class = "section-title", "02 // Detection Method"),
        selectInput(
          "detect_method",
          NULL,
          choices = c(
            "Both (naming + uniqueness)" = "both",
            "Naming conventions only" = "naming",
            "Uniqueness / value subset" = "uniqueness",
            "Manual only" = "manual"
          ),
          selected = "both"
        ),
        div(
          style = "font-size: 11px; color: var(--text-faint); margin-top: -6px; line-height: 1.5;",
          "Naming: detects columns like",
          tags$code("id"),
          ",",
          tags$code("{table}_id"),
          "— Uniqueness: checks value overlap with other tables' PKs."
        ),

        tags$hr(),
        div(class = "section-title", "03 // Manual Override"),
        div(
          style = "font-size: 11px; color: #475569; margin-bottom: 8px;",
          "Add or override relationships directly."
        ),
        uiOutput("ui_man_from_table"),
        uiOutput("ui_man_from_col"),
        uiOutput("ui_man_to_table"),
        uiOutput("ui_man_to_col"),
        actionButton("btn_add_rel", "＋ Add Relationship", class = "btn-add"),
        actionButton(
          "btn_clear_manual",
          "✕  Clear Manual",
          class = "btn-danger-soft"
        )
      )
    ),

    # ---- Main Panel ----
    column(
      9,
      tabsetPanel(
        id = "main_tabs",

        # --- ERD ---
        tabPanel(
          "ERD Diagram",
          br(),
          conditionalPanel(
            "output.has_tables == 'false'",
            div(
              class = "empty-state",
              div(style = "font-size: 48px; margin-bottom: 16px;", "◫"),
              h4("No tables loaded"),
              p(
                style = "color:#334155; font-size:13px;",
                "Upload one or more CSV files using the sidebar to begin."
              )
            )
          ),
          conditionalPanel(
            "output.has_tables == 'true'",
            div(
              class = "legend",
              div(
                class = "legend-item",
                div(class = "legend-dot", style = "background:#4ADE80;"),
                "naming"
              ),
              div(
                class = "legend-item",
                div(
                  class = "legend-dot",
                  style = "background:#FB923C; border-style:dashed;"
                ),
                "uniqueness (dashed)"
              ),
              div(
                class = "legend-item",
                div(class = "legend-dot", style = "background:#F87171;"),
                "manual"
              )
            ),
            visNetworkOutput("erd_plot", height = "540px"),
            div(
              class = "erd-hint",
              "drag nodes · scroll to zoom · hover for details · click to highlight connections"
            )
          )
        ),

        # --- Table Details ---
        tabPanel("Table Details", br(), uiOutput("table_details_ui")),

        # --- Relationships ---
        tabPanel(
          "Relationships",
          br(),
          uiOutput("relationships_ui"),
          br(),
          downloadButton("dl_rels", "⬇  Export CSV", class = "dl-btn")
        ),

        # --- Name Changes ---
        tabPanel("Name Changes", br(), uiOutput("rename_log_ui"))
      )
    )
  ),

  # ---- Sticky node detail panel (overlay) ----
  tags$div(
    id = "node-panel-overlay",
    style = paste0(
      "display:none;position:fixed;top:80px;right:20px;z-index:9999;",
      "width:360px;max-height:80vh;overflow-y:auto;",
      "background:#0f172a;border:1px solid #1e3a5f;border-radius:10px;",
      "box-shadow:0 8px 32px rgba(0,0,0,0.7);",
      "font-family:'IBM Plex Mono',monospace;"
    ),
    tags$div(
      style = "display:flex;justify-content:space-between;align-items:center;padding:12px 16px 8px;border-bottom:1px solid #1e3a5f;",
      tags$span(
        id = "node-panel-title",
        style = "color:#60a5fa;font-size:13px;font-weight:700;",
        "Table Details"
      ),
      tags$button(
        id = "node-panel-close",
        style = "background:none;border:none;color:#64748b;font-size:18px;cursor:pointer;padding:0;line-height:1;",
        "\u00d7"
      )
    ),
    uiOutput("node_panel_ui")
  ),
  br()
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  # ============================================================
  # State: accumulated tables (name -> data.frame)
  # ============================================================
  all_tables_rv <- reactiveVal(list())
  rename_log_rv <- reactiveVal(data.frame(
    object_type = character(),
    source = character(),
    original_name = character(),
    cleaned_name = character(),
    stringsAsFactors = FALSE
  ))
  selected_node_rv <- reactiveVal(NULL)
  # metadata store: name -> list(size, mtime, nrow, ncol)
  table_meta_rv <- reactiveVal(list())
  # pending conflict queue: list of conflict descriptor lists
  pending_conflicts_rv <- reactiveVal(list())

  # ---- Add files when fileInput fires ----
  observeEvent(input$csv_files, {
    req(input$csv_files)
    existing <- all_tables_rv()
    existing_meta <- table_meta_rv()
    n_files <- nrow(input$csv_files)

    # Read all files (raw + clean) with a progress bar
    raw_and_clean <- withProgress(message = "Reading CSV files...", value = 0, {
      lapply(seq_len(n_files), function(i) {
        incProgress(1 / n_files, detail = input$csv_files$name[i])
        tryCatch(
          {
            raw_df <- read.csv(
              input$csv_files$datapath[i],
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            clean_df <- janitor::clean_names(raw_df)
            finfo <- file.info(input$csv_files$datapath[i])
            list(
              ok = TRUE,
              raw = raw_df,
              clean = clean_df,
              meta = list(
                size = input$csv_files$size[i],
                mtime = finfo$mtime,
                nrow = nrow(clean_df),
                ncol = ncol(clean_df)
              )
            )
          },
          error = function(e) {
            message(
              "Read error on ",
              input$csv_files$name[i],
              ": ",
              conditionMessage(e)
            )
            list(ok = FALSE)
          }
        )
      })
    })

    valid <- vapply(raw_and_clean, `[[`, logical(1), "ok")
    valid_rc <- raw_and_clean[valid]
    new_names <- tools::file_path_sans_ext(input$csv_files$name[valid])
    failed <- input$csv_files$name[!valid]

    for (fn in failed) {
      showNotification(
        paste0("Could not read: ", fn),
        type = "error",
        duration = 8
      )
    }
    if (length(valid_rc) == 0) {
      showNotification(
        "No valid CSV files could be loaded.",
        type = "error",
        duration = 8
      )
      return()
    }

    # ── Classify each incoming file ──────────────────────────────────
    new_files <- list() # name -> rc (no clash — just add)
    exact_dupes <- character(0) # names that are exact duplicates
    conflicts <- list() # list of conflict descriptors for the modal

    for (i in seq_along(valid_rc)) {
      nm <- new_names[i]
      rc <- valid_rc[[i]]
      inc <- rc$meta # incoming metadata

      if (!nm %in% names(existing)) {
        # No clash at all — straightforward add
        new_files[[nm]] <- rc
      } else {
        # Name clash — compare metadata
        ex <- existing_meta[[nm]]
        same_size <- !is.null(ex) &&
          identical(as.numeric(inc$size), as.numeric(ex$size))
        same_dims <- !is.null(ex) &&
          identical(inc$nrow, ex$nrow) &&
          identical(inc$ncol, ex$ncol)

        if (same_size && same_dims) {
          # Almost certainly the same file
          exact_dupes <- c(exact_dupes, nm)
        } else {
          # Different content — queue for user resolution
          conflicts[[length(conflicts) + 1]] <- list(
            name = nm,
            rc = rc,
            existing_meta = ex,
            incoming_meta = inc
          )
        }
      }
    }

    # Warn about exact duplicates
    for (nm in exact_dupes) {
      showNotification(
        paste0(
          "\u26a0 '",
          nm,
          "': identical file already loaded — no changes made."
        ),
        type = "warning",
        duration = 6
      )
    }

    # Apply the non-conflicting new files immediately
    if (length(new_files) > 0) {
      .apply_new_files(new_files, existing, existing_meta, valid_rc, new_names)
    }

    # Queue conflicts and show first modal
    if (length(conflicts) > 0) {
      pending_conflicts_rv(conflicts)
      .show_conflict_modal(conflicts[[1]])
    }
  })

  # ── Helper: apply a named list of rc entries to state ─────────────
  .apply_new_files <- function(
    file_map,
    existing,
    existing_meta,
    valid_rc,
    new_names
  ) {
    merged <- existing
    merged_meta <- existing_meta

    for (nm in names(file_map)) {
      rc <- file_map[[nm]]
      merged[[nm]] <- rc$clean
      merged_meta[[nm]] <- rc$meta
    }
    all_tables_rv(merged)
    table_meta_rv(merged_meta)

    # Rename log
    log_rows <- lapply(names(file_map), function(nm) {
      rc <- file_map[[nm]]
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (!any(changed)) {
        return(NULL)
      }
      data.frame(
        object_type = "column",
        source = nm,
        original_name = orig[changed],
        cleaned_name = cleaned[changed],
        stringsAsFactors = FALSE
      )
    })
    log_rows <- Filter(Negate(is.null), log_rows)
    if (length(log_rows) > 0) {
      prev <- rename_log_rv()
      prev <- prev[!prev$source %in% names(file_map), , drop = FALSE]
      rename_log_rv(rbind(prev, do.call(rbind, log_rows)))
    }

    n <- length(file_map)
    showNotification(
      paste0(n, " table(s) added \u2014 ", length(merged), " total"),
      type = "message",
      duration = 4
    )
  }

  # ── Show conflict resolution modal ────────────────────────────────
  .show_conflict_modal <- function(conflict) {
    nm <- conflict$name
    ex <- conflict$existing_meta
    inc <- conflict$incoming_meta

    fmt_meta <- function(m) {
      if (is.null(m)) {
        return("unknown")
      }
      paste0(
        format(m$nrow, big.mark = ","),
        " rows \u00d7 ",
        m$ncol,
        " cols",
        " | ",
        round(m$size / 1024, 1),
        " KB"
      )
    }

    showModal(modalDialog(
      title = tagList(
        tags$span(
          style = "color:#f59e0b;font-family:'IBM Plex Mono',monospace;",
          paste0("\u26a0 Duplicate table name: '", nm, "'")
        )
      ),
      tags$p(
        style = "font-size:13px;color:#94a3b8;",
        "A table named ",
        tags$b(nm),
        " is already loaded. The incoming file has different metadata:"
      ),
      tags$table(
        style = "width:100%;border-collapse:collapse;font-family:'IBM Plex Mono',monospace;font-size:12px;",
        tags$thead(
          tags$tr(
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#64748b;text-align:left;",
              ""
            ),
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#60a5fa;text-align:left;",
              "Existing"
            ),
            tags$th(
              style = "padding:6px 10px;border-bottom:1px solid #334155;color:#4ade80;text-align:left;",
              "Incoming"
            )
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(style = "padding:5px 10px;color:#64748b;", "Dimensions"),
            tags$td(
              style = "padding:5px 10px;",
              if (!is.null(ex)) {
                paste0(format(ex$nrow, big.mark = ","), " \u00d7 ", ex$ncol)
              } else {
                "—"
              }
            ),
            tags$td(
              style = "padding:5px 10px;",
              paste0(format(inc$nrow, big.mark = ","), " \u00d7 ", inc$ncol)
            )
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;color:#64748b;", "File size"),
            tags$td(
              style = "padding:5px 10px;",
              if (!is.null(ex)) paste0(round(ex$size / 1024, 1), " KB") else "—"
            ),
            tags$td(
              style = "padding:5px 10px;",
              paste0(round(inc$size / 1024, 1), " KB")
            )
          )
        )
      ),
      br(),
      tags$p(
        style = "font-size:12px;color:#64748b;",
        "How should this be resolved?"
      ),
      footer = tagList(
        actionButton(
          "conflict_overwrite",
          "\u2b06 Replace existing",
          style = "background:#1e3a5f;color:#60a5fa;border:1px solid #334155;font-size:12px;"
        ),
        actionButton(
          "conflict_keep_both",
          "\u2795 Keep both (rename incoming to '",
          tags$code(paste0(nm, "_2")),
          "')",
          style = "background:#0c2a1a;color:#4ade80;border:1px solid #14532d;font-size:12px;"
        ),
        actionButton(
          "conflict_skip",
          "\u274c Skip incoming",
          style = "background:#1a0a0a;color:#f87171;border:1px solid #7f1d1d;font-size:12px;"
        )
      ),
      easyClose = FALSE,
      size = "m"
    ))
  }

  # ── Conflict resolution handlers ──────────────────────────────────
  resolve_conflict <- function(action) {
    conflicts <- pending_conflicts_rv()
    if (length(conflicts) == 0) {
      return()
    }

    conflict <- conflicts[[1]]
    nm <- conflict$name
    rc <- conflict$rc

    existing <- all_tables_rv()
    existing_meta <- table_meta_rv()

    if (action == "overwrite") {
      existing[[nm]] <- rc$clean
      existing_meta[[nm]] <- rc$meta
      all_tables_rv(existing)
      table_meta_rv(existing_meta)
      # Update rename log
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (any(changed)) {
        prev <- rename_log_rv()
        prev <- prev[prev$source != nm, , drop = FALSE]
        rename_log_rv(rbind(
          prev,
          data.frame(
            object_type = rep("column", sum(changed)),
            source = nm,
            original_name = orig[changed],
            cleaned_name = cleaned[changed],
            stringsAsFactors = FALSE
          )
        ))
      }
      showNotification(
        paste0("'", nm, "' replaced."),
        type = "message",
        duration = 3
      )
    } else if (action == "keep_both") {
      # Find a free name: nm_2, nm_3, ...
      new_nm <- nm
      suffix <- 2
      while (new_nm %in% names(existing)) {
        new_nm <- paste0(nm, "_", suffix)
        suffix <- suffix + 1
      }
      existing[[new_nm]] <- rc$clean
      existing_meta[[new_nm]] <- rc$meta
      all_tables_rv(existing)
      table_meta_rv(existing_meta)
      orig <- names(rc$raw)
      cleaned <- names(rc$clean)
      changed <- orig != cleaned
      if (any(changed)) {
        prev <- rename_log_rv()
        rename_log_rv(rbind(
          prev,
          data.frame(
            object_type = rep("column", sum(changed)),
            source = new_nm,
            original_name = orig[changed],
            cleaned_name = cleaned[changed],
            stringsAsFactors = FALSE
          )
        ))
      }
      showNotification(
        paste0("Saved as '", new_nm, "'."),
        type = "message",
        duration = 3
      )
    } else {
      # skip
      showNotification(
        paste0("'", nm, "' incoming file discarded."),
        type = "warning",
        duration = 3
      )
    }

    removeModal()
    remaining <- conflicts[-1]
    pending_conflicts_rv(remaining)
    if (length(remaining) > 0) .show_conflict_modal(remaining[[1]])
  }

  observeEvent(input$conflict_overwrite, resolve_conflict("overwrite"))
  observeEvent(input$conflict_keep_both, resolve_conflict("keep_both"))
  observeEvent(input$conflict_skip, resolve_conflict("skip"))

  # ---- Add Access DB when mdb fileInput fires ----
  observeEvent(input$mdb_file, {
    req(input$mdb_file)
    path <- input$mdb_file$datapath
    src_name <- tools::file_path_sans_ext(input$mdb_file$name)
    existing <- all_tables_rv()

    errors <- character(0)
    notify_fn <- function(msg) {
      errors <<- c(errors, msg)
    }

    raw_tbls <- withProgress(
      message = paste0("Reading ", input$mdb_file$name, "..."),
      value = 0.3,
      read_access_db(path, notify_fn)
    )

    for (e in errors) {
      showNotification(e, type = "error", duration = 12)
    }
    if (length(raw_tbls) == 0) {
      return()
    }

    # Clean column names and record renames
    log_rows <- list()
    clean_tbls <- setNames(
      lapply(names(raw_tbls), function(tname) {
        raw_df <- raw_tbls[[tname]]
        clean_df <- janitor::clean_names(raw_df)
        orig <- names(raw_df)
        cleaned <- names(clean_df)
        changed <- orig != cleaned
        if (any(changed)) {
          log_rows[[length(log_rows) + 1]] <<- data.frame(
            object_type = "column",
            source = tname,
            original_name = orig[changed],
            cleaned_name = cleaned[changed],
            stringsAsFactors = FALSE
          )
        }
        clean_df
      }),
      names(raw_tbls)
    )

    # Merge into accumulated state
    merged <- existing
    for (nm in names(clean_tbls)) {
      merged[[nm]] <- clean_tbls[[nm]]
    }
    all_tables_rv(merged)

    # Append rename log, dropping stale entries for replaced tables
    if (length(log_rows) > 0) {
      prev_log <- rename_log_rv()
      prev_log <- prev_log[
        !prev_log$source %in% names(clean_tbls),
        ,
        drop = FALSE
      ]
      rename_log_rv(rbind(prev_log, do.call(rbind, log_rows)))
    }

    n_replaced <- sum(names(clean_tbls) %in% names(existing))
    n_added <- length(clean_tbls) - n_replaced
    parts <- character(0)
    if (n_added > 0) {
      parts <- c(parts, paste0(n_added, " table(s) added"))
    }
    if (n_replaced > 0) {
      parts <- c(parts, paste0(n_replaced, " replaced"))
    }
    showNotification(
      paste0(
        src_name,
        ": ",
        paste(parts, collapse = ", "),
        " \u2014 ",
        length(merged),
        " total"
      ),
      type = "message",
      duration = 5
    )
  })
  observeEvent(input$remove_table_name, {
    nm <- input$remove_table_name
    tbl <- all_tables_rv()
    tbl[[nm]] <- NULL
    all_tables_rv(tbl)
    meta <- table_meta_rv()
    meta[[nm]] <- NULL
    table_meta_rv(meta)
    log <- rename_log_rv()
    rename_log_rv(log[log$source != nm, , drop = FALSE])
    showNotification(paste0("Removed: ", nm), type = "message", duration = 3)
  })

  # ---- Clear all tables ----
  observeEvent(input$btn_clear_tables, {
    all_tables_rv(list())
    table_meta_rv(list())
    rename_log_rv(data.frame(
      object_type = character(),
      source = character(),
      original_name = character(),
      cleaned_name = character(),
      stringsAsFactors = FALSE
    ))
    fk_cache$key <- NULL
    fk_cache$result <- list()
    showNotification("All tables cleared.", type = "message", duration = 3)
  })

  # ---- Loaded tables list UI ----
  output$loaded_tables_ui <- renderUI({
    tbls <- all_tables_rv()
    if (length(tbls) == 0) {
      return(div(class = "loaded-tables-empty", "No tables loaded yet."))
    }
    tagList(
      div(
        style = "margin: 8px 0 4px;",
        lapply(names(tbls), function(nm) {
          df <- tbls[[nm]]
          div(
            class = "loaded-table-item",
            span(class = "tbl-name", nm),
            span(
              class = "tbl-meta",
              paste0(format(nrow(df), big.mark = ","), "r × ", ncol(df), "c")
            ),
            tags$button(
              class = "btn-remove",
              `data-tname` = nm,
              title = "Remove",
              "×"
            )
          )
        })
      )
    )
  })

  # ============================================================
  # Caching: FK detection is the expensive step.
  # We store the last digest + result; only recompute on change.
  # ============================================================
  fk_cache <- new.env(parent = emptyenv())
  fk_cache$key <- NULL
  fk_cache$result <- list()

  # ---- PKs (fast — no caching needed) ----
  pk_map_rv <- reactive({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    method <- if (input$detect_method == "manual") {
      "both"
    } else {
      input$detect_method
    }
    setNames(
      lapply(names(tbls), function(t) detect_pks(tbls[[t]], t, method)),
      names(tbls)
    )
  })

  # ---- FK detection with cache ----
  auto_rels_rv <- reactive({
    tbls <- all_tables_rv()
    method <- input$detect_method
    req(length(tbls) > 0)

    # Cache key: table names + dimensions + method — invalidates on any structural change
    key_parts <- paste(
      names(tbls),
      vapply(tbls, nrow, integer(1)),
      vapply(tbls, ncol, integer(1)),
      sep = ":",
      collapse = "|"
    )
    cache_key <- paste0(key_parts, "//", method)

    if (!is.null(fk_cache$key) && identical(fk_cache$key, cache_key)) {
      return(fk_cache$result)
    }

    result <- withProgress(
      message = "Detecting relationships...",
      value = 0.5,
      tryCatch(
        detect_fks(tbls, method),
        error = function(e) {
          showNotification(
            paste0("Relationship detection error: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          list()
        }
      )
    )

    fk_cache$key <- cache_key
    fk_cache$result <- result
    result
  })

  # ---- Manual rels ----
  manual_rels_rv <- reactiveVal(list())
  all_rels_rv <- reactive({
    c(auto_rels_rv(), manual_rels_rv())
  })

  # ---- has_tables ----
  output$has_tables <- reactive({
    if (length(all_tables_rv()) > 0) "true" else "false"
  })
  outputOptions(output, "has_tables", suspendWhenHidden = FALSE)

  # ---- Manual relationship UI ----
  output$ui_man_from_table <- renderUI({
    req(length(all_tables_rv()) > 0)
    selectInput(
      "man_from_table",
      "From Table:",
      choices = names(all_tables_rv())
    )
  })
  output$ui_man_from_col <- renderUI({
    tbls <- all_tables_rv()
    req(
      length(tbls) > 0,
      input$man_from_table,
      input$man_from_table %in% names(tbls)
    )
    selectInput(
      "man_from_col",
      "From Column:",
      choices = names(tbls[[input$man_from_table]])
    )
  })
  output$ui_man_to_table <- renderUI({
    req(length(all_tables_rv()) > 0)
    selectInput("man_to_table", "To Table:", choices = names(all_tables_rv()))
  })
  output$ui_man_to_col <- renderUI({
    tbls <- all_tables_rv()
    req(
      length(tbls) > 0,
      input$man_to_table,
      input$man_to_table %in% names(tbls)
    )
    selectInput(
      "man_to_col",
      "To Column (PK):",
      choices = names(tbls[[input$man_to_table]])
    )
  })

  observeEvent(input$btn_add_rel, {
    req(
      input$man_from_table,
      input$man_from_col,
      input$man_to_table,
      input$man_to_col
    )
    if (input$man_from_table == input$man_to_table) {
      showNotification("From and To tables must differ.", type = "warning")
      return()
    }
    manual_rels_rv(c(
      manual_rels_rv(),
      list(list(
        from_table = input$man_from_table,
        from_col = input$man_from_col,
        to_table = input$man_to_table,
        to_col = input$man_to_col,
        detected_by = "manual"
      ))
    ))
    showNotification("Relationship added.", type = "message")
  })

  observeEvent(input$btn_clear_manual, {
    manual_rels_rv(list())
    showNotification("Manual relationships cleared.", type = "message")
  })

  # ---- ERD ----
  output$erd_plot <- renderVisNetwork({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    tryCatch(
      {
        net <- build_network(tbls, all_rels_rv(), pk_map_rv())
        visNetwork(net$nodes, net$edges, background = "#0f172a") %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
            nodesIdSelection = FALSE
          ) %>%
          visLayout(randomSeed = 42) %>%
          visPhysics(
            solver = "forceAtlas2Based",
            forceAtlas2Based = list(
              gravitationalConstant = -80,
              springLength = 220,
              springConstant = 0.04,
              damping = 0.9
            ),
            stabilization = list(iterations = 300, fit = TRUE)
          ) %>%
          visEdges(smooth = list(enabled = TRUE, type = "dynamic")) %>%
          visNodes(widthConstraint = list(minimum = 130, maximum = 230)) %>%
          visInteraction(
            navigationButtons = TRUE,
            tooltipDelay = 80,
            hover = TRUE
          ) %>%
          visEvents(
            click = "function(params) {
          if (params.nodes.length > 0) {
            Shiny.setInputValue('vis_clicked_node', {
              id: params.nodes[0],
              ts: Date.now()
            }, {priority: 'event'});
            showNodePanel(params.nodes[0]);
          }
        }"
          )
      },
      error = function(e) {
        showNotification(
          paste0("ERD error: ", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        visNetwork(
          data.frame(
            id = 1,
            label = paste("Error:", conditionMessage(e)),
            color = "#7f1d1d",
            font.color = "white"
          ),
          data.frame(),
          background = "#0f172a"
        )
      }
    )
  })

  # ---- Table Details ----
  output$table_details_ui <- renderUI({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    rels <- all_rels_rv()

    tagList(
      # Export All button at the top
      div(
        style = "display:flex; justify-content:flex-end; margin-bottom:12px;",
        downloadButton(
          "dl_all_table_details",
          "\u2b07  Export All Tables (CSV)",
          class = "dl-btn"
        )
      ),
      lapply(names(tbls), function(t) {
        df <- tbls[[t]]
        pk_v <- pks[[t]]
        fk_rels <- Filter(function(r) r$from_table == t, rels)
        fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")
        dl_id <- paste0("dl_tbl_", make.names(t))

        pills <- tagList(
          span(
            class = "pill pill-rows",
            paste0(format(nrow(df), big.mark = ","), " rows")
          ),
          span(class = "pill pill-cols", paste0(ncol(df), " cols")),
          if (length(pk_v) > 0) {
            lapply(pk_v, function(p) {
              span(class = "pill pill-pk", paste0("PK: ", p))
            })
          } else {
            span(class = "pill pill-warn", "\u26a0 no PK")
          },
          if (length(fk_cols) > 0) {
            lapply(seq_along(fk_rels), function(i) {
              span(
                class = "pill pill-fk",
                paste0(
                  "FK: ",
                  fk_rels[[i]]$from_col,
                  " \u2192 ",
                  fk_rels[[i]]$to_table
                )
              )
            })
          }
        )

        div(
          class = "tbl-card",
          div(
            style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
            p(
              class = "tbl-card-title",
              style = "margin:0;",
              paste0("[ ", t, " ]")
            ),
            downloadButton(
              dl_id,
              "\u2b07 CSV",
              class = "dl-btn",
              style = "padding:4px 10px; font-size:11px;"
            )
          ),
          div(style = "margin-bottom: 12px;", pills),
          DTOutput(paste0("dt_col_", make.names(t)))
        )
      })
    )
  })

  # Dynamic DT outputs
  observe({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    rels <- all_rels_rv()

    lapply(names(tbls), function(t) {
      local({
        tname <- t
        output_id <- paste0("dt_col_", make.names(tname))
        output[[output_id]] <- renderDT(
          {
            df <- tbls[[tname]]
            pk_v <- pks[[tname]]
            fk_rels <- Filter(function(r) r$from_table == tname, rels)
            fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")

            smry <- data.frame(
              Column = names(df),
              Type = vapply(
                df,
                function(x) paste(class(x), collapse = "/"),
                character(1)
              ),
              `Non-null` = vapply(df, function(x) sum(!is.na(x)), integer(1)),
              `Unique` = vapply(
                df,
                function(x) length(unique(na.omit(x))),
                integer(1)
              ),
              `PK` = ifelse(names(df) %in% pk_v, "✓", ""),
              `FK` = ifelse(names(df) %in% fk_cols, "✓", ""),
              check.names = FALSE,
              stringsAsFactors = FALSE
            )

            datatable(
              smry,
              options = list(
                pageLength = 10,
                dom = "tp",
                scrollX = TRUE,
                columnDefs = list(list(
                  className = "dt-center",
                  targets = c(2, 3, 4, 5)
                ))
              ),
              rownames = FALSE,
              selection = "none"
            ) %>%
              formatStyle(
                "PK",
                color = styleEqual("✓", "#fbbf24"),
                backgroundColor = styleEqual("✓", "#1a1000")
              ) %>%
              formatStyle(
                "FK",
                color = styleEqual("✓", "#c084fc"),
                backgroundColor = styleEqual("✓", "#140a2e")
              )
          },
          server = FALSE
        )
      })
    })
  })

  # Helper: build column-summary data.frame for one table
  make_tbl_summary <- function(t, tbls, pks, rels) {
    df <- tbls[[t]]
    pk_v <- pks[[t]]
    fk_rels <- Filter(function(r) r$from_table == t, rels)
    fk_cols <- vapply(fk_rels, `[[`, character(1), "from_col")
    data.frame(
      table = t,
      column = names(df),
      type = vapply(
        df,
        function(x) paste(class(x), collapse = "/"),
        character(1)
      ),
      non_null = vapply(df, function(x) sum(!is.na(x)), integer(1)),
      unique_vals = vapply(
        df,
        function(x) length(unique(na.omit(x))),
        integer(1)
      ),
      is_pk = names(df) %in% pk_v,
      is_fk = names(df) %in% fk_cols,
      stringsAsFactors = FALSE
    )
  }

  # Per-table download handlers (created dynamically)
  observe({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    pks <- pk_map_rv()
    rels <- all_rels_rv()
    lapply(names(tbls), function(t) {
      local({
        tname <- t
        dl_id <- paste0("dl_tbl_", make.names(tname))
        output[[dl_id]] <- downloadHandler(
          filename = function() paste0(tname, "_details.csv"),
          content = function(file) {
            write.csv(
              make_tbl_summary(
                tname,
                all_tables_rv(),
                pk_map_rv(),
                all_rels_rv()
              ),
              file,
              row.names = FALSE
            )
          }
        )
      })
    })
  })

  # Export All: one CSV with all tables stacked, table column first
  output$dl_all_table_details <- downloadHandler(
    filename = "all_table_details.csv",
    content = function(file) {
      tbls <- all_tables_rv()
      pks <- pk_map_rv()
      rels <- all_rels_rv()
      combined <- do.call(
        rbind,
        lapply(
          names(tbls),
          make_tbl_summary,
          tbls = tbls,
          pks = pks,
          rels = rels
        )
      )
      write.csv(combined, file, row.names = FALSE)
    }
  )

  # ---- Relationships Tab ----
  output$relationships_ui <- renderUI({
    tbls <- all_tables_rv()
    req(length(tbls) > 0)
    rels <- all_rels_rv()

    if (length(rels) == 0) {
      return(div(
        class = "empty-state",
        h4("No relationships detected"),
        p(
          style = "color:#334155; font-size:13px;",
          "Try uploading more tables or adjusting the detection method."
        )
      ))
    }

    methods <- list(
      list(key = "naming", label = "Naming Convention", cls = "m-naming"),
      list(
        key = "uniqueness",
        label = "Uniqueness / Value Subset",
        cls = "m-uniqueness"
      ),
      list(key = "manual", label = "Manual", cls = "m-manual")
    )

    tagList(
      lapply(methods, function(m) {
        m_rels <- Filter(function(r) r$detected_by == m$key, rels)
        if (length(m_rels) == 0) {
          return(NULL)
        }
        tagList(
          div(
            class = "rel-section-hdr",
            paste0(m$label, " (", length(m_rels), ")")
          ),
          lapply(m_rels, function(r) {
            to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) {
              r$to_col
            } else {
              "?"
            }
            div(
              class = "rel-row",
              span(class = "rel-table", r$from_table),
              span(class = "rel-col", paste0(".", r$from_col)),
              span(class = "rel-arrow", "→"),
              span(class = "rel-table", r$to_table),
              span(class = "rel-col", paste0(".", to_col)),
              span(class = paste("rel-method", m$cls), m$key)
            )
          })
        )
      })
    )
  })

  # ---- Sticky node detail panel ----
  observeEvent(input$vis_clicked_node, {
    req(input$vis_clicked_node$id)
    selected_node_rv(input$vis_clicked_node$id)
  })

  output$node_panel_ui <- renderUI({
    node_id <- selected_node_rv()
    req(node_id)
    tbls <- all_tables_rv()
    pks <- pk_map_rv()
    rels <- all_rels_rv()

    # node_id is the integer index; map back to table name
    tnames <- names(tbls)
    req(node_id <= length(tnames))
    t <- tnames[[node_id]]
    df <- tbls[[t]]
    pk_v <- pks[[t]]
    fk_r <- Filter(function(r) r$from_table == t, rels)
    fk_cols <- vapply(fk_r, `[[`, character(1), "from_col")

    # Column chips: PK = amber, FK = purple, plain = muted
    col_chips <- lapply(names(df), function(cn) {
      if (cn %in% pk_v) {
        tags$span(
          class = "col-chip",
          style = "background:#2a1a00;color:#fbbf24;border-color:#78350f;",
          cn
        )
      } else if (cn %in% fk_cols) {
        tags$span(
          class = "col-chip",
          style = "background:#1a0a2e;color:#c084fc;border-color:#4c1d95;",
          cn
        )
      } else {
        tags$span(
          class = "col-chip",
          style = "background:rgba(255,255,255,0.04);color:#94a3b8;border-color:#1e3a5f;",
          cn
        )
      }
    })

    tagList(
      # Stats
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Overview"),
        tags$div(
          style = "display:flex;gap:12px;flex-wrap:wrap;",
          tags$span(
            class = "pill pill-rows",
            paste0(format(nrow(df), big.mark = ","), " rows")
          ),
          tags$span(class = "pill pill-cols", paste0(ncol(df), " cols"))
        )
      ),
      # PKs
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Primary Key(s)"),
        if (length(pk_v) > 0) {
          tags$div(lapply(pk_v, function(p) {
            tags$span(class = "pill pill-pk", p)
          }))
        } else {
          tags$span(style = "color:#f87171;font-size:12px;", "none detected")
        }
      ),
      # FKs
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", "Foreign Key(s)"),
        if (length(fk_r) > 0) {
          tags$div(lapply(fk_r, function(r) {
            to_col <- if (!is.na(r$to_col) && !is.null(r$to_col)) {
              r$to_col
            } else {
              "?"
            }
            tags$div(
              style = "font-size:11px;color:#c084fc;padding:1px 0;",
              paste0(r$from_col, " → ", r$to_table, ".", to_col)
            )
          }))
        } else {
          tags$span(style = "color:#64748b;font-size:12px;", "none")
        }
      ),
      # All columns
      tags$div(
        class = "panel-section",
        tags$div(class = "panel-label", paste0("Columns (", ncol(df), ")")),
        tags$div(style = "display:flex;flex-wrap:wrap;gap:3px;", col_chips)
      )
    )
  })

  # ---- Name Changes tab ----
  output$rename_log_ui <- renderUI({
    log <- rename_log_rv()
    if (nrow(log) == 0) {
      return(tagList(
        div(
          class = "rename-summary",
          "No name changes detected. All column names were already clean."
        ),
        div(
          class = "empty-state",
          div(style = "font-size:36px;margin-bottom:12px;", "✓"),
          h4("All names clean"),
          p(
            style = "font-size:13px;color:var(--text-faint);",
            "janitor::clean_names() found nothing to rename in the uploaded files."
          )
        )
      ))
    }

    n_tables <- length(unique(log$source))
    tagList(
      div(
        class = "rename-summary",
        tags$b(nrow(log)),
        " column name(s) renamed across ",
        tags$b(n_tables),
        " table(s). ",
        "Amber = PK-related, purple = FK-related columns."
      ),
      DTOutput("dt_rename_log"),
      br(),
      downloadButton("dl_rename_log", "\u2b07  Export CSV", class = "dl-btn")
    )
  })

  output$dt_rename_log <- renderDT(
    {
      log <- rename_log_rv()
      req(nrow(log) > 0)
      datatable(
        log,
        colnames = c("Type", "Table", "Original Name", "Cleaned Name"),
        options = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
        rownames = FALSE,
        selection = "none"
      )
    },
    server = FALSE
  )

  output$dl_rename_log <- downloadHandler(
    filename = "name_changes.csv",
    content = function(file) write.csv(rename_log_rv(), file, row.names = FALSE)
  )

  # ---- Download ----
  output$dl_rels <- downloadHandler(
    filename = "table_relationships.csv",
    content = function(file) {
      rels <- all_rels_rv()
      if (length(rels) == 0) {
        write.csv(
          data.frame(
            from_table = "",
            from_col = "",
            to_table = "",
            to_col = "",
            detected_by = ""
          )[0, ],
          file,
          row.names = FALSE
        )
      } else {
        df <- do.call(
          rbind,
          lapply(rels, function(r) {
            data.frame(
              from_table = r$from_table,
              from_col = r$from_col,
              to_table = r$to_table,
              to_col = if (!is.na(r$to_col) && !is.null(r$to_col)) {
                r$to_col
              } else {
                ""
              },
              detected_by = r$detected_by,
              stringsAsFactors = FALSE
            )
          })
        )
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
