# ============================================================
# file_readers.R — File Format Readers
# Table Relationship Explorer
# ============================================================

# ── Null-coalescing operator ─────────────────────────────────
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !(is.character(a) && length(a) == 1L && !nzchar(a))) a else b
}

# ── Supported file extensions ──────────────────────────────────

supported_extensions <- c(
  ".csv", ".tsv", ".txt",
  ".xlsx", ".xls", ".xlsm",
  ".ods",
  ".parquet",
  ".json", ".ndjson",
  ".sav", ".por",
  ".sas7bdat", ".xpt",
  ".dta",
  ".rds", ".rdata", ".rda",
  ".mdb", ".accdb"
)

# ── Multi-format file reader dispatcher ────────────────────────

read_table_file <- function(path, name, notify_fn = message) {
  ext <- tolower(tools::file_ext(name))
  if (!startsWith(ext, ".")) ext <- paste0(".", ext)

  result <- tryCatch(
    switch(ext,
      ".csv"      = list(tables = setNames(list(
        read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      ), tools::file_path_sans_ext(name))),

      ".tsv" = , ".txt" = list(tables = setNames(list(
        read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
      ), tools::file_path_sans_ext(name))),

      ".xlsx" = , ".xls" = , ".xlsm" = read_excel_file(path, name, notify_fn),

      ".ods" = read_ods_file(path, name, notify_fn),

      ".parquet" = read_parquet_file(path, name, notify_fn),

      ".json" = read_json_file(path, name, notify_fn),

      ".ndjson" = read_ndjson_file(path, name, notify_fn),

      ".sav" = read_haven_file(path, name, "sav", notify_fn),
      ".por" = read_haven_file(path, name, "por", notify_fn),
      ".sas7bdat" = read_haven_file(path, name, "sas", notify_fn),
      ".xpt" = read_haven_file(path, name, "xpt", notify_fn),
      ".dta" = read_haven_file(path, name, "dta", notify_fn),

      ".rds" = read_rds_file(path, name, notify_fn),
      ".rdata" = , ".rda" = read_rdata_file(path, name, notify_fn),

      ".mdb" = , ".accdb" = {
        tbls <- read_access_db(path, notify_fn)
        list(tables = tbls)
      },

      {
        notify_fn(paste0("Unsupported file format: ", ext))
        list(tables = list())
      }
    ),
    error = function(e) {
      notify_fn(paste0("Error reading ", name, ": ", conditionMessage(e)))
      list(tables = list())
    }
  )

  if (is.null(result$tables)) result$tables <- list()
  result
}

# ── Excel reader ───────────────────────────────────────────────

read_excel_file <- function(path, name, notify_fn) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    notify_fn("Install the 'readxl' package to read Excel files: install.packages('readxl')")
    return(list(tables = list()))
  }
  sheets <- readxl::excel_sheets(path)
  base <- tools::file_path_sans_ext(name)
  tbls <- list()
  for (s in sheets) {
    df <- tryCatch(
      as.data.frame(readxl::read_excel(path, sheet = s)),
      error = function(e) {
        notify_fn(paste0("Could not read sheet '", s, "': ", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(df) && nrow(df) > 0) {
      tname <- if (length(sheets) == 1) base else paste0(base, "_", janitor::make_clean_names(s))
      tbls[[tname]] <- df
    }
  }
  list(tables = tbls)
}

# ── ODS reader ─────────────────────────────────────────────────

read_ods_file <- function(path, name, notify_fn) {
  if (!requireNamespace("readODS", quietly = TRUE)) {
    notify_fn("Install the 'readODS' package to read ODS files: install.packages('readODS')")
    return(list(tables = list()))
  }
  sheets <- readODS::list_ods_sheets(path)
  base <- tools::file_path_sans_ext(name)
  tbls <- list()
  for (s in sheets) {
    df <- tryCatch(
      as.data.frame(readODS::read_ods(path, sheet = s)),
      error = function(e) {
        notify_fn(paste0("Could not read sheet '", s, "': ", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(df) && nrow(df) > 0) {
      tname <- if (length(sheets) == 1) base else paste0(base, "_", janitor::make_clean_names(s))
      tbls[[tname]] <- df
    }
  }
  list(tables = tbls)
}

# ── Parquet reader ─────────────────────────────────────────────

read_parquet_file <- function(path, name, notify_fn) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    notify_fn("Install the 'arrow' package to read Parquet files: install.packages('arrow')")
    return(list(tables = list()))
  }
  df <- as.data.frame(arrow::read_parquet(path))
  list(tables = setNames(list(df), tools::file_path_sans_ext(name)))
}

# ── JSON reader ────────────────────────────────────────────────

read_json_file <- function(path, name, notify_fn) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    notify_fn("Install the 'jsonlite' package to read JSON files: install.packages('jsonlite')")
    return(list(tables = list()))
  }
  raw <- jsonlite::fromJSON(path, flatten = TRUE)
  if (is.data.frame(raw)) {
    return(list(tables = setNames(list(raw), tools::file_path_sans_ext(name))))
  }
  if (is.list(raw) && all(vapply(raw, is.data.frame, logical(1)))) {
    return(list(tables = raw))
  }
  notify_fn(paste0("JSON in '", name, "' did not parse to a data frame."))
  list(tables = list())
}

# ── NDJSON reader ──────────────────────────────────────────────

read_ndjson_file <- function(path, name, notify_fn) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    notify_fn("Install the 'jsonlite' package to read NDJSON files: install.packages('jsonlite')")
    return(list(tables = list()))
  }
  df <- jsonlite::stream_in(file(path), verbose = FALSE)
  list(tables = setNames(list(as.data.frame(df)), tools::file_path_sans_ext(name)))
}

# ── Haven reader (SPSS, SAS, Stata) ───────────────────────────

read_haven_file <- function(path, name, fmt, notify_fn) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    notify_fn("Install the 'haven' package to read SPSS/SAS/Stata files: install.packages('haven')")
    return(list(tables = list()))
  }
  df <- switch(fmt,
    sav = haven::read_sav(path),
    por = haven::read_por(path),
    sas = haven::read_sas(path),
    xpt = haven::read_xpt(path),
    dta = haven::read_dta(path)
  )
  df <- as.data.frame(df)
  list(tables = setNames(list(df), tools::file_path_sans_ext(name)))
}

# ── RDS reader ─────────────────────────────────────────────────

read_rds_file <- function(path, name, notify_fn) {
  obj <- readRDS(path)
  if (is.data.frame(obj)) {
    return(list(tables = setNames(list(obj), tools::file_path_sans_ext(name))))
  }
  notify_fn(paste0("RDS file '", name, "' does not contain a data frame."))
  list(tables = list())
}

# ── RData reader ───────────────────────────────────────────────

read_rdata_file <- function(path, name, notify_fn) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  objs <- ls(env)
  tbls <- list()
  for (nm in objs) {
    obj <- get(nm, envir = env)
    if (is.data.frame(obj)) {
      tbls[[nm]] <- obj
    }
  }
  if (length(tbls) == 0) {
    notify_fn(paste0("No data frames found in '", name, "'."))
  }
  list(tables = tbls)
}

# ── Schema file import ─────────────────────────────────────────

parse_schema_file <- function(path, name, notify_fn = message) {
  ext <- tolower(tools::file_ext(name))
  schema <- tryCatch(
    {
      if (ext %in% c("json")) {
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          notify_fn("Install 'jsonlite' to import JSON schema files.")
          return(list(tables = list(), relationships = list()))
        }
        jsonlite::fromJSON(path, simplifyVector = FALSE)
      } else if (ext %in% c("yaml", "yml")) {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          notify_fn("Install the 'yaml' package to import YAML schema files: install.packages('yaml')")
          return(list(tables = list(), relationships = list()))
        }
        yaml::read_yaml(path)
      } else {
        notify_fn(paste0("Unsupported schema format: .", ext))
        return(list(tables = list(), relationships = list()))
      }
    },
    error = function(e) {
      notify_fn(paste0("Error parsing schema file: ", conditionMessage(e)))
      return(list(tables = list(), relationships = list()))
    }
  )

  tables <- list()
  relationships <- list()

  # Parse tables from schema + extract inline FK definitions
  if (!is.null(schema$tables)) {
    for (tdef in schema$tables) {
      tname <- tdef$name
      if (is.null(tname)) next
      cols <- tdef$columns
      if (is.null(cols)) cols <- tdef$fields
      if (!is.null(cols)) {
        col_names <- vapply(cols, function(c) c$name %||% "", character(1))
        col_names <- col_names[nzchar(col_names)]
        if (length(col_names) > 0) {
          df <- as.data.frame(
            matrix(nrow = 0, ncol = length(col_names)),
            stringsAsFactors = FALSE
          )
          names(df) <- col_names
          tables[[tname]] <- df
        }
        # Extract inline foreign_key definitions
        for (cdef in cols) {
          fk <- cdef$foreign_key
          if (!is.null(fk)) {
            rel <- list(
              from_table  = tname,
              from_col    = cdef$name %||% "",
              to_table    = fk$table %||% "",
              to_col      = fk$column %||% "",
              detected_by = "schema",
              confidence  = "high",
              score       = 1.0,
              signals     = list(schema = 1.0),
              reasons     = "schema-defined"
            )
            if (nzchar(rel$from_table) && nzchar(rel$to_table)) {
              relationships[[length(relationships) + 1]] <- rel
            }
          }
        }
      }
    }
  }

  # Parse top-level relationships array (if present)
  if (!is.null(schema$relationships)) {
    for (rdef in schema$relationships) {
      rel <- list(
        from_table  = rdef$from_table %||% rdef$from %||% "",
        from_col    = rdef$from_column %||% rdef$from_col %||% "",
        to_table    = rdef$to_table %||% rdef$to %||% "",
        to_col      = rdef$to_column %||% rdef$to_col %||% "",
        detected_by = "schema",
        confidence  = "high",
        score       = 1.0,
        signals     = list(schema = 1.0),
        reasons     = "schema-defined"
      )
      if (nzchar(rel$from_table) && nzchar(rel$to_table)) {
        relationships[[length(relationships) + 1]] <- rel
      }
    }
  }

  list(tables = tables, relationships = relationships)
}

# ── Access database support ──────────────────────────────────

access_jar_dir <- function() {
  app_jars <- file.path(dirname(sys.frame(1)$ofile %||% "."), "access_jars")
  if (dir.exists(app_jars) || dir.create(app_jars, showWarnings = FALSE)) {
    return(app_jars)
  }
  tools::R_user_dir("table_explorer_access_jars", "cache")
}

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
  # Strategy 1: RJDBC + UCanAccess (all platforms, needs Java)
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

  # Strategy 2: RODBC (Windows — needs Access Database Engine)
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

  # No strategy succeeded
  notify_fn(paste0(
    "Could not read Access file. ",
    "Install the RJDBC package (install.packages('RJDBC')) and ensure Java is available. ",
    "The required UCanAccess JARs will be downloaded automatically on first use. ",
    "On Windows, RODBC + the Microsoft Access Database Engine are also accepted."
  ))
  list()
}
