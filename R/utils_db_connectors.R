# ============================================================
# utils_db_connectors.R — Database Connection Functions
# ============================================================
#
# @noRd

# ── Supported database types ───────────────────────────────────

db_types <- c(
  "PostgreSQL"     = "postgres",
  "MySQL / MariaDB" = "mysql",
  "SQL Server"     = "sqlserver",
  "Snowflake"      = "snowflake",
  "BigQuery"       = "bigquery",
  "Redshift"       = "redshift",
  "SQLite"         = "sqlite"
)

# ── Connect to a database ─────────────────────────────────────

db_connect <- function(type, host = "", port = NULL, dbname = "",
                       user = "", password = "", schema = "",
                       driver = "", project = "", dataset = "",
                       path = "", notify_fn = message) {
  tryCatch(
    switch(type,
      postgres = {
        if (!requireNamespace("RPostgres", quietly = TRUE)) {
          stop("Install 'RPostgres': install.packages('RPostgres')")
        }
        DBI::dbConnect(
          RPostgres::Postgres(),
          host = host, port = port %||% 5432L,
          dbname = dbname, user = user, password = password
        )
      },
      mysql = {
        if (!requireNamespace("RMariaDB", quietly = TRUE)) {
          stop("Install 'RMariaDB': install.packages('RMariaDB')")
        }
        DBI::dbConnect(
          RMariaDB::MariaDB(),
          host = host, port = port %||% 3306L,
          dbname = dbname, user = user, password = password
        )
      },
      sqlserver = {
        if (!requireNamespace("odbc", quietly = TRUE)) {
          stop("Install 'odbc': install.packages('odbc')")
        }
        drv_name <- if (nzchar(driver)) driver else "ODBC Driver 17 for SQL Server"
        DBI::dbConnect(
          odbc::odbc(),
          Driver = drv_name,
          Server = host, Port = port %||% 1433L,
          Database = dbname, UID = user, PWD = password
        )
      },
      snowflake = {
        if (!requireNamespace("odbc", quietly = TRUE)) {
          stop("Install 'odbc': install.packages('odbc')")
        }
        drv_name <- if (nzchar(driver)) driver else "Snowflake"
        DBI::dbConnect(
          odbc::odbc(),
          Driver = drv_name,
          Server = host,
          Database = dbname, Schema = schema,
          UID = user, PWD = password
        )
      },
      bigquery = {
        if (!requireNamespace("bigrquery", quietly = TRUE)) {
          stop("Install 'bigrquery': install.packages('bigrquery')")
        }
        DBI::dbConnect(
          bigrquery::bigquery(),
          project = project,
          dataset = dataset
        )
      },
      redshift = {
        if (!requireNamespace("RPostgres", quietly = TRUE)) {
          stop("Install 'RPostgres': install.packages('RPostgres')")
        }
        DBI::dbConnect(
          RPostgres::Redshift(),
          host = host, port = port %||% 5439L,
          dbname = dbname, user = user, password = password
        )
      },
      sqlite = {
        DBI::dbConnect(RSQLite::SQLite(), dbname = path)
      },
      stop(paste0("Unsupported database type: ", type))
    ),
    error = function(e) {
      notify_fn(paste0("Connection failed: ", conditionMessage(e)))
      NULL
    }
  )
}

# ── Introspect database metadata ──────────────────────────────

db_introspect <- function(conn, type, schema = "public") {
  result <- list(tables = character(0), pks = list(), fks = list())

  # Get table list
  result$tables <- tryCatch(
    {
      if (type %in% c("postgres", "redshift", "mysql", "sqlserver", "snowflake")) {
        q <- switch(type,
          postgres = , redshift = paste0(
            "SELECT table_name FROM information_schema.tables ",
            "WHERE table_schema = '", schema, "' AND table_type = 'BASE TABLE'"
          ),
          mysql = paste0(
            "SELECT table_name FROM information_schema.tables ",
            "WHERE table_schema = DATABASE() AND table_type = 'BASE TABLE'"
          ),
          sqlserver = paste0(
            "SELECT table_name FROM information_schema.tables ",
            "WHERE table_schema = '", schema, "' AND table_type = 'BASE TABLE'"
          ),
          snowflake = paste0(
            "SELECT table_name FROM information_schema.tables ",
            "WHERE table_schema = '", toupper(schema), "' AND table_type = 'BASE TABLE'"
          )
        )
        res <- DBI::dbGetQuery(conn, q)
        res[[1]]
      } else {
        DBI::dbListTables(conn)
      }
    },
    error = function(e) {
      DBI::dbListTables(conn)
    }
  )

  # Get primary keys
  result$pks <- tryCatch(
    {
      if (type %in% c("postgres", "redshift", "mysql", "sqlserver")) {
        q <- switch(type,
          postgres = , redshift = paste0(
            "SELECT tc.table_name, kcu.column_name ",
            "FROM information_schema.table_constraints tc ",
            "JOIN information_schema.key_column_usage kcu ",
            "  ON tc.constraint_name = kcu.constraint_name ",
            "  AND tc.table_schema = kcu.table_schema ",
            "WHERE tc.constraint_type = 'PRIMARY KEY' ",
            "AND tc.table_schema = '", schema, "'"
          ),
          mysql = paste0(
            "SELECT tc.table_name, kcu.column_name ",
            "FROM information_schema.table_constraints tc ",
            "JOIN information_schema.key_column_usage kcu ",
            "  ON tc.constraint_name = kcu.constraint_name ",
            "  AND tc.table_schema = kcu.table_schema ",
            "WHERE tc.constraint_type = 'PRIMARY KEY' ",
            "AND tc.table_schema = DATABASE()"
          ),
          sqlserver = paste0(
            "SELECT tc.table_name, kcu.column_name ",
            "FROM information_schema.table_constraints tc ",
            "JOIN information_schema.key_column_usage kcu ",
            "  ON tc.constraint_name = kcu.constraint_name ",
            "  AND tc.table_schema = kcu.table_schema ",
            "WHERE tc.constraint_type = 'PRIMARY KEY' ",
            "AND tc.table_schema = '", schema, "'"
          )
        )
        pk_df <- DBI::dbGetQuery(conn, q)
        if (nrow(pk_df) > 0) {
          split(pk_df[[2]], pk_df[[1]])
        } else {
          list()
        }
      } else {
        list()
      }
    },
    error = function(e) list()
  )

  # Get foreign keys
  result$fks <- tryCatch(
    {
      if (type %in% c("postgres", "redshift", "mysql", "sqlserver")) {
        q <- switch(type,
          postgres = , redshift = paste0(
            "SELECT ",
            "  kcu.table_name AS from_table, ",
            "  kcu.column_name AS from_col, ",
            "  ccu.table_name AS to_table, ",
            "  ccu.column_name AS to_col ",
            "FROM information_schema.referential_constraints rc ",
            "JOIN information_schema.key_column_usage kcu ",
            "  ON rc.constraint_name = kcu.constraint_name ",
            "  AND rc.constraint_schema = kcu.constraint_schema ",
            "JOIN information_schema.constraint_column_usage ccu ",
            "  ON rc.unique_constraint_name = ccu.constraint_name ",
            "  AND rc.unique_constraint_schema = ccu.constraint_schema ",
            "WHERE rc.constraint_schema = '", schema, "'"
          ),
          mysql = paste0(
            "SELECT ",
            "  kcu.table_name AS from_table, ",
            "  kcu.column_name AS from_col, ",
            "  kcu.referenced_table_name AS to_table, ",
            "  kcu.referenced_column_name AS to_col ",
            "FROM information_schema.key_column_usage kcu ",
            "WHERE kcu.referenced_table_name IS NOT NULL ",
            "AND kcu.table_schema = DATABASE()"
          ),
          sqlserver = paste0(
            "SELECT ",
            "  fk_tab.name AS from_table, ",
            "  fk_col.name AS from_col, ",
            "  pk_tab.name AS to_table, ",
            "  pk_col.name AS to_col ",
            "FROM sys.foreign_key_columns fkc ",
            "JOIN sys.tables fk_tab ON fkc.parent_object_id = fk_tab.object_id ",
            "JOIN sys.columns fk_col ON fkc.parent_object_id = fk_col.object_id ",
            "  AND fkc.parent_column_id = fk_col.column_id ",
            "JOIN sys.tables pk_tab ON fkc.referenced_object_id = pk_tab.object_id ",
            "JOIN sys.columns pk_col ON fkc.referenced_object_id = pk_col.object_id ",
            "  AND fkc.referenced_column_id = pk_col.column_id"
          )
        )
        fk_df <- DBI::dbGetQuery(conn, q)
        if (nrow(fk_df) > 0) {
          lapply(seq_len(nrow(fk_df)), function(i) {
            list(
              from_table  = fk_df$from_table[i],
              from_col    = fk_df$from_col[i],
              to_table    = fk_df$to_table[i],
              to_col      = fk_df$to_col[i],
              detected_by = "schema",
              confidence  = "high",
              score       = 1.0,
              signals     = list(schema = 1.0),
              reasons     = "database constraint"
            )
          })
        } else {
          list()
        }
      } else {
        list()
      }
    },
    error = function(e) list()
  )

  result
}

# ── Load a single table from the database ─────────────────────

db_load_table <- function(conn, table_name, schema = "", limit = 10000) {
  q <- if (nzchar(schema)) {
    paste0("SELECT * FROM \"", schema, "\".\"", table_name, "\" LIMIT ", limit)
  } else {
    paste0("SELECT * FROM \"", table_name, "\" LIMIT ", limit)
  }
  tryCatch(
    DBI::dbGetQuery(conn, q),
    error = function(e) {
      # Fallback: try without schema quoting (for MySQL, SQLite, etc.)
      tryCatch(
        DBI::dbGetQuery(conn, paste0("SELECT * FROM `", table_name, "` LIMIT ", limit)),
        error = function(e2) NULL
      )
    }
  )
}

# ── Close a database connection ───────────────────────────────

db_close <- function(conn) {
  tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
}
