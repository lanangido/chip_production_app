# config/db_config.R


DB_HOST <- Sys.getenv("PG_HOST", "localhost")
DB_PORT <- as.numeric(Sys.getenv("PG_PORT", 5432))
DB_NAME <- Sys.getenv("PG_DBNAME", "chip_production_db")
DB_USER <- Sys.getenv("PG_USER", "postgres")
DB_PASSWORD <- Sys.getenv("PG_PASSWORD", "12345678")

# Fungsi untuk membuat koneksi database
get_db_conn <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = DB_HOST,
    port = DB_PORT,
    dbname = DB_NAME,
    user = DB_USER,
    password = DB_PASSWORD
  )
}

