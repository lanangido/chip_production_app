# helpers/utils.R

# Fungsi helper untuk membaca dan mengonversi data dari DB
read_and_convert_table <- function(conn, table_name, date_cols = c()) {
  df <- DBI::dbReadTable(conn, table_name)
  for (col in date_cols) {
    if (col %in% colnames(df)) {
      df[[col]] <- as.Date(df[[col]])
    }
  }
  return(df)
}
