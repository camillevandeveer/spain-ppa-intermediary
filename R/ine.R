# Functions for downloading tables from INE (Spain's statistics institute).
#
# INE's API (servicios.ine.es/wstempus) returns a table as JSON: one entry per
# data series, each with metadata saying what the series is (e.g. sector
# "Industria", size class "De 0 a 9") and its values by year.

library(tidyverse)
library(jsonlite)

ine_table_url <- function(table_id, last_n_years) {
  paste0(
    "https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/", table_id,
    "?nult=", last_n_years, "&tip=AM"
  )
}

# Download an INE table to `dest_dir` (once; existing files are kept) and
# return the path. `last_n_years` limits the download to recent years.
download_ine_table <- function(table_id, dest_dir, last_n_years = 1) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(dest_dir, paste0("ine_table_", table_id, ".json"))
  if (file.exists(dest_file)) return(dest_file)

  message("Downloading INE table ", table_id)
  tmp_file <- tempfile(fileext = ".json")
  download.file(ine_table_url(table_id, last_n_years), tmp_file, quiet = TRUE, mode = "wb")
  if (!identical(substr(read_file(tmp_file), 1, 1), "[")) {
    unlink(tmp_file)
    stop("INE did not return a data table for table ", table_id)
  }
  file.copy(tmp_file, dest_file)
  unlink(tmp_file)
  dest_file
}

# Read a downloaded INE table into a tidy table: one column per metadata
# variable (named as INE names it), plus year and value.
read_ine_table <- function(path) {
  series <- fromJSON(path, simplifyVector = FALSE)

  map(series, \(s) {
    meta <- set_names(
      map_chr(s$MetaData, \(m) m$Nombre),
      map_chr(s$MetaData, \(m) str_trim(m$T3_Variable))
    )
    values <- tibble(
      year  = map_int(s$Data, \(d) as.integer(d$Anyo)),
      value = map_dbl(s$Data, \(d) if (is.null(d$Valor)) NA_real_ else d$Valor)
    )
    bind_cols(as_tibble(as.list(meta)), values, .name_repair = "minimal")
  }) |>
    list_rbind()
}
