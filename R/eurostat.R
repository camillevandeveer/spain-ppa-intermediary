# Functions for downloading Eurostat data.
#
# Eurostat's API returns any dataset as a CSV file. A "key" picks the slice
# we want, with one position per dimension separated by dots; an empty
# position means "all values". For example, for the energy balance
# (dimensions: freq.nrg_bal.siec.unit.geo), "A..E7000.GWH.ES" means:
# annual, all balance items, electricity, in GWh, for Spain.

library(tidyverse)

eurostat_url <- function(dataset, key) {
  paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
    dataset, "/", key, "?format=SDMX-CSV"
  )
}

# Download a dataset slice to `dest_dir` (once; existing files are kept so the
# raw data never changes under us) and return the path.
download_eurostat <- function(dataset, key, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  safe_key <- str_replace_all(key, "[^A-Za-z0-9]+", "_")
  dest_file <- file.path(dest_dir, paste0(dataset, "_", safe_key, ".csv"))
  if (file.exists(dest_file)) return(dest_file)

  message("Downloading Eurostat ", dataset, " (", key, ")")
  tmp_file <- tempfile(fileext = ".csv")
  download.file(eurostat_url(dataset, key), tmp_file, quiet = TRUE, mode = "wb")
  if (!identical(substr(read_lines(tmp_file, n_max = 1), 1, 8), "DATAFLOW")) {
    unlink(tmp_file)
    stop("Eurostat did not return a CSV for ", dataset, " / ", key)
  }
  file.copy(tmp_file, dest_file)
  unlink(tmp_file)
  dest_file
}

# Read a downloaded Eurostat CSV into a tidy table.
read_eurostat <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    select(-any_of(c("DATAFLOW", "LAST UPDATE", "CONF_STATUS"))) |>
    rename(year = TIME_PERIOD, value = OBS_VALUE, flag = OBS_FLAG) |>
    mutate(year = as.integer(year))
}
