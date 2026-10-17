# Functions for downloading and reading OMIE day-ahead market prices.
#
# OMIE publishes one small text file per day ("marginalpdbc"). Each row is one
# delivery period: year; month; day; period; price Portugal; price Spain (EUR/MWh).
# Until 30 Sept 2025 there were 24 periods per day (hourly). Since 1 Oct 2025
# the market uses 15-minute periods, so a normal day has 96 rows.

library(tidyverse)

# Build the download address for one day's file.
omie_price_url <- function(date) {
  filename <- paste0("marginalpdbc_", format(date, "%Y%m%d"), ".1")
  paste0(
    "https://www.omie.es/es/file-download?parents%5B0%5D=marginalpdbc&filename=",
    filename
  )
}

# Download the daily files between two dates into `dest_dir`.
# Files that are already there are skipped, so re-running is cheap and the
# raw data is never overwritten.
download_omie_prices <- function(start_date, end_date, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  for (i in seq_along(dates)) {
    date <- dates[i]
    dest_file <- file.path(dest_dir, paste0("marginalpdbc_", format(date, "%Y%m%d"), ".1"))
    if (file.exists(dest_file)) next

    message("Downloading ", date)
    # Download to a temporary file first and only keep it if it looks like a
    # real OMIE price file. Otherwise a failed download would be saved and then
    # skipped on every later run.
    tmp_file <- tempfile()
    download.file(omie_price_url(date), tmp_file, quiet = TRUE, mode = "wb")
    if (!identical(read_lines(tmp_file, n_max = 1), "MARGINALPDBC;")) {
      unlink(tmp_file)
      stop("Download for ", date, " is not an OMIE price file. Is the file published yet?")
    }
    file.copy(tmp_file, dest_file)
    unlink(tmp_file)
    Sys.sleep(0.5) # be polite to OMIE's server
  }

  invisible(dest_dir)
}

# Read one daily file into a table.
read_omie_price_file <- function(path) {
  lines <- read_lines(path)
  # Keep only data rows (they start with a year); this drops the
  # "MARGINALPDBC;" header and the "*" end marker.
  data_lines <- lines[str_detect(lines, "^\\d{4};")]

  read_delim(
    I(data_lines),
    delim = ";",
    col_names = c("year", "month", "day", "period", "price_pt", "price_es", "empty"),
    col_types = "iiiiddc"
  ) |>
    select(-empty) |>
    mutate(date = make_date(year, month, day), .before = 1) |>
    select(date, period, price_es, price_pt)
}

# Read every daily file in a folder and check the result.
read_omie_prices <- function(dir) {
  files <- list.files(dir, pattern = "^marginalpdbc_\\d{8}\\.1$", full.names = TRUE)
  if (length(files) == 0) stop("No OMIE price files found in ", dir)

  prices <- map(files, read_omie_price_file) |> list_rbind()

  # Sanity checks: every day should have a valid number of periods.
  # Hourly days: 24 (23 or 25 on clock-change days).
  # 15-minute days: 96 (92 or 100 on clock-change days).
  periods_per_day <- count(prices, date)
  bad_days <- filter(periods_per_day, !n %in% c(23, 24, 25, 92, 96, 100))
  if (nrow(bad_days) > 0) {
    stop("Unexpected number of periods on: ", paste(bad_days$date, collapse = ", "))
  }

  expected_days <- seq(min(prices$date), max(prices$date), by = "day")
  missing_days <- setdiff(expected_days, unique(prices$date))
  if (length(missing_days) > 0) {
    stop("Missing days: ", paste(as.Date(missing_days), collapse = ", "))
  }

  if (any(is.na(prices$price_es))) stop("Some Spanish prices are missing (NA).")

  prices
}
