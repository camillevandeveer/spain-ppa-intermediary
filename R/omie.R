# Functions for downloading and reading OMIE day-ahead market prices.
#
# OMIE publishes one small text file per day ("marginalpdbc"). Each row is one
# delivery period: year; month; day; period; price Portugal; price Spain (EUR/MWh).
# Until 30 Sept 2025 there were 24 periods per day (hourly). Since 1 Oct 2025
# the market uses 15-minute periods, so a normal day has 96 rows.
#
# Older years (2022 and before) are no longer offered as daily files, only as
# one .zip archive per year. download_omie_prices() handles both.

library(tidyverse)

# Build the download address for one day's file.
# OMIE sometimes republishes a day with a correction; the file then ends in
# .2 (or .3) instead of .1.
omie_price_url <- function(date, version = 1) {
  filename <- paste0("marginalpdbc_", format(date, "%Y%m%d"), ".", version)
  paste0(
    "https://www.omie.es/es/file-download?parents%5B0%5D=marginalpdbc&filename=",
    filename
  )
}

# Find the file for one date in `dir`. If there are several versions
# (.1, .2, ...), the highest one is the most recent, so that one is returned.
# Returns NA if there is no file for that date.
omie_day_file <- function(dir, date) {
  pattern <- paste0("^marginalpdbc_", format(date, "%Y%m%d"), "\\.\\d+$")
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  versions <- as.integer(str_extract(files, "\\d+$"))
  files[which.max(versions)]
}

# Build the download address for one year's .zip archive.
omie_year_archive_url <- function(year) {
  paste0(
    "https://www.omie.es/es/file-download?parents%5B0%5D=marginalpdbc&filename=",
    "marginalpdbc_", year, ".zip"
  )
}

# Try to get a whole year from OMIE's yearly archive and unpack the daily files
# into `dest_dir` (files already there are left untouched).
# Returns TRUE if an archive existed, FALSE if OMIE has none for that year.
download_omie_year_archive <- function(year, dest_dir) {
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip))

  downloaded <- tryCatch(
    {
      download.file(omie_year_archive_url(year), tmp_zip, quiet = TRUE, mode = "wb")
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!downloaded) return(FALSE)

  # OMIE answers with a web page (not a zip) when there is no archive
  contents <- tryCatch(unzip(tmp_zip, list = TRUE)$Name, error = function(e) NULL, warning = function(w) NULL)
  if (is.null(contents)) return(FALSE)

  day_files <- contents[str_detect(contents, "^marginalpdbc_\\d{8}\\.\\d+$")]
  new_files <- day_files[!file.exists(file.path(dest_dir, day_files))]
  if (length(new_files) > 0) unzip(tmp_zip, files = new_files, exdir = dest_dir)
  message("Unpacked ", length(new_files), " daily files from the ", year, " archive")
  TRUE
}

# Download the daily files between two dates into `dest_dir`.
# Files that are already there are skipped, so re-running is cheap and the
# raw data is never overwritten.
download_omie_prices <- function(start_date, end_date, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  have_file <- function(d) !is.na(omie_day_file(dest_dir, d))

  # First try the yearly archive for every year that still has missing days
  missing <- dates[!map_lgl(dates, have_file)]
  for (y in unique(year(missing))) download_omie_year_archive(y, dest_dir)

  # Then download whatever is still missing day by day
  for (i in seq_along(dates)) {
    date <- dates[i]
    if (have_file(date)) next

    # Print progress once per month instead of once per day
    if (i == 1 || format(date, "%d") == "01") message("Downloading ", format(date, "%B %Y"), " ...")

    # Try version .1 first, then the corrected versions .2 and .3.
    # Each download goes to a temporary file and is only kept if it looks like
    # a real OMIE price file, so a failed download is never saved.
    saved <- FALSE
    for (version in 1:3) {
      tmp_file <- tempfile()
      ok <- tryCatch(
        {
          download.file(omie_price_url(date, version), tmp_file, quiet = TRUE, mode = "wb")
          identical(read_lines(tmp_file, n_max = 1), "MARGINALPDBC;")
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
      )
      if (ok) {
        file.copy(tmp_file, file.path(dest_dir, paste0("marginalpdbc_", format(date, "%Y%m%d"), ".", version)))
      }
      unlink(tmp_file)
      Sys.sleep(0.25) # be polite to OMIE's server
      if (ok) {
        saved <- TRUE
        break
      }
    }
    if (!saved) stop("No OMIE price file found for ", date, ". Is it published yet?")
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

# Read the daily files between two dates and check the result.
read_omie_prices <- function(dir, start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  files <- map_chr(dates, \(d) omie_day_file(dir, d))
  if (anyNA(files)) {
    stop(sum(is.na(files)), " days have no file, e.g. ", dates[is.na(files)][1],
         ". Run download_omie_prices() for these dates first.")
  }

  prices <- map(files, read_omie_price_file) |> list_rbind()

  # Sanity checks: every day should have a valid number of periods.
  # Hourly days: 24 (23 or 25 on clock-change days).
  # 15-minute days: 96 (92 or 100 on clock-change days).
  periods_per_day <- count(prices, date)
  bad_days <- filter(periods_per_day, !n %in% c(23, 24, 25, 92, 96, 100))
  if (nrow(bad_days) > 0) {
    stop("Unexpected number of periods on: ", paste(bad_days$date, collapse = ", "))
  }

  missing_days <- setdiff(dates, unique(prices$date))
  if (length(missing_days) > 0) {
    stop("Missing days: ", paste(as.Date(missing_days), collapse = ", "))
  }

  if (any(is.na(prices$price_es))) stop("Some Spanish prices are missing (NA).")

  prices
}

# Convert prices to one value per hour.
# Days with 15-minute periods (from 1 Oct 2025) are averaged into hours:
# periods 1-4 become hour 1, periods 5-8 hour 2, and so on.
# Hourly days are left as they are. On the two clock-change days per year the
# "hour" is the period number (23 or 25 hours), not the clock hour.
omie_to_hourly <- function(prices) {
  prices |>
    group_by(date) |>
    mutate(minutes_per_period = if_else(n() > 25, 15L, 60L)) |>
    ungroup() |>
    mutate(hour = if_else(minutes_per_period == 15L, (period - 1L) %/% 4L + 1L, period)) |>
    group_by(date, hour) |>
    summarise(
      price_es = mean(price_es),
      price_pt = mean(price_pt),
      .groups = "drop"
    )
}
