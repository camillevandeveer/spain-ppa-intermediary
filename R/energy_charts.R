# Functions for downloading Spanish electricity generation by technology from
# Energy-Charts (Fraunhofer ISE), which republishes ENTSO-E Transparency data.
# Values are average power in MW for each 15-minute (or hourly) interval.

library(tidyverse)
library(jsonlite)

energy_charts_url <- function(start_date, end_date) {
  paste0(
    "https://api.energy-charts.info/public_power?country=es",
    "&start=", format(as.Date(start_date)), "T00:00Z",
    "&end=", format(as.Date(end_date) + 1), "T00:00Z"
  )
}

# Download one file per month between two dates into `dest_dir` (once;
# existing files are kept) and return the file paths.
download_energy_charts <- function(start_date, end_date, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  months <- seq(floor_date(as.Date(start_date), "month"), floor_date(as.Date(end_date), "month"), by = "month")

  map_chr(months, \(m) {
    dest_file <- file.path(dest_dir, paste0("public_power_es_", format(m, "%Y-%m"), ".json"))
    if (file.exists(dest_file)) return(dest_file)

    message("Downloading Energy-Charts ", format(m, "%B %Y"))
    tmp_file <- tempfile(fileext = ".json")
    url <- energy_charts_url(m, ceiling_date(m, "month") - 1)

    # The server limits how often it can be asked ("429 Too Many Requests"),
    # so wait and try again, waiting longer each time.
    waits <- c(0, 30, 60, 120, 240)
    for (wait in waits) {
      if (wait > 0) {
        message("  server busy, waiting ", wait, " seconds")
        Sys.sleep(wait)
      }
      ok <- tryCatch({
        download.file(url, tmp_file, quiet = TRUE, mode = "wb")
        str_detect(read_file(tmp_file), "\"production_types\"")
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) break
    }
    if (!ok) {
      unlink(tmp_file)
      stop("Energy-Charts did not return generation data for ", format(m, "%Y-%m"), ". Try again later.")
    }
    file.copy(tmp_file, dest_file)
    unlink(tmp_file)
    Sys.sleep(5) # be polite to the server
    dest_file
  })
}

# Read downloaded files and return hourly average MW for the chosen technologies,
# with timestamps in UTC (start of each hour).
read_energy_charts_hourly <- function(paths, technologies = c("Solar", "Wind onshore")) {
  map(paths, \(path) {
    d <- fromJSON(path, simplifyVector = FALSE)
    ts <- as.POSIXct(unlist(d$unix_seconds), origin = "1970-01-01", tz = "UTC")
    map(d$production_types, \(p) {
      if (!p$name %in% technologies) return(NULL)
      tibble(
        time_utc   = ts,
        technology = p$name,
        mw         = map_dbl(p$data, \(x) if (is.null(x)) NA_real_ else x)
      )
    }) |>
      list_rbind()
  }) |>
    list_rbind() |>
    distinct(time_utc, technology, .keep_all = TRUE) |> # months can overlap at the edges
    mutate(hour_utc = floor_date(time_utc, "hour")) |>
    group_by(technology, hour_utc) |>
    summarise(n_values = sum(!is.na(mw)), mw = mean(mw, na.rm = TRUE), .groups = "drop") |>
    mutate(mw = if_else(n_values == 0, NA_real_, mw))
}
