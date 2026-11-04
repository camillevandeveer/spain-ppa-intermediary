# Functions for building hourly solar/wind supply profiles and simulating a
# supply portfolio (solar + wind + battery) against a factory's demand.
# Used by analysis/06_shape_mismatch.R and analysis/07_portfolio_coverage.R.

library(tidyverse)
library(jsonlite)

# --- Installed capacity --------------------------------------------------------

# Download Energy-Charts' yearly installed capacity for Spain (once) and read it.
# Energy-Charts' value for year t is the capacity at the START of year t.
read_energy_charts_capacity <- function(dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(dest_dir, "installed_power_es_yearly.json")
  if (!file.exists(dest_file)) {
    message("Downloading Energy-Charts installed capacity")
    download.file("https://api.energy-charts.info/installed_power?country=es&time_step=yearly",
                  dest_file, quiet = TRUE, mode = "wb")
  }
  d <- fromJSON(dest_file, simplifyVector = FALSE)
  years <- as.integer(unlist(d$time))
  map(d$production_types, \(p) {
    tibble(technology = p$name, year = years,
           gw = map_dbl(p$data, \(x) if (is.null(x)) NA_real_ else x))
  }) |>
    list_rbind()
}

# Installed capacity (GW) at each hour, interpolated in a straight line between
# the start of one year and the start of the next. If the next year's value is
# not published yet, assume the fleet grew by the same amount as the year before.
capacity_at <- function(hour_utc, capacity_one_tech) {
  local_year <- year(with_tz(hour_utc, "Europe/Madrid"))
  cap <- set_names(capacity_one_tech$gw, capacity_one_tech$year)
  get_cap <- function(y) if (as.character(y) %in% names(cap)) cap[[as.character(y)]] else NA_real_

  start_cap <- map_dbl(local_year, get_cap)
  next_cap  <- map_dbl(local_year + 1, get_cap)
  prev_cap  <- map_dbl(local_year - 1, get_cap)
  next_cap  <- if_else(is.na(next_cap), start_cap + (start_cap - prev_cap), next_cap)

  year_start <- as.POSIXct(paste0(local_year, "-01-01"), tz = "Europe/Madrid")
  year_end   <- as.POSIXct(paste0(local_year + 1, "-01-01"), tz = "Europe/Madrid")
  fraction   <- as.numeric(difftime(hour_utc, year_start, units = "hours")) /
                as.numeric(difftime(year_end, year_start, units = "hours"))

  start_cap + fraction * (next_cap - start_cap)
}

# Add an hourly capacity factor (output / installed capacity) to hourly generation.
# `generation` has columns technology ("Solar", "Wind onshore"), hour_utc, mw.
add_capacity_factor <- function(generation, capacity) {
  # Generation names on the left, matching capacity names on the right
  technologies <- c("Solar" = "Solar AC", "Wind onshore" = "Wind onshore")
  imap(technologies, \(capacity_name, generation_name) {
    g <- filter(generation, technology == generation_name)
    g$capacity_gw <- capacity_at(g$hour_utc, filter(capacity, technology == capacity_name))
    g
  }) |>
    list_rbind() |>
    mutate(capacity_factor = mw / (capacity_gw * 1000))
}

# --- Battery ---------------------------------------------------------------------

# Simulate a battery hour by hour. `net` = supply minus demand in each hour (MW).
# The battery charges from surplus and discharges into shortfall, with no
# forecasting. Power and energy limits in MW and MWh; losses split equally
# between charging and discharging.
# Returns, per hour: energy charged into the battery (from surplus) and energy
# delivered from the battery (into shortfall).
simulate_battery <- function(net, power_mw, energy_mwh, round_trip_efficiency) {
  n <- length(net)
  charged   <- numeric(n)
  delivered <- numeric(n)
  if (energy_mwh <= 0) return(list(charged = charged, delivered = delivered))

  eff <- sqrt(round_trip_efficiency)
  stored <- 0
  for (h in seq_len(n)) {
    if (net[h] > 0) {
      # Surplus: store as much as power and free space allow
      take <- min(net[h], power_mw, (energy_mwh - stored) / eff)
      charged[h] <- take
      stored <- stored + take * eff
    } else if (net[h] < 0) {
      # Shortfall: deliver as much as power and stored energy allow
      give <- min(-net[h], power_mw, stored * eff)
      delivered[h] <- give
      stored <- stored - give / eff
    }
  }
  list(charged = charged, delivered = delivered)
}

# --- One portfolio ----------------------------------------------------------------

# Simulate one portfolio for one year against flat demand of 1 MW.
# cf_solar, cf_wind: hourly capacity factors; price: hourly price (EUR/MWh).
# solar_share: share of yearly output from solar; oversizing: yearly output /
# yearly demand; battery_hours: storable hours of demand.
simulate_portfolio <- function(cf_solar, cf_wind, price, solar_share, oversizing,
                               battery_hours, round_trip_efficiency) {
  n <- length(price)
  demand <- rep(1, n)
  target <- oversizing * sum(demand)

  # Scale each technology so its yearly output is its share of the target
  solar  <- if (solar_share > 0) cf_solar / sum(cf_solar) * target * solar_share else 0
  wind   <- if (solar_share < 1) cf_wind / sum(cf_wind) * target * (1 - solar_share) else 0
  supply <- solar + wind

  battery <- simulate_battery(supply - demand, power_mw = 1, energy_mwh = battery_hours,
                              round_trip_efficiency = round_trip_efficiency)

  direct    <- pmin(supply, demand)
  used      <- direct + battery$delivered
  sold      <- pmax(supply - demand, 0) - battery$charged
  bought    <- demand - used

  tibble(
    hourly_match         = sum(used) / sum(demand),
    surplus_sold_share   = sum(sold) / sum(supply),
    battery_share        = sum(battery$delivered) / sum(demand),
    # Net cost per MWh of demand of buying the shortfall and selling the surplus
    # at the day-ahead price (negative = the surplus earns more than the shortfall costs)
    net_market_cost      = (sum(price * bought) - sum(price * sold)) / sum(demand),
    shortfall_avg_price  = if (sum(bought) > 0) sum(price * bought) / sum(bought) else NA_real_
  )
}

# --- Shared input data -----------------------------------------------------------

# One row per hour: year, hourly capacity factors for solar and wind, and the
# OMIE day-ahead price. Uses the raw files downloaded by script 06 and the
# hourly prices saved by script 03.
load_hourly_inputs <- function(years, raw_dir, prices_file) {
  files <- download_energy_charts(paste0(min(years) - 1, "-12-01"), paste0(max(years), "-12-31"), raw_dir)

  generation <- read_energy_charts_hourly(files) |>
    mutate(year = year(with_tz(hour_utc, "Europe/Madrid"))) |>
    filter(year %in% years) |>
    arrange(technology, hour_utc) |>
    group_by(technology) |>
    mutate(mw = if_else(is.na(mw), (lag(mw) + lead(mw)) / 2, mw)) |>
    ungroup() |>
    add_capacity_factor(read_energy_charts_capacity(raw_dir))

  prices <- read_csv(prices_file, show_col_types = FALSE) |>
    mutate(hour_utc = with_tz(force_tz(as.POSIXct(date), "Europe/Madrid") + (hour - 1) * 3600, "UTC")) |>
    select(hour_utc, price = price_es)

  generation |>
    select(hour_utc, year, technology, capacity_factor) |>
    pivot_wider(names_from = technology, values_from = capacity_factor) |>
    rename(cf_solar = Solar, cf_wind = `Wind onshore`) |>
    inner_join(prices, by = "hour_utc") |>
    arrange(hour_utc)
}
