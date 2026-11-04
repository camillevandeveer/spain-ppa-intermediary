# 06: Can a factory with round-the-clock demand run on a single solar or wind PPA?
#     (Sub-question 1, the shape barrier)
#
# Set-up (details and sources in config/assumptions.yaml, section shape_mismatch):
# - A factory uses the same amount of electricity every hour of the year.
# - It signs a solar-only or a wind-only PPA sized to produce, over the year,
#   exactly what the factory uses (the usual "100% renewable" claim).
# - The PPA follows the hourly shape of Spain's national solar or wind output.
# - Hour by hour, the factory uses PPA power when there is enough, sells any
#   surplus at the day-ahead price, and buys any shortfall at the day-ahead price.
#
# What we measure, for each year 2022-2025:
# - hourly match: share of the factory's demand met by the PPA in the same hour
# - surplus: share of the PPA's output that arrives when the factory doesn't need it
# - shape cost: extra cost per MWh of demand of buying the shortfall and selling
#   the surplus at market prices (= average price minus the PPA's capture price)

library(tidyverse)
library(here)
library(yaml)

source(here("R", "energy_charts.R"))

assumptions <- read_yaml(here("config", "assumptions.yaml"))$shape_mismatch
years <- unlist(assumptions$years$value)

# 1. Hourly solar and wind output in Spain --------------------------------------
# The data are in UTC; Spain's first hour of the year (00:00) is still 23:00 UTC
# on 31 December, so the December before the first year is downloaded too.
files <- download_energy_charts(paste0(min(years) - 1, "-12-01"), paste0(max(years), "-12-31"),
                                here("data", "raw", "energy_charts"))

generation <- read_energy_charts_hourly(files) |>
  mutate(year = year(with_tz(hour_utc, "Europe/Madrid"))) |>
  filter(year %in% years)

# Checks: every hour of every year present; very few missing values
gen_check <- generation |>
  group_by(technology, year) |>
  summarise(hours = n(), missing = sum(is.na(mw)), .groups = "drop") |>
  mutate(expected = if_else(leap_year(year), 8784L, 8760L))
gen_check
stopifnot(all(gen_check$hours == gen_check$expected))
stopifnot(all(gen_check$missing / gen_check$expected < 0.01))

# Fill the rare missing hours with the average of the hours either side
generation <- generation |>
  arrange(technology, hour_utc) |>
  group_by(technology) |>
  mutate(mw = if_else(is.na(mw), (lag(mw) + lead(mw)) / 2, mw)) |>
  ungroup()
stopifnot(!anyNA(generation$mw))

# 2. Hourly day-ahead prices (from script 03), on the same UTC clock -------------
# OMIE's hour 1 is 00:00-01:00 Spanish time; counting hours from local midnight
# keeps clock-change days (23 or 25 hours) correct.
prices <- read_csv(here("data", "processed", "omie_prices_hourly_2022_2025.csv"), show_col_types = FALSE) |>
  mutate(hour_utc = with_tz(force_tz(as.POSIXct(date), "Europe/Madrid") + (hour - 1) * 3600, "UTC")) |>
  select(hour_utc, price = price_es)

hourly <- generation |>
  inner_join(prices, by = "hour_utc")

# Check: the join kept (almost) every hour
stopifnot(nrow(hourly) >= 0.999 * nrow(generation))

# 3. The factory and its PPA -------------------------------------------------------
# Factory demand = 1 MW every hour. The PPA is scaled so its yearly output equals
# the factory's yearly demand (number of hours).
hourly <- hourly |>
  group_by(technology, year) |>
  mutate(
    demand = 1,
    ppa    = mw / sum(mw) * n(),
    used   = pmin(ppa, demand),
    surplus   = pmax(ppa - demand, 0),
    shortfall = pmax(demand - ppa, 0)
  ) |>
  ungroup() |>
  mutate(technology = recode(technology, "Solar" = "Solar PPA", "Wind onshore" = "Wind PPA"))

results <- hourly |>
  group_by(technology, year) |>
  summarise(
    hourly_match      = sum(used) / sum(demand),
    surplus_share     = sum(surplus) / sum(ppa),
    hours_almost_none = mean(ppa < 0.1 * demand), # hours where the PPA covers under 10% of demand
    average_price     = mean(price),
    capture_price     = sum(price * ppa) / sum(ppa),
    shape_cost        = average_price - capture_price,
    .groups = "drop"
  )

results |> mutate(across(where(is.double), \(x) round(x, 3))) |> print(n = Inf)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(results, here("outputs", "tables", "06_shape_mismatch.csv"))

# Colours: wind blue, solar orange (same in both figures); demand in grey
tech_colours <- c("Wind PPA" = "#2a78d6", "Solar PPA" = "#eb6834")

# 4. Figure A: an average day in the latest year ------------------------------------
latest <- max(years)
average_day <- hourly |>
  filter(year == latest) |>
  mutate(local_hour = hour(with_tz(hour_utc, "Europe/Madrid"))) |>
  group_by(technology, local_hour) |>
  summarise(ppa = mean(ppa), .groups = "drop")

solar_latest <- filter(results, technology == "Solar PPA", year == latest)
day_labels <- average_day |>
  group_by(technology) |>
  slice_max(ppa, n = 1)

fig_a <- ggplot(average_day, aes(x = local_hour, y = ppa, colour = technology)) +
  geom_hline(yintercept = 1, colour = "grey45", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = 1, y = 1, label = "Factory demand (flat)", hjust = 0, vjust = 1.8, size = 3.4, colour = "grey30") +
  geom_line(linewidth = 1) +
  geom_text(data = day_labels, aes(label = technology), vjust = -0.8, size = 3.4, show.legend = FALSE) +
  scale_colour_manual(values = tech_colours, guide = "none") +
  scale_x_continuous(breaks = seq(0, 23, by = 3), labels = \(h) sprintf("%02d:00", h)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("A solar PPA covers only ", scales::percent(solar_latest$hourly_match, accuracy = 1),
                   " of a 24/7 factory's demand hour by hour (", latest, ")"),
    subtitle = paste0("Average day in ", latest, ": output of a PPA sized to the factory's yearly demand, ",
                      "relative to the factory's hourly demand (= 1)"),
    x = "Hour of the day (Spanish time)",
    y = "Power relative to factory demand",
    caption = "Sources: Energy-Charts / ENTSO-E (national solar and wind output); our calculation (script 06)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey35"))

fig_a
ggsave(here("outputs", "figures", "06a_average_day_ppa_vs_demand.png"), fig_a,
       width = 9, height = 5, dpi = 300, bg = "white")

# 5. Figure B: the shape cost, by year ---------------------------------------------
fig_b <- ggplot(results, aes(x = factor(year), y = shape_cost, fill = technology)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = round(shape_cost), vjust = if_else(shape_cost >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.75), size = 3.3, colour = "grey20") +
  scale_fill_manual(values = tech_colours, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  labs(
    title = paste0("Filling a solar PPA's gaps cost a 24/7 factory about €", round(solar_latest$shape_cost),
                   "/MWh extra in ", latest),
    subtitle = "Extra cost per MWh of demand of selling the PPA's surplus and buying its shortfall at day-ahead prices",
    x = NULL,
    y = "€ per MWh of demand",
    caption = "Shape cost = average day-ahead price minus the PPA's capture price. Sources: OMIE; Energy-Charts / ENTSO-E; our calculation (script 06)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "top",
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey35"))

fig_b
ggsave(here("outputs", "figures", "06b_shape_cost_by_year.png"), fig_b,
       width = 9, height = 5, dpi = 300, bg = "white")
