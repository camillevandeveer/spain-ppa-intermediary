# 07: How much of a 24/7 factory's demand can a combined portfolio cover?
#     (Sub-question 2, Module 3 Part A: coverage)
#
# Script 06 showed that a single solar PPA covers 46% of a flat factory's demand
# hour by hour, and a single wind PPA about 76%. Here the intermediary combines:
# - solar and wind in different proportions (solar_shares),
# - more output than the factory uses in a year (oversizing), surplus resold,
# - a battery that stores surplus and releases it into gaps (battery_hours).
#
# For every combination and year we measure the hourly match, the surplus that
# must be resold, and the net market cost of the leftover shortfall and surplus.
# Costs of building the plants and batteries come later (Part B).
#
# All settings and sources: config/assumptions.yaml, section portfolio_coverage.
# Requires scripts 03 (prices) and 06 (generation downloads) to have run.

library(tidyverse)
library(here)
library(yaml)

source(here("R", "energy_charts.R"))
source(here("R", "portfolio.R"))

settings <- read_yaml(here("config", "assumptions.yaml"))$portfolio_coverage
years <- unlist(settings$years$value)

# 1. Hourly inputs ---------------------------------------------------------------
inputs <- load_hourly_inputs(
  years,
  raw_dir = here("data", "raw", "energy_charts"),
  prices_file = here("data", "processed", "omie_prices_hourly_2022_2025.csv")
)

# Checks: complete years, no missing values
inputs |> count(year)
stopifnot(all(count(inputs, year)$n >= 8759), !anyNA(inputs))

# 2. Simulate every portfolio in every year ----------------------------------------
grid <- expand_grid(
  year          = years,
  solar_share   = unlist(settings$solar_shares$value),
  oversizing    = unlist(settings$oversizing$value),
  battery_hours = unlist(settings$battery_hours$value),
  round_trip_efficiency = c(settings$battery_round_trip_efficiency$value,
                            settings$battery_round_trip_efficiency$sensitivity)
) |>
  # The efficiency sensitivity only matters when there is a battery
  filter(battery_hours > 0 | round_trip_efficiency == settings$battery_round_trip_efficiency$value)

message("Simulating ", nrow(grid), " portfolio-years (about a minute)")

results <- grid |>
  mutate(sim = pmap(list(year, solar_share, oversizing, battery_hours, round_trip_efficiency),
                    \(y, s, o, b, e) {
                      d <- filter(inputs, year == y)
                      simulate_portfolio(d$cf_solar, d$cf_wind, d$price, s, o, b, e)
                    })) |>
  unnest(sim)

# Check: single-technology portfolios with no battery and no oversizing should
# reproduce script 06's figures (solar about 46%, wind about 76%)
results |>
  filter(oversizing == 1, battery_hours == 0, solar_share %in% c(0, 1)) |>
  select(year, solar_share, hourly_match)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(results, here("outputs", "tables", "07_portfolio_coverage_all.csv"))

# 3. Best mix for each oversizing and battery size -----------------------------------
main <- results |>
  filter(round_trip_efficiency == settings$battery_round_trip_efficiency$value)

best <- main |>
  group_by(year, oversizing, battery_hours) |>
  slice_max(hourly_match, n = 1, with_ties = FALSE) |>
  ungroup()

best |>
  filter(year == max(years)) |>
  select(oversizing, battery_hours, solar_share, hourly_match, surplus_sold_share, net_market_cost) |>
  mutate(across(c(hourly_match, surplus_sold_share), \(x) round(x, 3)), net_market_cost = round(net_market_cost, 1)) |>
  print(n = Inf)

write_csv(best, here("outputs", "tables", "07_portfolio_coverage_best_mix.csv"))

# Smallest portfolio (least oversizing, then smallest battery) reaching each target
targets <- unlist(settings$coverage_targets$value)
reaching <- map(targets, \(t) {
  best |>
    filter(hourly_match >= t) |>
    group_by(year) |>
    arrange(oversizing, battery_hours, .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(target = t, .before = 1)
}) |>
  list_rbind()
reaching |> select(target, year, oversizing, battery_hours, solar_share, hourly_match)
write_csv(reaching, here("outputs", "tables", "07_smallest_portfolio_per_target.csv"))

# Efficiency sensitivity: how much does 85% instead of 82% change coverage?
sensitivity <- results |>
  filter(battery_hours > 0) |>
  select(year, solar_share, oversizing, battery_hours, round_trip_efficiency, hourly_match) |>
  pivot_wider(names_from = round_trip_efficiency, values_from = hourly_match, names_prefix = "eff_")
sensitivity |>
  summarise(max_difference_pct_points = max(abs(.data[[names(sensitivity)[6]]] - .data[[names(sensitivity)[5]]])) * 100)

# 4. Figure A: coverage rises with oversizing and storage (latest year) ------------
latest <- max(years)
single <- main |>
  filter(year == latest, oversizing == 1, battery_hours == 0, solar_share %in% c(0, 1)) |>
  mutate(label = if_else(solar_share == 1, "Solar PPA alone", "Wind PPA alone"))

plot_a <- best |>
  filter(year == latest) |>
  mutate(battery = factor(paste0(battery_hours, " h"), levels = paste0(sort(unique(battery_hours)), " h")))

battery_colours <- set_names(c("#b8b8b8", "#9cc3ee", "#2a78d6", "#174a85"), levels(plot_a$battery))
headline <- filter(plot_a, oversizing == 1.5, battery_hours == 4)

fig_a <- ggplot(plot_a, aes(x = oversizing, y = hourly_match, colour = battery)) +
  geom_hline(data = single, aes(yintercept = hourly_match), linetype = "dashed", colour = "grey55") +
  geom_text(data = single, aes(x = 3, y = hourly_match, label = paste0(label, ": ", scales::percent(hourly_match, accuracy = 1))),
            inherit.aes = FALSE, hjust = 1, vjust = -0.5, size = 3.2, colour = "grey35") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = battery_colours, name = "Battery storage (hours of factory demand)") +
  scale_x_continuous(breaks = unlist(settings$oversizing$value), labels = \(x) paste0(x, "×")) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = paste0("With 1.5× output and 4 hours of storage, a solar\u2013wind mix covers ",
                   scales::percent(headline$hourly_match, accuracy = 1), " of a 24/7 factory's demand (", latest, ")"),
    subtitle = "Share of demand met hour by hour, using the best solar/wind mix for each portfolio size",
    x = "Portfolio's yearly output relative to the factory's yearly demand",
    y = "Demand met in the same hour",
    caption = paste0("Battery round-trip efficiency ", settings$battery_round_trip_efficiency$value * 100,
                     "% (EIA). National solar and wind output, adjusted for capacity growth. Sources: Energy-Charts / ENTSO-E; OMIE; our calculation (script 07).")
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey35"))

fig_a
ggsave(here("outputs", "figures", "07a_coverage_by_portfolio.png"), fig_a,
       width = 9, height = 5.5, dpi = 300, bg = "white")

# 5. Figure B: which mix works best (latest year, 4 hours of storage) -------------
plot_b <- main |>
  filter(year == latest, battery_hours == 4) |>
  mutate(solar_label = paste0(solar_share * 100, "% solar"),
         solar_label = factor(solar_label, levels = paste0(unlist(settings$solar_shares$value) * 100, "% solar")),
         size_label = factor(paste0(oversizing, "×"), levels = paste0(unlist(settings$oversizing$value), "×")))

fig_b <- ggplot(plot_b, aes(x = solar_label, y = size_label, fill = hourly_match)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = scales::percent(hourly_match, accuracy = 1),
                colour = hourly_match > 0.75), size = 3.6) +
  scale_fill_gradient(low = "#e8f0fb", high = "#174a85", labels = scales::label_percent(), name = "Demand met") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"), guide = "none") +
  labs(
    title = paste0("Mostly wind with some solar covers the most demand (", latest, ", 4 hours of storage)"),
    subtitle = "Share of a 24/7 factory's demand met hour by hour, by solar share of output and portfolio size",
    x = "Solar share of the portfolio's yearly output (the rest is wind)",
    y = "Yearly output vs. demand",
    caption = "Sources: Energy-Charts / ENTSO-E; our calculation (script 07)."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey35"))

fig_b
ggsave(here("outputs", "figures", "07b_coverage_by_mix.png"), fig_b,
       width = 9, height = 5, dpi = 300, bg = "white")
