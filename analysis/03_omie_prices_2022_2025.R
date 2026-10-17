# 03: Spanish day-ahead prices, 2022–2025
#
# Question: how has the daily price shape changed as more solar came online?
# Script 01 showed one month (April 2024). This script looks at four full years.
#
# Things to keep in mind when reading the results:
# - 2022 prices were very high because of the gas crisis. From mid-June 2022
#   to the end of 2023 the "Iberian exception" capped the gas price used for
#   power; OMIE's marginal price does not include the cost of that mechanism
#   that consumers paid separately.
# - From 1 Oct 2025 OMIE publishes 15-minute prices. They are averaged into
#   hours here so all years are comparable (see omie_to_hourly() in R/omie.R).
#
# The first run downloads about 1,460 small daily files (roughly 15 minutes).
# Later runs skip files you already have and take under a minute.

library(tidyverse)
library(here)

source(here("R", "omie.R"))

# 1. Download the raw files (already-downloaded days are skipped)
raw_dir <- here("data", "raw", "omie", "marginalpdbc")
download_omie_prices("2022-01-01", "2025-12-31", raw_dir)

# 2. Read, keep 2022–2025, and convert to one price per hour
prices <- read_omie_prices(raw_dir, "2022-01-01", "2025-12-31") |>
  omie_to_hourly() |>
  mutate(
    year  = year(date),
    month = floor_date(date, "month")
  )

# Check: every year should have 8,760 hours (8,784 in the leap year 2024)
hours_per_year <- count(prices, year)
hours_per_year
stopifnot(all(hours_per_year$n == if_else(leap_year(hours_per_year$year), 8784L, 8760L)))

# Save the cleaned hourly table for later scripts (Module 3 will use it)
dir.create(here("data", "processed"), showWarnings = FALSE)
write_csv(prices, here("data", "processed", "omie_prices_hourly_2022_2025.csv"))

# 3. Headline numbers per year
# "midday" = hours 12 to 17, i.e. 11:00 to 17:00, when solar output is highest
yearly_summary <- prices |>
  group_by(year) |>
  summarise(
    mean_price          = mean(price_es),
    mean_price_midday   = mean(price_es[hour >= 12 & hour <= 17]),
    midday_vs_average   = mean_price_midday / mean_price,
    hours_zero_or_below = sum(price_es <= 0),
    share_zero_or_below = mean(price_es <= 0)
  )

yearly_summary

dir.create(here("outputs", "tables"), showWarnings = FALSE)
write_csv(yearly_summary, here("outputs", "tables", "03_omie_yearly_summary.csv"))

# Colours: one per year, in a fixed order (checked for colour-blind readability)
year_colours <- c("2022" = "#2a78d6", "2023" = "#eb6834", "2024" = "#1baf7a", "2025" = "#eda100")

# 4. Figure A: average price for each hour of the day, one line per year
hourly_profile <- prices |>
  filter(hour <= 24) |> # drop the extra hour on clock-change days
  group_by(year, hour) |>
  summarise(mean_price = mean(price_es), .groups = "drop") |>
  mutate(year = factor(year))

# Labels at the right-hand end of each line, so no legend lookup is needed
line_labels <- filter(hourly_profile, hour == 24)

fig_a <- ggplot(hourly_profile, aes(x = hour, y = mean_price, colour = year)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_text(
    data = line_labels, aes(label = year),
    hjust = 0, nudge_x = 0.3, size = 3.5, colour = "grey20", show.legend = FALSE
  ) +
  scale_colour_manual(values = year_colours) +
  scale_x_continuous(breaks = seq(1, 24, by = 2), expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Average Spanish day-ahead price by hour of the day, 2022–2025",
    subtitle = "Each year, midday prices fall further below the rest of the day as more solar comes online",
    x = "Hour of the day (1 = 00:00–01:00, Spanish time)",
    y = "Average price (€/MWh)",
    colour = "Year",
    caption = "Source: OMIE, marginalpdbc files. 15-minute prices from Oct 2025 averaged to hours."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

fig_a

ggsave(here("outputs", "figures", "03a_omie_hourly_profile_2022_2025.png"), fig_a,
       width = 8, height = 5, dpi = 300, bg = "white")

# 5. Figure B: share of hours each month with a price at or below zero
monthly_zero <- prices |>
  group_by(month) |>
  summarise(share_zero_or_below = mean(price_es <= 0))

fig_b <- ggplot(monthly_zero, aes(x = month, y = share_zero_or_below)) +
  geom_col(fill = "#2a78d6", width = 25) +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Share of hours with a price at or below zero, by month",
    subtitle = "Rare before 2024; since then, up to about one in three hours in spring months",
    x = NULL,
    y = "Share of hours in the month",
    caption = "Source: OMIE, marginalpdbc files."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

fig_b

ggsave(here("outputs", "figures", "03b_omie_zero_price_hours_by_month.png"), fig_b,
       width = 8, height = 4.5, dpi = 300, bg = "white")
