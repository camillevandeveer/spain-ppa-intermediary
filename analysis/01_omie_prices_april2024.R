# 01: First look at Spanish day-ahead electricity prices (April 2024)
#
# Question: what does a typical spring day look like for prices in Spain?
# April 2024 is a good test month: lots of solar, and many hours with prices
# at or below zero. That is the "shape problem" behind SQ1 and Module 3.
#
# Run this script from top to bottom in Positron (Cmd+Shift+Enter, or
# select lines and press Cmd+Enter to run them one at a time).

library(tidyverse)
library(here)

source(here("R", "omie.R"))

# 1. Download the raw files (only the first time; already-downloaded days are skipped)
raw_dir <- here("data", "raw", "omie", "marginalpdbc")
download_omie_prices("2024-04-01", "2024-04-30", raw_dir)

# 2. Read them into one table and check it
prices <- read_omie_prices(raw_dir) |>
  filter(date >= as.Date("2024-04-01"), date <= as.Date("2024-04-30")) |>
  rename(hour = period)

# April has 30 days and no clock change, so we expect exactly 30 × 24 rows
stopifnot(nrow(prices) == 30 * 24)

# Save the cleaned table so later scripts can use it without re-reading raw files
dir.create(here("data", "interim"), showWarnings = FALSE)
write_csv(prices, here("data", "interim", "omie_prices_april2024.csv"))

prices         # print the first rows
summary(prices) # min, max, mean of each column

# 3. A few headline numbers
prices |>
  summarise(
    mean_price      = mean(price_es),
    hours           = n(),
    hours_zero_or_below = sum(price_es <= 0),
    share_zero_or_below = mean(price_es <= 0)
  )

# 4. Average price for each hour of the day
hourly_profile <- prices |>
  group_by(hour) |>
  summarise(mean_price = mean(price_es))

# 5. Plot: each grey line is one day; the dark line is the monthly average
p <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_line(
    data = prices,
    aes(x = hour, y = price_es, group = date),
    colour = "grey75", linewidth = 0.3
  ) +
  geom_line(
    data = hourly_profile,
    aes(x = hour, y = mean_price),
    colour = "#1f5f99", linewidth = 1
  ) +
  scale_x_continuous(breaks = seq(1, 24, by = 2)) +
  labs(
    title = "Spanish day-ahead prices by hour, April 2024",
    subtitle = "Grey: each day. Blue: monthly average. Midday solar pushes prices to zero or below.",
    x = "Hour of the day (period 1 = 00:00–01:00, Spanish time)",
    y = "Price (€/MWh)",
    caption = "Source: OMIE, marginalpdbc files"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

p

# 6. Save the figure
ggsave(here("outputs", "figures", "01_omie_prices_april2024.png"), p,
       width = 8, height = 5, dpi = 300, bg = "white")
