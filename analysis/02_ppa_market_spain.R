# 02: Who buys PPAs in Spain? (RE-Source / WindEurope tracker)
#
# Sub-question 1 asks who is left out of the PPA market. This script looks at
# who is IN it: how concentrated it is, which sectors and technologies
# dominate, and how big deals are. Small buyers' absence shows up as a market
# made of a few very large contracts.
#
# Reminder: the tracker's breakdowns are not linked to each other, so each one
# is analysed on its own (see R/resource_tracker.R).

library(tidyverse)
library(here)

source(here("R", "resource_tracker.R"))

# 1. Read the hand-entered tracker data and check it
tracker_file <- here("data", "raw", "resource_tracker", "(final) resource_ppa_summary_spain.xlsx")
tracker <- read_tracker(tracker_file)

tracker$snapshots

# How close does each breakdown come to the total? (gap_share near 0 = complete)
breakdown_check <- check_tracker(tracker)
breakdown_check

total_mw    <- tracker$snapshots$total_capacity_mw
total_deals <- tracker$snapshots$number_of_deals

dir.create(here("outputs", "tables"), showWarnings = FALSE)

# 2. Market concentration among buyers
# "Undisclosed buyer" groups many unnamed buyers, so it is not ranked as one
# buyer, but its MW stay in the total.
buyers <- tracker$companies |>
  filter(company != "Undisclosed buyer") |>
  arrange(desc(capacity_mw)) |>
  mutate(
    buyer_rank       = row_number(),
    share            = capacity_mw / total_mw,
    cumulative_share = cumsum(share)
  )

concentration <- tibble(
  total_mw                 = total_mw,
  total_deals              = total_deals,
  average_deal_mw          = total_mw / total_deals,
  named_buyers             = nrow(buyers),
  undisclosed_mw           = sum(tracker$companies$capacity_mw[tracker$companies$company == "Undisclosed buyer"]),
  largest_buyer            = buyers$company[1],
  largest_buyer_share      = buyers$share[1],
  top5_share               = sum(buyers$share[1:5]),
  top10_share              = sum(buyers$share[1:10]),
  buyers_for_half_of_mw    = min(which(buyers$cumulative_share >= 0.5)),
  median_buyer_mw          = median(buyers$capacity_mw),
  buyers_under_50mw        = sum(buyers$capacity_mw < 50),
  share_from_buyers_under_50mw = sum(buyers$share[buyers$capacity_mw < 50])
)

concentration |> pivot_longer(everything(), values_transform = as.character) |> print(n = Inf)
write_csv(concentration, here("outputs", "tables", "02_buyer_concentration.csv"))

# 3. Shares by sector, technology and deal type (each against the total)
# If the sectors add up to clearly less than the total (more than 1%), the
# difference is shown as its own row so the shares stay honest.
sector_gap_mw <- total_mw - sum(tracker$sectors$capacity_mw)
sectors <- tracker$sectors |>
  select(sector, capacity_mw)
if (sector_gap_mw / total_mw > 0.01) {
  sectors <- add_row(sectors, sector = "Not in sector breakdown", capacity_mw = sector_gap_mw)
}
sectors <- mutate(sectors, share = capacity_mw / total_mw)

technologies <- tracker$technologies |>
  select(technology, capacity_mw) |>
  mutate(share = capacity_mw / total_mw)

deal_types <- tracker$deal_types |>
  select(deal_type, capacity_mw, number_of_deals) |>
  mutate(
    share            = capacity_mw / total_mw,
    average_deal_mw  = capacity_mw / number_of_deals
  )

sectors
technologies
deal_types

write_csv(sectors, here("outputs", "tables", "02_sectors.csv"))
write_csv(technologies, here("outputs", "tables", "02_technologies.csv"))
write_csv(deal_types, here("outputs", "tables", "02_deal_types.csv"))

# Colours: one accent for what the chart is about, grey for the rest
accent <- "#2a78d6"
grey   <- "#b8b8b8"

pct <- scales::label_percent(accuracy = 1)

# 4. Figure A: how much of the market the largest buyers hold
top10 <- filter(buyers, buyer_rank == 10)

fig_a <- ggplot(buyers, aes(x = buyer_rank, y = cumulative_share)) +
  geom_step(colour = accent, linewidth = 0.8) +
  geom_point(data = slice(buyers, c(1, 10)), colour = accent, size = 2.5) +
  geom_text(
    data = slice(buyers, 1),
    aes(label = paste0(company, ": ", pct(share))),
    hjust = 0, nudge_x = 3, size = 3.5, colour = "grey20"
  ) +
  geom_text(
    data = top10,
    aes(label = paste0("Top 10 buyers: ", pct(cumulative_share))),
    hjust = 0, nudge_x = 3, nudge_y = -0.02, size = 3.5, colour = "grey20"
  ) +
  scale_y_continuous(labels = pct, limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = c(1, seq(20, 120, by = 20))) +
  labs(
    title = paste0("The 10 largest buyers hold ", pct(concentration$top10_share),
                   " of Spain's contracted PPA capacity"),
    subtitle = paste0(concentration$named_buyers, " named buyers ranked by size; ",
                      scales::comma(total_mw), " MW in ", total_deals, " deals (",
                      tracker$snapshots$period_start_year, "–", tracker$snapshots$period_end_year, ")"),
    x = "Buyers, largest to smallest",
    y = "Cumulative share of total MW",
    caption = paste0("Source: RE-Source / WindEurope PPA tracker, data as of ",
                     format(as.Date(tracker$snapshots$data_as_of), "%d %b %Y"),
                     ". 'Undisclosed buyer' (", scales::comma(concentration$undisclosed_mw),
                     " MW) counted in the total but not ranked.")
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

fig_a
ggsave(here("outputs", "figures", "02a_buyer_concentration.png"), fig_a,
       width = 8, height = 5, dpi = 300, bg = "white")

# 5. Figure B: share of capacity by buyer sector
top_sector <- slice_max(sectors, capacity_mw)

sectors_plot <- sectors |>
  mutate(
    sector  = fct_reorder(sector, capacity_mw),
    sector  = fct_relevel(sector, intersect("Not in sector breakdown", levels(sector))), # keep any gap row at the bottom
    is_top  = capacity_mw == max(capacity_mw)
  )

fig_b <- ggplot(sectors_plot, aes(x = share, y = sector, fill = is_top)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(share < 0.005, "<1%", pct(share))), hjust = 0, nudge_x = 0.005, size = 3.2, colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = accent, `FALSE` = grey), guide = "none") +
  scale_x_continuous(labels = pct, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("One sector, ", top_sector$sector, ", accounts for ",
                   pct(top_sector$share), " of contracted capacity"),
    subtitle = "Share of total PPA capacity in Spain by buyer sector",
    x = "Share of total MW",
    y = NULL,
    caption = "Source: RE-Source / WindEurope PPA tracker. Sector names as shown in the tracker."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

fig_b
ggsave(here("outputs", "figures", "02b_sectors.png"), fig_b,
       width = 8, height = 6, dpi = 300, bg = "white")

# 6. Figure C: deal types, with the number of deals and the average deal size
deal_plot <- deal_types |>
  mutate(
    deal_type = fct_reorder(deal_type, capacity_mw),
    label = paste0(scales::comma(capacity_mw), " MW · ", number_of_deals, if_else(number_of_deals == 1, " deal · ", " deals · "),
                   round(average_deal_mw), " MW avg.")
  )

fig_c <- ggplot(deal_plot, aes(x = capacity_mw, y = deal_type)) +
  geom_col(width = 0.7, fill = grey) +
  geom_text(aes(label = label), hjust = 0, nudge_x = 60, size = 3.2, colour = "grey20") +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.6))) +
  labs(
    title = paste0("The average Spanish PPA covers about ", round(total_mw / total_deals), " MW"),
    subtitle = "Contracted capacity by deal type, with number of deals and average deal size",
    x = "Contracted capacity (MW)",
    y = NULL,
    caption = "Source: RE-Source / WindEurope PPA tracker. Deal-type names as shown in the tracker."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

fig_c
ggsave(here("outputs", "figures", "02c_deal_types.png"), fig_c,
       width = 8, height = 5, dpi = 300, bg = "white")
