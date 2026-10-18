# 05: How big is the market that today's PPAs leave out? (Step 1.3)
#
# Uses the data downloaded by script 04 and the tracker data from script 02.
# All assumptions are in config/assumptions.yaml (section market_sizing).
#
# Four questions:
# A. How much non-household electricity do small, medium and large consumers
#    use? (Eurostat consumption bands)
# B. How much of it is industry, and how much is flat-demand industry?
#    (Eurostat energy balance)
# C. How much do small and medium-sized companies use? (INE employment by firm
#    size, used to split each sector's electricity)
# D. How much electricity do today's PPAs cover, compared with these groups?

library(tidyverse)
library(here)
library(yaml)

source(here("R", "eurostat.R"))
source(here("R", "resource_tracker.R"))

assumptions <- read_yaml(here("config", "assumptions.yaml"))$market_sizing
base_year   <- assumptions$base_year$value

interim_dir <- here("data", "interim")
tables_dir  <- here("outputs", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

balance <- read_csv(file.path(interim_dir, "eurostat_electricity_final_consumption_es.csv"), show_col_types = FALSE)
bands   <- read_csv(file.path(interim_dir, "eurostat_electricity_bands_es.csv"), show_col_types = FALSE)

gwh <- function(code, yr = base_year) balance$gwh[balance$nrg_bal == code & balance$year == yr]

# Non-household electricity use = all final consumption minus households
non_household_gwh <- gwh("FC_E") - gwh("FC_OTH_HH_E")

# A. Demand by consumer size ---------------------------------------------------
# Each Eurostat band, with its upper limit in GWh, so any cut-off can be applied
band_info <- tribble(
  ~band,              ~band_label,          ~upper_gwh,
  "MWH_LT20",         "Under 0.02 GWh",     0.02,
  "MWH20-499",        "0.02–0.5 GWh",  0.5,
  "MWH500-1999",      "0.5–2 GWh",     2,
  "MWH2000-19999",    "2–20 GWh",      20,
  "MWH20000-69999",   "20–70 GWh",     70,
  "MWH70000-149999",  "70–150 GWh",    150,
  "MWH_GE150000",     "150 GWh and over",   Inf
)

demand_by_band <- bands |>
  filter(year == base_year) |>
  inner_join(band_info, by = "band") |>
  arrange(upper_gwh) |>
  mutate(
    twh = share_pct / 100 * non_household_gwh / 1000,
    segment = case_when(
      upper_gwh <= 0.5 ~ "Very small (under 0.5 GWh)",
      upper_gwh <= 20  ~ "Small and medium (0.5–20 GWh)",
      TRUE             ~ "Large (20 GWh and over)"
    )
  )
stopifnot(nrow(demand_by_band) == 7)

demand_by_segment <- demand_by_band |>
  group_by(segment) |>
  summarise(twh = sum(twh), share = sum(share_pct) / 100) |>
  arrange(twh)
demand_by_segment

# Sensitivity: how much demand is "too small to sign alone" at each cut-off?
cutoffs <- c(assumptions$too_small_cutoff_gwh$sensitivities, assumptions$too_small_cutoff_gwh$value) |> sort()
too_small <- tibble(cutoff_gwh = cutoffs) |>
  mutate(
    twh   = map_dbl(cutoff_gwh, \(c) sum(demand_by_band$twh[demand_by_band$upper_gwh <= c])),
    share = twh / (non_household_gwh / 1000),
    is_main_case = cutoff_gwh == assumptions$too_small_cutoff_gwh$value
  )
too_small

# Trend: share of demand from consumers under the main cut-off, by year
too_small_trend <- bands |>
  inner_join(band_info, by = "band") |>
  group_by(year) |>
  summarise(share_too_small = sum(share_pct[upper_gwh <= assumptions$too_small_cutoff_gwh$value]) / 100)
too_small_trend

write_csv(demand_by_band, file.path(tables_dir, "05_demand_by_band.csv"))
write_csv(too_small, file.path(tables_dir, "05_too_small_sensitivity.csv"))

# B. Demand by sector ------------------------------------------------------------
flat_codes <- unlist(assumptions$flat_demand_sectors$value)

demand_by_sector <- tibble(
  group = c("Industry", "  of which flat-demand industry", "Commercial and public services",
            "Transport", "Agriculture, fishing and other"),
  twh = c(
    gwh("FC_IND_E"),
    sum(map_dbl(flat_codes, gwh)),
    gwh("FC_OTH_CP_E"),
    gwh("FC_TRA_E"),
    gwh("FC_OTH_AF_E") + gwh("FC_OTH_FISH_E") + gwh("FC_OTH_NSP_E")
  ) / 1000
) |>
  mutate(share_of_non_household = twh / (non_household_gwh / 1000))
demand_by_sector

# Check: the groups (without the "of which" line) add up to the non-household total
stopifnot(abs(sum(demand_by_sector$twh[-2]) - non_household_gwh / 1000) < 0.5)

write_csv(demand_by_sector, file.path(tables_dir, "05_demand_by_sector.csv"))

# C. Demand by company size --------------------------------------------------------
# Employment by firm size, used to split each sector group's electricity.
# Industry = mining + manufacturing (closest match to the energy balance's
# industry); services = trade + services excluding transport (transport
# electricity is counted separately in the energy balance).
structural <- read_csv(file.path(interim_dir, "ine_structural_by_sector_size.csv"), show_col_types = FALSE) |>
  filter(`Personas/Horas` == "Personal ocupado", is.na(DIVISIONES), is.na(GRUPOS))

size_class <- function(x) case_when(
  x %in% c("De 0 a 1", "De 2 a 9", "De 0 a 9") ~ "Micro (0–9 employees)",
  x %in% c("De 10 a 19", "De 20 a 49")         ~ "Small (10–49)",
  x == "De 50 a 249"                            ~ "Medium (50–249)",
  x == "De 250 o más"                           ~ "Large (250+)"
)

industry_sections <- c("Industrias extractivas", "Industria manufacturera")
services_sections <- c("Comercio al por mayor y al por menor; reparación de vehículos de motor y motocicletas",
                       setdiff(unique(structural$SECCIONES[structural$sector_group == "services"]),
                               c(NA, "Transporte y almacenamiento")))

employment <- structural |>
  filter(SECCIONES %in% c(industry_sections, services_sections)) |>
  mutate(
    sector = if_else(SECCIONES %in% industry_sections, "Industry", "Commercial and public services"),
    size   = size_class(`Estrato de ocupados`)
  ) |>
  # Services report 0-1 and 2-9 separately, industry reports 0-9: keep the finest split, never both
  filter(!is.na(size), !(`Estrato de ocupados` == "De 0 a 9" & sector_group != "industry")) |>
  group_by(sector, size) |>
  summarise(employees = sum(value), .groups = "drop")

# Check: size classes add up to the sections' own totals
section_totals <- structural |>
  filter(SECCIONES %in% c(industry_sections, services_sections), `Estrato de ocupados` == "Total") |>
  mutate(sector = if_else(SECCIONES %in% industry_sections, "Industry", "Commercial and public services")) |>
  group_by(sector) |>
  summarise(total = sum(value))
check_sizes <- employment |> group_by(sector) |> summarise(sum_sizes = sum(employees)) |> left_join(section_totals, by = "sector")
stopifnot(all(abs(check_sizes$sum_sizes - check_sizes$total) / check_sizes$total < 0.001))

sector_twh <- c(Industry = gwh("FC_IND_E"), `Commercial and public services` = gwh("FC_OTH_CP_E")) / 1000

demand_by_firm_size <- employment |>
  group_by(sector) |>
  mutate(
    employment_share = employees / sum(employees),
    twh = employment_share * sector_twh[sector]
  ) |>
  ungroup() |>
  mutate(size = factor(size, levels = c("Micro (0–9 employees)", "Small (10–49)", "Medium (50–249)", "Large (250+)"))) |>
  arrange(sector, size)
demand_by_firm_size

sme_twh <- sum(demand_by_firm_size$twh[demand_by_firm_size$size != "Large (250+)"])
sme_twh

write_csv(demand_by_firm_size, file.path(tables_dir, "05_demand_by_firm_size.csv"))

# D. What today's PPAs cover ----------------------------------------------------------
# Capacity factor = production / (average capacity during the year x hours)
cf_years <- unlist(assumptions$capacity_factor_method$years)
capacity <- download_eurostat("nrg_inf_epcrw", "A.RA300+RA420.CAP_NET_ELC.MW.ES", here("data", "raw", "eurostat")) |>
  read_eurostat() |>
  select(siec, year, mw = value)
production <- download_eurostat("nrg_bal_peh", "A.GEP.RA300+RA420.GWH.ES", here("data", "raw", "eurostat")) |>
  read_eurostat() |>
  select(siec, year, gwh = value)

capacity_factors <- capacity |>
  arrange(siec, year) |>
  group_by(siec) |>
  mutate(avg_mw = (mw + lag(mw)) / 2) |>
  ungroup() |>
  inner_join(production, by = c("siec", "year")) |>
  filter(year %in% cf_years) |>
  mutate(hours = if_else(leap_year(year), 8784, 8760), capacity_factor = gwh * 1000 / (avg_mw * hours)) |>
  group_by(siec) |>
  summarise(capacity_factor = mean(capacity_factor))
capacity_factors

cf_solar <- capacity_factors$capacity_factor[capacity_factors$siec == "RA420"]
cf_wind  <- capacity_factors$capacity_factor[capacity_factors$siec == "RA300"]

tracker <- suppressMessages(read_tracker(here("data", "raw", "resource_tracker", "(final) resource_ppa_summary_spain.xlsx")))

ppa_energy <- tracker$technologies |>
  select(technology, capacity_mw) |>
  mutate(
    capacity_factor = case_when(
      technology == "Solar" ~ cf_solar,
      technology == "Wind"  ~ cf_wind,
      TRUE                  ~ (cf_solar + cf_wind) / 2
    ),
    twh_per_year = capacity_mw * capacity_factor * 8760 / 1e6
  )
ppa_energy

ppa_twh <- sum(ppa_energy$twh_per_year)
large_twh <- demand_by_segment$twh[demand_by_segment$segment == "Large (20 GWh and over)"]

coverage <- tibble(
  measure = c("PPA-contracted energy (TWh/year)", "Non-household demand (TWh)", "PPAs as share of non-household demand",
              "Demand from consumers under the cut-off (TWh)", "Demand from large consumers (TWh)",
              "PPAs as share of large-consumer demand", "Demand from SMEs, estimated (TWh)"),
  value = c(ppa_twh, non_household_gwh / 1000, ppa_twh / (non_household_gwh / 1000),
            too_small$twh[too_small$is_main_case], large_twh, ppa_twh / large_twh, sme_twh)
)
coverage
write_csv(coverage, file.path(tables_dir, "05_ppa_coverage.csv"))

# Figure: business demand by consumer size next to all PPAs, on one scale ------------
# Two bars on the same TWh scale, so the PPA total is compared with total demand
# (not with single slices of it).
segment_levels <- c("Very small (under 0.5 GWh)", "Small and medium (0.5–20 GWh)", "Large (20 GWh and over)")
segment_short  <- c("Very small", "Small and medium", "Large")

plot_data <- bind_rows(
  demand_by_segment |>
    mutate(bar = "Business electricity demand",
           part = factor(segment, levels = segment_levels),
           label = paste0(segment_short[as.integer(part)], "\n", round(twh), " TWh")),
  tibble(bar = "All PPAs signed 2021–2026", part = "PPAs", twh = ppa_twh,
         label = paste0(round(ppa_twh), " TWh"))
) |>
  mutate(
    bar  = factor(bar, levels = c("All PPAs signed 2021–2026", "Business electricity demand")),
    part = factor(as.character(part), levels = c(segment_levels, "PPAs"))
  )

# Demand segments in shades of grey (light = small consumers); PPAs in the accent colour
part_colours <- c(
  "Very small (under 0.5 GWh)"        = "#d9d9d9",
  "Small and medium (0.5–20 GWh)" = "#b3b3b3",
  "Large (20 GWh and over)"           = "#8c8c8c",
  "PPAs"                              = "#2a78d6"
)

fig <- ggplot(plot_data, aes(x = twh, y = bar, fill = part)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.8, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = label, colour = part == "PPAs"), position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3.4, lineheight = 0.9, show.legend = FALSE) +
  scale_fill_manual(values = part_colours, guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"), guide = "none") +
  annotate("text", x = non_household_gwh / 1000, y = "Business electricity demand", hjust = -0.15, size = 3.4,
           fontface = "bold", colour = "grey15", label = paste0(round(non_household_gwh / 1000), " TWh")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("All PPAs signed in Spain equal ", scales::percent(ppa_twh / (non_household_gwh / 1000), accuracy = 1),
                   " of business electricity demand"),
    subtitle = paste0("Electricity use of businesses and public bodies in ", base_year,
                      ", by consumer size, next to the estimated yearly output of all PPAs"),
    x = "TWh per year",
    y = NULL,
    caption = paste0("Consumer size = annual use per consumer. Very small: under 0.5 GWh; small and medium: 0.5–20 GWh; large: 20 GWh and over.\n",
                     "PPA output estimated from contracted MW and Spain's average solar and wind output per MW.\n",
                     "Sources: Eurostat (nrg_pc_205_v, nrg_bal_c, nrg_inf_epcrw, nrg_bal_peh); RE-Source / WindEurope tracker; our calculation (script 05).")
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey35"))

fig
ggsave(here("outputs", "figures", "05_demand_by_consumer_size.png"), fig,
       width = 9, height = 4, dpi = 300, bg = "white")
