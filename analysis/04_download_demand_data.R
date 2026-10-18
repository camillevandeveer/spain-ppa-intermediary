# 04: Download the data for sizing the excluded market (Step 1.3)
#
# What we download, and why:
# 1. Eurostat nrg_pc_205_v: share of Spain's non-household electricity use in
#    each annual consumption band (under 20 MWh ... 150 GWh and over).
# 2. Eurostat nrg_bal_c: Spain's electricity final consumption by sector (GWh),
#    which turns the band shares into TWh and shows industry separately.
# 3. INE business register (DIRCE, table 73019): number of firms by sector and
#    number of employees.
# 4. INE structural business statistics for industry (76825), trade (76819) and
#    services (76813): employment and turnover by sector and firm size. We will
#    use employment to split each sector's electricity use by firm size. That
#    is an assumption (same electricity use per employee in small and large
#    firms of a sector); it is recorded in config/assumptions.yaml when used.
#
# This script only downloads, tidies and checks. The sizing itself comes in
# the next script.

library(tidyverse)
library(here)

source(here("R", "eurostat.R"))
source(here("R", "ine.R"))

eurostat_dir <- here("data", "raw", "eurostat")
ine_dir      <- here("data", "raw", "ine")
interim_dir  <- here("data", "interim")
dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Electricity use by consumption band (shares in %) ------------------------
bands <- download_eurostat("nrg_pc_205_v", "A....ES", eurostat_dir) |>
  read_eurostat() |>
  select(year, band = nrg_cons, share_pct = value, flag)

# Check: the bands of each year should add up to about 100%
band_totals <- bands |>
  group_by(year) |>
  summarise(total_pct = sum(share_pct, na.rm = TRUE), bands_reported = sum(!is.na(share_pct)))
band_totals
stopifnot(all(abs(band_totals$total_pct[band_totals$bands_reported == 7] - 100) < 1))

write_csv(bands, file.path(interim_dir, "eurostat_electricity_bands_es.csv"))

# 2. Electricity final consumption by sector (GWh) ------------------------------
balance <- download_eurostat("nrg_bal_c", "A..E7000.GWH.ES", eurostat_dir) |>
  read_eurostat() |>
  filter(str_starts(nrg_bal, "FC_"), str_ends(nrg_bal, "_E")) |> # final consumption, energy use
  select(year, nrg_bal, gwh = value, flag)

# Non-household final consumption = all final consumption minus households
non_household <- balance |>
  filter(nrg_bal %in% c("FC_E", "FC_OTH_HH_E")) |>
  pivot_wider(id_cols = year, names_from = nrg_bal, values_from = gwh) |>
  mutate(non_household_gwh = FC_E - FC_OTH_HH_E) |>
  arrange(desc(year))
head(non_household, 5)

# Check: industry subsectors should add up to the industry total
industry_check <- balance |>
  filter(str_starts(nrg_bal, "FC_IND_")) |>
  group_by(year) |>
  summarise(subsectors = sum(gwh[nrg_bal != "FC_IND_E"], na.rm = TRUE), total = gwh[nrg_bal == "FC_IND_E"]) |>
  filter(year >= 2015)
stopifnot(all(abs(industry_check$subsectors - industry_check$total) / industry_check$total < 0.01))

write_csv(balance, file.path(interim_dir, "eurostat_electricity_final_consumption_es.csv"))

# 3. Number of firms by sector and employee size (INE DIRCE) --------------------
firms <- download_ine_table(73019, ine_dir) |>
  read_ine_table()

glimpse(firms)
write_csv(firms, file.path(interim_dir, "ine_dirce_firms_by_sector_size.csv"))

# 4. Employment and turnover by sector and firm size (INE structural statistics)
structural <- c(industry = 76825, trade = 76819, services = 76813) |>
  imap(\(id, sector_group) {
    download_ine_table(id, ine_dir) |>
      read_ine_table() |>
      mutate(sector_group = sector_group, .before = 1)
  }) |>
  list_rbind()

structural |> count(sector_group, year)
write_csv(structural, file.path(interim_dir, "ine_structural_by_sector_size.csv"))
