# Functions for reading the RE-Source / WindEurope PPA tracker data,
# entered by hand in data/raw/resource_tracker/.
#
# The tracker only gives totals and separate breakdowns (by sector, technology,
# company and deal type). The breakdowns are NOT linked to each other: we know
# how many MW Amazon bought and how many MW were solar, but not how many of
# Amazon's MW were solar.

library(tidyverse)
library(readxl)

tracker_breakdowns <- c("sectors", "technologies", "companies", "deal_types")

# Read every sheet of the tracker file into a list of tables.
# Breakdown rows with an empty snapshot_id are given the snapshot's ID when the
# file has exactly one snapshot (the raw file itself is not changed).
read_tracker <- function(path) {
  snapshots <- read_excel(path, sheet = "snapshots") |>
    filter(!is.na(snapshot_id))
  if (nrow(snapshots) == 0) stop("The snapshots sheet has no rows with a snapshot_id.")

  read_breakdown <- function(sheet) {
    d <- read_excel(path, sheet = sheet) |>
      filter(!is.na(capacity_mw))
    if (any(is.na(d$snapshot_id))) {
      if (nrow(snapshots) == 1) {
        message(sheet, ": empty snapshot_id filled with ", snapshots$snapshot_id)
        d$snapshot_id <- snapshots$snapshot_id
      } else {
        stop(sheet, ": some rows have no snapshot_id and there are several snapshots.")
      }
    }
    d
  }

  out <- map(set_names(tracker_breakdowns), read_breakdown)
  out$snapshots <- snapshots
  out
}

# Compare each breakdown's total with the snapshot total and stop on clear errors.
check_tracker <- function(tracker) {
  for (sheet in tracker_breakdowns) {
    d <- tracker[[sheet]]
    if (any(d$capacity_mw < 0)) stop(sheet, ": negative capacity values.")
    name_col <- names(d)[3]
    dups <- d |> count(snapshot_id, item = tolower(.data[[name_col]])) |> filter(n > 1)
    if (nrow(dups) > 0) stop(sheet, ": duplicated names: ", paste(dups$item, collapse = ", "))
  }

  # How close does each breakdown come to the snapshot total? Small gaps are
  # normal (rounding in the tracker); large gaps mean items are missing.
  map(tracker_breakdowns, \(sheet) {
    tracker[[sheet]] |>
      group_by(snapshot_id) |>
      summarise(breakdown_mw = sum(capacity_mw)) |>
      mutate(breakdown = sheet, .before = 1)
  }) |>
    list_rbind() |>
    left_join(select(tracker$snapshots, snapshot_id, total_capacity_mw), by = "snapshot_id") |>
    mutate(
      gap_mw = total_capacity_mw - breakdown_mw,
      gap_share = gap_mw / total_capacity_mw
    )
}
