# Project brief: Spain PPA market and a public tripartite intermediary

## Research questions

1. **SQ1:** For which buyers and projects is Spain's corporate PPA market not accessible, and why?
2. **SQ2:** How could tripartite agreements, with a public intermediary between generators and buyers, close that gap?

The approach is an **illustrative techno-economic analysis** inside a policy analysis. It is not a market forecast.

## Barrier → intermediary function → metric

| Excluded buyer | Barrier | Intermediary function | Metric |
|---|---|---|---|
| SMEs without strong credit | Can't post collateral / not an acceptable counterparty | Credit enhancement | Δ PPA price (€/MWh), collateral avoided |
| Buyers with demand too small | Load far below one project's output | Aggregation (back-to-back contracts) | Buyers per project, transaction cost €/MWh |
| Energy-intensive, flat 24/7 demand | Solar/wind shape mismatch | Portfolio firming (wind + solar + storage) | Hourly match %, firming cost €/MWh |
| Households | Too small, high transaction cost | Qualitative only (retailers, cooperatives, energy communities) | — |

## Modules (SQ2)

1. Credit enhancement: project cash-flow model, PPA price under different offtaker risk.
2. Aggregation: buyers per project and transaction cost per MWh.
3. Hourly shape/firming: portfolio grid vs. load profiles using hourly OMIE prices and generation profiles. **Core module.**
4. Public-risk Monte Carlo: buyer defaults × price paths → expected and tail loss for the intermediary.

## Folder layout

- `data/raw/`: original downloads. **Never edit or overwrite.**
- `data/interim/`, `data/processed/`: cleaned and model-ready data, always produced by code.
- `config/assumptions.yaml`: every numeric assumption, each with a source.
- `R/`: reusable functions.
- `analysis/`: numbered scripts run in order (`01_…`, `02_…`).
- `outputs/figures/`, `outputs/tables/`: generated results.
- `literature/notes/`: one note file per source.
- `paper/`: Quarto manuscript.

## Conventions

- Language: **R** (tidyverse style). Use `here::here()` for file paths.
- The user is a beginner in R, Git and Positron. Explain code in plain language, keep steps small, and let the user run code and make commits themselves unless they ask otherwise.
- Never invent data, numbers or citations. If a value or source is missing, leave a clear `TODO` and say so.
- Every external number goes into `config/assumptions.yaml` with its source and access date.
- Add simple sanity checks to data loaders (e.g. 8,760 or 8,784 hours per year, no missing hours).
- Interview notes and personal data must not be committed; keep them outside the repository or in an ignored folder.
