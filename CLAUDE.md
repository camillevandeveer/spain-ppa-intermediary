# Project brief: Spain PPA market and a public tripartite intermediary

## Research questions

1. **SQ1:** For which buyers and projects is Spain's corporate PPA market not accessible, and why?
2. **SQ2:** How could tripartite agreements, with a public intermediary between generators and buyers, close that gap?
3. **SQ3:** How could the state-backed intermediary be structured (institution, contract role, procurement on both sides, products, funding), and how does each design change what it can achieve and what risk the state carries?

The approach is an **illustrative techno-economic analysis** inside a policy analysis. It is not a market forecast.

Key context: CATF, "Tripartite contracting for clean electricity" (https://www.catf.us/resource/tripartite-contracting-for-clean-electricity/), which proposes a state entity as central counterparty to generators and industrial consumers.

## Barrier → intermediary function → metric

| Excluded buyer | Barrier | Intermediary function | Metric |
|---|---|---|---|
| SMEs without strong credit | Can't post collateral / not an acceptable counterparty | Credit enhancement | Δ PPA price (€/MWh), collateral avoided |
| Buyers with demand too small | Load far below one project's output | Aggregation (back-to-back contracts) | Buyers per project, transaction cost €/MWh |
| Energy-intensive, flat 24/7 demand | Solar/wind shape mismatch | Portfolio firming (wind + solar + storage) | Hourly match %, firming cost €/MWh |

Households are out of scope for now (decided by the user and their partner, 17 Oct 2026).

## Modules (SQ2)

1. Credit enhancement: project cash-flow model, PPA price under different offtaker risk.
2. Aggregation: buyers per project and transaction cost per MWh.
3. Hourly shape/firming: portfolio grid vs. load profiles using hourly OMIE prices and generation profiles. **Core module.**
4. Public-risk Monte Carlo: buyer defaults × price paths → expected and tail loss for the intermediary.

## Step 1.3: sizing the excluded market (decisions of 18 Oct 2026)

Goal: electricity demand (TWh/year) outside today's PPA market, split by barrier.

- **Scope:** all non-household demand (businesses and public sector), with industry shown separately using the sector data.
- **Segments,** following Eurostat's annual consumption bands (dataset `nrg_pc_205_v`, share of non-household consumption by band; totals from energy balances `nrg_bal_c`):
  - Very small: under 0.5 GWh (bands IA–IB). Barrier: size and transaction costs.
  - Small and medium: 0.5–20 GWh (bands IC–ID). Barrier: size and credit.
  - Large: 20 GWh and over (bands IE–IG). Can sign alone; flat-demand industry (chemicals, metals, non-metallic minerals, paper) faces the shape barrier.
- **"Too small to sign alone" = under 20 GWh/year;** test 2 and 70 GWh as sensitivities. The 1 GWh guarantee-fund threshold falls inside band IC, so report it as a range using the 0.5 and 2 GWh band edges.
- **Consumer vs. company:** bands count consumers, not firms (multi-site chains look small). Add INE firm-size data (DIRCE firm counts by employees and sector) to estimate demand by company size. TODO: check whether INE publishes electricity use by firm size (e.g. its industrial energy consumption survey).
- Sector and size come from separate tables and cannot be cross-tabulated.

## Intermediary design options (SQ3)

Mostly qualitative. Sketch a short list of designs (2–4) early, before Step 1.3 and before fixing module assumptions, because the design decides which modules apply and which data are needed; then assess the designs in full after the modules run. Design dimensions:

| Dimension | Options to compare |
|---|---|
| Contract role | Guarantor only (bilateral PPA stays between generator and buyer) · Central counterparty / principal (buys from generators, sells back-to-back to buyers) · Platform / aggregator (standard contracts and matching, little balance-sheet risk) |
| Institutional home | Existing body (e.g. IDAE, ICO, CESCE) · new public vehicle · public–private set-up |
| Generator-side procurement | Competitive auction · standardised tender · bilateral negotiation |
| Buyer-side allocation | Eligibility rules (size, sector, credit, load profile) · open enrolment vs. allocation rounds · pricing (pass-through vs. fixed) |
| Product | Pay-as-produced · baseload · shaped / 24/7 · physical vs. financial (CfD) · tenor |
| Funding and risk capital | Guarantee fund · fees · state budget · EU / EIB backing; state-aid compliance |

How design links to the modules: credit enhancement (Module 1) applies to every design except a pure platform; aggregation (Module 2) applies to principal and platform designs; shape risk (Module 3) and most of the public risk (Module 4) fall on the state only in the principal design.

## Folder layout

- `data/raw/`: original downloads. **Never edit or overwrite.**
  - Exception: `data/raw/resource_tracker/(final) resource_ppa_summary_spain.xlsx` (the checked, filled-in copy of the template; read with `read_tracker()` in `R/resource_tracker.R`) is entered by hand by the user from the RE-Source tracker. The tracker only gives aggregates, not deal-level data. Sheet `snapshots` holds one row per filter set (total MW, number of deals). Sheets `sectors`, `technologies`, `companies` and `deal_types` hold the separate breakdowns (MW per item), linked to a snapshot by `snapshot_id`. Breakdowns cannot be cross-tabulated with each other. Column definitions are in sheet `codebook`. Code only reads this file; never rename its sheets or columns.
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

## Progress log

A running plain-language summary for the user's research partner lives in a Claude Doc: "Spain PPA research: progress log" (https://claude.ai/code/artifact/d29e26d6-bf14-4140-8d69-a1226f695cf5). After each meaningful step (new data, new results, a changed plan), update the relevant section and add a row to its "Update log" table (newest first). Keep the language non-technical.
