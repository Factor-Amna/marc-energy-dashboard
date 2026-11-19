# MARC Station Energy Opportunity Screening

An interactive dashboard for 38 MARC stations from the MDOT MTA station file, built for GW EMSE 6577.

**Live page:** https://factor-amna.github.io/marc-energy-dashboard/

What it shows:
- **Overview:** linked filters, a parking versus ridership chart, a lighting and controls cost chart and a station table.
- **Station profiles:** a click-through view of every station.
- **Histograms:** the raw and transformed distributions of the station variables.

Costs are screening estimates from published unit cost ranges, not contractor quotes. The study matches the six MTA energy conservation measures to station types and is not causal.

## Rebuild it

The R source is in `source/`. With R, `rmarkdown`, `ggplot2`, `dplyr`, `readr`, `tidyr`, `patchwork`, `plotly`, `DT`, `crosstalk` and `htmltools` installed, run `rmarkdown::render("station_dashboard.Rmd")` from that folder. The output is one self-contained HTML file.
