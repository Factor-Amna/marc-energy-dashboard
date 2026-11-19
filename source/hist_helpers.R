# Shared helpers for the MARC station histograms
# Run from the folder that holds stations_tidy.csv
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(tidyr); library(readr); library(patchwork)})

navy <- "#093679"; gold <- "#D99A0B"; mute <- "#6B665E"; bg <- "#F2EFEA"; red <- "#B03A2E"

theme_deck <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.background  = element_rect(fill = bg, color = NA),
          panel.background = element_rect(fill = bg, color = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title  = element_text(color = mute),
          axis.text   = element_text(color = mute),
          plot.title  = element_text(face = "bold", color = navy, size = base),
          plot.margin = margin(6, 10, 6, 6))
}

stations <- read_csv("stations_tidy.csv", show_col_types = FALSE)

# variable name -> label
vars <- c(parking = "Parking spots", ridership = "Weekday ridership",
          population = "Area population (2010)", density = "Intersection density",
          transit_score = "Transit score (1 to 5)", utilization = "Utilization rate 2014 (%)")

# transformations chosen on skew (see the deck): label and function
transforms <- list(
  parking    = list(label = "Parking spots, cube root",       f = function(x) x^(1/3), keep = TRUE),
  ridership  = list(label = "Weekday ridership, cube root",  f = function(x) x^(1/3),  keep = TRUE),
  density    = list(label = "Intersection density, square root", f = sqrt, keep = TRUE),
  population = list(label = "Area population, log(1 + x), rejected", f = log1p, keep = FALSE)
)

skew <- function(x) { x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }

# One histogram. vline = value to mark (a station), fill = bar colour.
hist_one <- function(x, title, bins = 15, vline = NULL, vline_label = NULL,
                     fill = navy, median_line = TRUE, discrete = FALSE) {
  d <- tibble(x = x[!is.na(x)])
  p <- ggplot(d, aes(x))
  p <- if (discrete) p + geom_bar(fill = fill, width = 0.8)
       else p + geom_histogram(bins = bins, fill = fill, color = bg, linewidth = 0.2)
  if (median_line) p <- p + geom_vline(xintercept = median(d$x), color = gold, linewidth = 1)
  if (!is.null(vline) && !is.na(vline))
    p <- p + geom_vline(xintercept = vline, color = red, linewidth = 1.2, linetype = "dashed")
  p + scale_x_continuous(labels = scales::label_comma()) +
    labs(title = title, x = NULL, y = "Stations") + theme_deck(11)
}

# Raw histograms, all six variables (gold = median)
plot_raw <- function(df = stations) {
  ps <- lapply(names(vars), function(v) {
    x <- df[[v]]
    hist_one(x, sprintf("%s (skew %.2f)", vars[[v]], skew(x)),
             discrete = (v == "transit_score"))
  })
  wrap_plots(ps, ncol = 3)
}

# Raw vs transformed, one column per variable
plot_transforms <- function(df = stations) {
  top <- lapply(names(transforms), function(v) {
    hist_one(df[[v]], sprintf("%s, raw (skew %.2f)", vars[[v]], skew(df[[v]])))
  })
  bot <- lapply(names(transforms), function(v) {
    t <- transforms[[v]]; y <- t$f(df[[v]])
    hist_one(y, sprintf("%s (skew %.2f)", t$label, skew(y)), fill = ifelse(t$keep, navy, red))
  })
  wrap_plots(c(top, bot), ncol = 4)
}

# Four histograms for one station, with that station marked (red dashed line)
plot_station <- function(row, df = stations) {
  keep <- c("parking", "ridership", "population", "density")
  ps <- lapply(keep, function(v) {
    val <- row[[v]]
    pct <- if (is.na(val)) NA else round(100 * mean(df[[v]] <= val, na.rm = TRUE))
    ttl <- if (is.na(pct)) sprintf("%s (no value)", vars[[v]])
           else sprintf("%s: %s (%s percentile)", vars[[v]], format(round(val), big.mark = ","), pct)
    hist_one(df[[v]], ttl, vline = val)
  })
  wrap_plots(ps, ncol = 2)
}
