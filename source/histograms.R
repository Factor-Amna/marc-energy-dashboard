# Makes the deck histograms as PNG files.  Usage: Rscript histograms.R
source("hist_helpers.R")
dir.create("figs", showWarnings = FALSE)

ggsave("figs/hist_raw.png",        plot_raw(),        width = 13, height = 6,   dpi = 200, bg = bg)
ggsave("figs/hist_transforms.png", plot_transforms(), width = 16, height = 6.5, dpi = 200, bg = bg)

# Skew before and after, printed for the transform table
tab <- tibble(
  variable = vars[names(transforms)],
  raw_skew = sapply(names(transforms), function(v) skew(stations[[v]])),
  transformed_skew = sapply(names(transforms), function(v) skew(transforms[[v]]$f(stations[[v]])))
)
print(tab, n = Inf)
