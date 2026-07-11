#!/usr/bin/env Rscript
# mokken scale evidence for the medication count used as a covariate
# (article: scalability H >= 0.70, no monotonicity violations)
#
# the scale sums a reversed "I do not take any medicine" indicator plus four
# category items (high blood cholesterol, high blood pressure, heart or brain
# infarction, diabetes), all "at least once a week" items of the health module.
# this script reproduces the mokken assumptions table from the frozen health
# extract, per wave (1 to 7) and pooled, and releases it as an aggregate table.
#
# output: output/medication_mokken.csv
# run from the compendium root; needs liss_health.sav in data/ or LISS_DATA_DIR

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

health <- read_liss("liss_health.sav")
items <- c("ch00a184", "ch00a169", "ch00a170", "ch00a171", "ch00a174")
stopifnot(all(c(items, "wavenr") %in% names(health)))

med <- data.frame(
  wavenr = as.numeric(health$wavenr),
  any_meds = abs(as.numeric(health$ch00a184) - 1),
  cholesterol = as.numeric(health$ch00a169),
  blood_pressure = as.numeric(health$ch00a170),
  heart_brain = as.numeric(health$ch00a171),
  diabetes = as.numeric(health$ch00a174)
)
med <- med[med$wavenr %in% 1:7 & stats::complete.cases(med), ]
cat(sprintf("complete medication item sets, waves 1 to 7: %d person-waves\n", nrow(med)))

mokken_row <- function(d, label) {
  co <- mokken::coefH(d, se = FALSE, results = FALSE)
  mono <- summary(mokken::check.monotonicity(d))
  data.frame(
    scope = label,
    n = nrow(d),
    Hij_min = round(min(co$Hij), 4),
    Hi_min = round(min(co$Hi), 4),
    H = round(as.numeric(co$H), 4),
    zsig_max = max(mono[, 9]),
    crit_max = max(mono[, 10])
  )
}

by_wave <- do.call(rbind, lapply(1:7, function(w) {
  mokken_row(med[med$wavenr == w, -1], paste0("wave_", w))
}))
pooled <- mokken_row(med[-1], "pooled")
res <- rbind(by_wave, pooled)

print(res, row.names = FALSE)
utils::write.csv(res, file.path("output", "medication_mokken.csv"),
  row.names = FALSE
)
cat(sprintf(
  "\nwrote output/medication_mokken.csv; pooled H = %.3f (article threshold 0.70)\n",
  res$H[res$scope == "pooled"]
))
