#!/usr/bin/env Rscript
# resolve the frve derivation and its spearman-brown reliability
# run from the compendium root; needs data/liss_merged_long.sav and the raw
# health modules ch07a to ch13g in data/ (as downloaded by 00_acquire_data.R)
#
# outputs, printed and written to output/:
#   1. the empirical (vegetables, fruit) -> frve mapping with a determinism check
#   2. per-wave item correlations and spearman-brown coefficients, raw scale and,
#      when the mapping is item-wise, the recoded scale entering the composite
#   3. the count of sample respondents whose education category at the first
#      observed wave was 'other' or 'not (yet) completed' (oplmet 7 to 9)

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

wave_prefixes <- c(
  ch07a = 1, ch08b = 2, ch09c = 3, ch10d = 4, ch11e = 5, ch12f = 6, ch13g = 7
)
in_range <- function(x, lo, hi) {
  x <- suppressWarnings(as.numeric(x))
  dplyr::if_else(x >= lo & x <= hi, x, NA_real_)
}

merged <- read_liss("liss_merged_long.sav")
sample_ids <- utils::read.csv(file.path("output", "sample_ids.csv"))$id

# 1. raw items, one frame across the seven waves ------------------------------
raw <- purrr::imap_dfr(wave_prefixes, function(w, prefix) {
  f <- list.files("data", pattern = paste0("^", prefix, ".*\\.sav$"),
    full.names = TRUE
  )
  stopifnot(length(f) == 1)
  d <- haven::read_sav(f)
  veg_col <- grep(paste0("^", prefix, "196$"), names(d), value = TRUE)
  fru_col <- grep(paste0("^", prefix, "197$"), names(d), value = TRUE)
  stopifnot(length(veg_col) == 1, length(fru_col) == 1)
  dplyr::tibble(
    nomem_encr = as.numeric(d$nomem_encr),
    wavenr = w,
    veg = in_range(d[[veg_col]], 1, 6),
    fruit = in_range(d[[fru_col]], 1, 6)
  )
})

j <- merged %>%
  dplyr::select(nomem_encr, wavenr, frve) %>%
  dplyr::inner_join(raw, by = c("nomem_encr", "wavenr")) %>%
  dplyr::filter(!is.na(veg), !is.na(fruit), !is.na(frve))

# 2. empirical mapping and determinism ----------------------------------------
map_tab <- j %>%
  dplyr::count(veg, fruit, frve) %>%
  dplyr::arrange(veg, fruit, frve)
n_rules <- map_tab %>%
  dplyr::count(veg, fruit, name = "n_frve_values")
deterministic <- all(n_rules$n_frve_values == 1)

cat("\n== (veg, fruit) -> frve mapping ==\n")
print(as.data.frame(map_tab), row.names = FALSE)
cat("deterministic mapping:", deterministic, "\n")
n_anomalous <- sum(map_tab$n) -
  sum(tapply(map_tab$n, paste(map_tab$veg, map_tab$fruit), max))
writeLines(
  sprintf(
    "deterministic mapping: %s (%d anomalous person-waves of %d)",
    deterministic, n_anomalous, nrow(j)
  ),
  file.path("output", "frve_mapping_deterministic.txt")
)
utils::write.csv(map_tab, file.path("output", "frve_mapping.csv"),
  row.names = FALSE
)

# item-wise hypothesis: a single recode g: 1..6 -> 1..3 applied to each item,
# then averaged (and, if needed, rounded); rows with veg == fruit expose g
g_tab <- j %>%
  dplyr::filter(veg == fruit) %>%
  dplyr::count(item = veg, frve) %>%
  dplyr::arrange(item, frve)
cat("\n== candidate item recode from veg == fruit rows ==\n")
print(as.data.frame(g_tab), row.names = FALSE)

item_wise <- FALSE
if (deterministic && all(table(g_tab$item) == 1)) {
  g <- stats::setNames(g_tab$frve, g_tab$item)
  pred_mean <- (g[as.character(j$veg)] + g[as.character(j$fruit)]) / 2
  exact_mean <- isTRUE(all.equal(unname(pred_mean), j$frve))
  exact_round <- all(round(pred_mean) == j$frve)
  cat("\nitem-wise recode reproduces frve as plain mean:", exact_mean, "\n")
  cat("item-wise recode reproduces frve after rounding:", exact_round, "\n")
  item_wise <- exact_mean || exact_round
  if (item_wise) {
    j$veg3 <- unname(g[as.character(j$veg)])
    j$fruit3 <- unname(g[as.character(j$fruit)])
  }
}

# 3. spearman-brown, per wave and pooled --------------------------------------
sb <- function(r) 2 * r / (1 + r)
reliability <- function(d, a, b) {
  per_wave <- d %>%
    dplyr::group_by(wavenr) %>%
    dplyr::summarise(
      n = dplyr::n(),
      r = stats::cor(.data[[a]], .data[[b]]),
      spearman_brown = sb(r),
      .groups = "drop"
    )
  pooled_r <- stats::cor(d[[a]], d[[b]])
  list(per_wave = per_wave, pooled_r = pooled_r, pooled_sb = sb(pooled_r))
}

j_sample <- dplyr::filter(j, nomem_encr %in% sample_ids)

cat("\n== reliability, raw 1-6 items (analysis sample) ==\n")
raw_rel <- reliability(j_sample, "veg", "fruit")
print(as.data.frame(raw_rel$per_wave), row.names = FALSE)
cat(sprintf(
  "pooled r = %.3f, pooled spearman-brown = %.3f, per-wave sb range %.3f to %.3f\n",
  raw_rel$pooled_r, raw_rel$pooled_sb,
  min(raw_rel$per_wave$spearman_brown), max(raw_rel$per_wave$spearman_brown)
))

if (item_wise) {
  cat("\n== reliability, recoded items as they enter the composite ==\n")
  rec_rel <- reliability(j_sample, "veg3", "fruit3")
  print(as.data.frame(rec_rel$per_wave), row.names = FALSE)
  cat(sprintf(
    "pooled r = %.3f, pooled spearman-brown = %.3f, per-wave sb range %.3f to %.3f\n",
    rec_rel$pooled_r, rec_rel$pooled_sb,
    min(rec_rel$per_wave$spearman_brown), max(rec_rel$per_wave$spearman_brown)
  ))
  cat("cite these recoded-scale values in the manuscript; the raw-scale values\n")
  cat("above are the sensitivity companion\n")
  utils::write.csv(rec_rel$per_wave,
    file.path("output", "frve_reliability_recoded.csv"),
    row.names = FALSE
  )
}
utils::write.csv(raw_rel$per_wave,
  file.path("output", "frve_reliability_raw.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(
    pooled_r = raw_rel$pooled_r,
    pooled_spearman_brown = raw_rel$pooled_sb,
    sb_min = min(raw_rel$per_wave$spearman_brown),
    sb_max = max(raw_rel$per_wave$spearman_brown)
  ),
  file.path("output", "frve_reliability_pooled.csv"),
  row.names = FALSE
)

# 4. oplmet 7-9 count at first observed wave (analysis sample) ----------------
educ_first <- merged %>%
  dplyr::filter(nomem_encr %in% sample_ids, wavenr %in% 1:7, !is.na(oplmet)) %>%
  dplyr::arrange(nomem_encr, wavenr) %>%
  dplyr::distinct(nomem_encr, .keep_all = TRUE)
n_other <- sum(educ_first$oplmet %in% 7:9)
cat(sprintf(
  "\nrespondents with oplmet in {7, 8, 9} at first observed wave: %d of %d\n",
  n_other, nrow(educ_first)
))
writeLines(
  sprintf(
    "respondents with oplmet in {7, 8, 9} at first observed wave: %d of %d",
    n_other, nrow(educ_first)
  ),
  file.path("output", "educ_other_count.txt")
)

# 5. baseline correlations and adjacent-wave stabilities (analysis sample) ----
# raw observed descriptives that back the cross-sectional and stability
# sentences; pa uses the same 40 h/week plausibility ceiling as the pipeline
# (cb111), defined here because this script runs standalone
pa_ceiling <- 40
foc <- merged %>%
  dplyr::filter(nomem_encr %in% sample_ids, wavenr %in% 1:7) %>%
  dplyr::transmute(
    nomem_encr, wavenr,
    bmi,
    pa = dplyr::if_else(sport > pa_ceiling, NA_real_, sport),
    fv = frve
  )

w1 <- dplyr::filter(foc, wavenr == 1)
cat("\n== baseline (wave 1) pearson correlations ==\n")
print(round(stats::cor(w1[c("bmi", "pa", "fv")],
  use = "pairwise.complete.obs"
), 3))
utils::write.csv(
  round(stats::cor(w1[c("bmi", "pa", "fv")], use = "pairwise.complete.obs"), 3),
  file.path("output", "baseline_correlations.csv")
)

stability <- function(v) {
  r <- vapply(1:6, function(k) {
    a <- foc %>%
      dplyr::filter(wavenr == k) %>%
      dplyr::transmute(nomem_encr, x = .data[[v]])
    b <- foc %>%
      dplyr::filter(wavenr == k + 1) %>%
      dplyr::transmute(nomem_encr, y = .data[[v]])
    m <- dplyr::inner_join(a, b, by = "nomem_encr")
    stats::cor(m$x, m$y, use = "pairwise.complete.obs")
  }, numeric(1))
  c(mean = mean(r), lo = min(r), hi = max(r))
}

cat("\n== adjacent-wave stabilities (lag-1 pearson r) ==\n")
stab_tab <- do.call(rbind, lapply(
  c(bmi = "bmi", pa = "pa", fv = "fv"), stability
))
print(round(stab_tab, 3))
for (v in rownames(stab_tab)) {
  cat(sprintf(
    "%s: mean adjacent-wave r = %.3f (range %.3f to %.3f)\n",
    v, stab_tab[v, "mean"], stab_tab[v, "lo"], stab_tab[v, "hi"]
  ))
}
utils::write.csv(
  data.frame(construct = rownames(stab_tab), stab_tab, row.names = NULL),
  file.path("output", "adjacent_wave_stability.csv"), row.names = FALSE
)
