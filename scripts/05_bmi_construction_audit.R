#!/usr/bin/env Rscript
# audit the construction-stage bmi processing: compare the bmi carried by the
# frozen merged panel against a raw, uncleaned recomputation from the per-wave
# health modules (chXXa016 height in cm, chXXa017 weight in kg), waves 1 to 7.
#
# categories per person-wave:
#   identical        |bmi_merged - bmi_raw| < 1e-6
#   negligible_diff  absolute difference below 0.01 kg/m2 (storage rounding)
#   value_changed    absolute difference of 0.01 kg/m2 or more (captures the
#                    unit-error corrections, person-level height smoothing, and
#                    weight-outlier repairs of the construction record)
#   merged_only      merged bmi present where the raw items yield none, i.e. a
#                    missing or unusable input was filled at construction
#   raw_only         raw items yield a value but the merged panel carries no
#                    bmi; diagnosed below into rows the merged panel contains
#                    with bmi missing versus person-waves absent from the
#                    merged panel altogether
#   both_missing     no bmi from either source
#
# outputs:
#   output/bmi_construction_audit_summary.csv     per-wave counts (releasable)
#   output/bmi_construction_audit_magnitudes.csv  size bands of value_changed
#   output/bmi_construction_audit_rawonly.csv     raw_only diagnosis by wave
#   data/bmi_construction_flags.csv               affected person-waves only;
#                                                 stays local (data/* ignored)
#
# run from the compendium root; needs liss_merged_long.sav and the raw health
# modules ch07a to ch13g in data/ or LISS_DATA_DIR. the raw recomputation
# applies no plausibility handling by design: this script counts construction
# effects, it does not judge them.

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

wave_prefixes <- c(
  ch07a = 1, ch08b = 2, ch09c = 3, ch10d = 4, ch11e = 5, ch12f = 6, ch13g = 7
)

# 1. merged bmi, analysis waves only ------------------------------------------
merged <- read_liss("liss_merged_long.sav")
stopifnot(all(c("nomem_encr", "wavenr", "bmi") %in% names(merged)))
merged <- merged[merged$wavenr %in% unname(wave_prefixes),
                 c("nomem_encr", "wavenr", "bmi")]
merged$nomem_encr <- as.numeric(merged$nomem_encr)
merged$wavenr <- as.numeric(merged$wavenr)
merged$bmi_merged <- as.numeric(merged$bmi)
merged$bmi <- NULL
merged$in_merged <- TRUE

# 2. raw recomputation from the per-wave health modules -----------------------
raw <- purrr::imap_dfr(wave_prefixes, function(w, prefix) {
  f <- list.files(liss_data_dir(), pattern = paste0("^", prefix, ".*\\.sav$"),
    full.names = TRUE
  )
  stopifnot(length(f) == 1)
  d <- haven::read_sav(f)
  cm_col <- grep(paste0("^", prefix, "016$"), names(d), value = TRUE)
  kg_col <- grep(paste0("^", prefix, "017$"), names(d), value = TRUE)
  stopifnot(length(cm_col) == 1, length(kg_col) == 1)
  dplyr::tibble(
    nomem_encr = as.numeric(d$nomem_encr),
    wavenr = w,
    cm = suppressWarnings(as.numeric(d[[cm_col]])),
    kg = suppressWarnings(as.numeric(d[[kg_col]]))
  )
})
raw$bmi_raw <- ifelse(!is.na(raw$kg) & !is.na(raw$cm) & raw$cm > 0,
  raw$kg / (raw$cm / 100)^2, NA_real_
)

# 3. join and categorise ------------------------------------------------------
j <- dplyr::full_join(
  merged, raw[c("nomem_encr", "wavenr", "bmi_raw")],
  by = c("nomem_encr", "wavenr")
)
j$in_merged[is.na(j$in_merged)] <- FALSE
adiff <- abs(j$bmi_merged - j$bmi_raw)
j$category <- dplyr::case_when(
  is.na(j$bmi_merged) & is.na(j$bmi_raw) ~ "both_missing",
  is.na(j$bmi_raw) ~ "merged_only",
  is.na(j$bmi_merged) ~ "raw_only",
  adiff < 1e-6 ~ "identical",
  adiff < 0.01 ~ "negligible_diff",
  TRUE ~ "value_changed"
)

# 4. per-wave summary (releasable aggregate) ----------------------------------
tab <- table(wavenr = j$wavenr, category = j$category)
summary_tab <- as.data.frame.matrix(tab)
summary_tab <- cbind(wavenr = rownames(summary_tab), summary_tab)
total <- summary_tab[1, , drop = FALSE]
total[1, 1] <- "total"
total[1, -1] <- as.list(colSums(summary_tab[, -1, drop = FALSE]))
summary_tab <- rbind(summary_tab, total)
utils::write.csv(summary_tab,
  file.path("output", "bmi_construction_audit_summary.csv"),
  row.names = FALSE
)

cat("== bmi construction audit, waves 1 to 7 ==\n")
print(summary_tab, row.names = FALSE)

n_present <- sum(!is.na(j$bmi_merged))
n_changed <- sum(j$category == "value_changed")
n_filled <- sum(j$category == "merged_only")
n_removed <- sum(j$category == "raw_only")
cat(sprintf(
  paste0(
    "\nperson-waves with a merged bmi: %d\n",
    "  altered at construction (value_changed): %d (%.2f%%)\n",
    "  filled at construction (merged_only):    %d (%.2f%%)\n",
    "  raw value not carried over (raw_only):   %d\n"
  ),
  n_present, n_changed, 100 * n_changed / n_present,
  n_filled, 100 * n_filled / n_present, n_removed
))

# 5. magnitude bands within value_changed -------------------------------------
# below 0.5 kg/m2: consistent with person-level height smoothing of reporting
# noise; 0.5 to 2: larger smoothing and weight repairs; 2 and above: unit-error
# corrections of implausible raw entries
if (n_changed > 0) {
  d_ch <- adiff[j$category == "value_changed"]
  bands <- cut(d_ch,
    breaks = c(0, 0.5, 2, Inf), right = FALSE,
    labels = c("below_0.5", "0.5_to_2", "2_and_above")
  )
  band_tab <- as.data.frame(table(band = bands))
  band_tab$share <- round(band_tab$Freq / sum(band_tab$Freq), 4)
  utils::write.csv(band_tab,
    file.path("output", "bmi_construction_audit_magnitudes.csv"),
    row.names = FALSE
  )
  cat("\nabsolute difference among value_changed (kg/m2):\n")
  print(band_tab, row.names = FALSE)
  print(round(stats::quantile(d_ch, probs = c(0.5, 0.75, 0.9, 0.99, 1)), 3))
}

# 6. raw_only diagnosis --------------------------------------------------------
# split the raw-but-not-merged person-waves into rows the merged panel carries
# with bmi missing (a value was dropped or not attached during construction,
# e.g. the background inner join) versus person-waves absent from the merged
# panel altogether
ro <- j[j$category == "raw_only", ]
ro_tab <- as.data.frame.matrix(table(
  wavenr = ro$wavenr,
  merged_row = ifelse(ro$in_merged, "row_present_bmi_missing", "no_merged_row")
))
ro_tab <- cbind(wavenr = rownames(ro_tab), ro_tab)
utils::write.csv(ro_tab,
  file.path("output", "bmi_construction_audit_rawonly.csv"),
  row.names = FALSE
)
cat("\n== raw_only diagnosis (why a raw bmi did not reach the merged panel) ==\n")
print(ro_tab, row.names = FALSE)

# 7. person-wave flags for the sensitivity refit (local only) -----------------
flags <- j[j$category %in% c("value_changed", "merged_only", "raw_only"),
           c("nomem_encr", "wavenr", "category")]
utils::write.csv(flags,
  file.path(liss_data_dir(), "bmi_construction_flags.csv"),
  row.names = FALSE
)
cat(sprintf(
  paste0(
    "\nwrote the three audit CSVs to output/ (releasable) and\n",
    "%s (local only; %d rows).\n",
    "the sensitivity exclusion set is category value_changed plus merged_only.\n"
  ),
  file.path(liss_data_dir(), "bmi_construction_flags.csv"), nrow(flags)
))
