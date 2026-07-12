#!/usr/bin/env Rscript
# verify the published pipeline under lissr 1.4.0 and weasel 0.4.0
#
# run from the compendium root with the frozen data in place:
#   cd "/path/to/diet-activity-bmi-riclpm" && Rscript scripts/verify_updated_packages.R
#
# the script re-executes the two package touchpoints of the published
# pipeline (cb51 equivalisation cross-check, cb112 weasel selection audit)
# against the frozen liss_merged_long.sav, decides the two empirical
# conditions the sandbox audit could not observe (zero-adult household
# compositions; duplicated id-wave pairs), and diffs every artifact that
# is expected to change against its committed 0.3.1-era counterpart.
# runtime is seconds; no models are fitted.

library(magrittr)

fail <- character(0)
note <- character(0)
say <- function(...) cat(paste0(..., collapse = ""), "\n", sep = "")
result <- function(id, ok, msg) {
  say(sprintf("[%s] %s: %s", if (isTRUE(ok)) "PASS" else "FAIL", id, msg))
  if (!isTRUE(ok)) fail <<- c(fail, id)
}

# ---- 0. environment ---------------------------------------------------------
if (!file.exists("DESCRIPTION") || !dir.exists("analysis")) {
  stop("run this script from the compendium root (diet-activity-bmi-riclpm)",
       call. = FALSE)
}
v_lissr <- as.character(utils::packageVersion("lissr"))
v_weasel <- as.character(utils::packageVersion("weasel"))
say("lissr ", v_lissr, " | weasel ", v_weasel, " | ", R.version.string)
if (utils::packageVersion("lissr") < "1.4.0" ||
    utils::packageVersion("weasel") < "0.4.0") {
  say("note: this script targets lissr >= 1.4.0 and weasel >= 0.4.0; ",
      "update the packages first (remotes::install_github)")
}

invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE),
                 source))
liss <- read_liss("liss_merged_long.sav")

# ---- 1. condition c1: zero-adult household compositions ---------------------
# lissr 1.4.0 returns NA for compositions with fewer than one adult
# (aantalhh - aantalki < 1), where 1.3.2 and the in-line formula return a
# finite value; count the rows this affects among rows the cross-check sees
hh <- as.numeric(liss$aantalhh)
ki <- as.numeric(liss$aantalki)
bad_comp <- is.finite(hh) & is.finite(ki) & (hh < 1 | ki < 0 | (hh - ki) < 1)
n_bad <- sum(bad_comp, na.rm = TRUE)
n_bad_finite_inc <- sum(bad_comp & is.finite(as.numeric(liss$nethh)),
                        na.rm = TRUE)
say(sprintf("c1: %d row(s) with an invalid (zero-adult) composition, %d of them with finite nethh",
            n_bad, n_bad_finite_inc))
if (n_bad_finite_inc > 0) {
  note <- c(note, sprintf(
    "c1: %d row(s) leave the cb51 cross-check under 1.4.0 (they keep their in-line stand_inc; only the lissr-side verification narrows); consider the strengthened assertion below",
    n_bad_finite_inc))
}

# ---- 2. cb51 cross-check, verbatim and strengthened -------------------------
stand_inc <- liss$nethh / ((hh - ki + 0.8 * ki)^0.5)
eq <- lissr::liss_equivalise_income(liss$nethh, liss$aantalhh, liss$aantalki,
                                    verbose = FALSE)
ok <- is.finite(stand_inc) & is.finite(eq)
result("cb51-verbatim", isTRUE(all.equal(stand_inc[ok], eq[ok])),
       sprintf("pipeline assertion over %d finite pairs", sum(ok)))

# strengthened form: lissr may differ from the in-line formula only on
# invalid compositions, and there only by returning NA
divergent <- which(is.finite(stand_inc) & !is.finite(eq))
result("cb51-strict",
       all(divergent %in% which(bad_comp)) &&
         !any(is.finite(eq) & !is.finite(stand_inc) & !is.na(liss$nethh)),
       sprintf("%d divergent row(s), all attributable to the 1.4.0 composition guard",
               length(divergent)))

# ---- 3. condition c2 and the cb112 selection audit --------------------------
analysis_waves <- 1:7
min_waves <- 3L
pa_ceiling <- 40
frame <- data.frame(
  id = liss$nomem_encr, t = liss$wavenr, bmi = liss$bmi, fv = liss$frve,
  pa = ifelse(liss$sport > pa_ceiling, NA_real_, liss$sport),
  female = liss$gender - 1, age = liss$leeftijd
)
presence <- frame[frame$t %in% analysis_waves &
                    (!is.na(frame$bmi) | !is.na(frame$fv) | !is.na(frame$pa)), ]

n_dup <- sum(duplicated(presence[c("id", "t")]))
result("c2-duplicates", n_dup == 0,
       sprintf("%d duplicated (id, t) pair(s) in the presence frame (0 keeps weasel_selectivity identical to 0.3.1)",
               n_dup))

# pipeline rule (cb112), base-r form
waves_per_id <- tapply(presence$t, presence$id, function(x) length(unique(x)))
rule_ids <- as.numeric(names(waves_per_id)[waves_per_id >= min_waves])

# weasel path, collecting classed warnings
warnings_seen <- character(0)
ws_plan <- withCallingHandlers(
  weasel::weasel_plan(
    presence[c("id", "t", "female", "age", "bmi", "fv", "pa")],
    id = "id", wave = "t", span = "full",
    scenarios = data.frame(
      scenario = "min3_of7", require_endpoints = FALSE,
      max_missing = length(analysis_waves) - min_waves,
      n_gap_max = 5L, max_gap_len = 5L
    )
  ),
  warning = function(w) {
    warnings_seen <<- c(warnings_seen, paste(class(w)[1], conditionMessage(w)))
    invokeRestart("muffleWarning")
  }
)
kept <- unique(weasel::weasel_apply(ws_plan, "min3_of7")$id)
result("cb112-setequal", setequal(kept, rule_ids),
       sprintf("weasel selects %d ids; dplyr-equivalent rule selects %d ids",
               length(kept), length(rule_ids)))
result("cb112-n", length(kept) == 5676,
       sprintf("retained n = %d (published: 5676)", length(kept)))

# ---- 4. sensitivity sweep vs the published transcript -----------------------
sens <- withCallingHandlers(
  weasel::weasel_sensitivity(ws_plan, require_endpoints = FALSE,
                             max_missing = 0:6, n_gap_max = 5L,
                             max_gap_len = 5L),
  warning = function(w) {
    warnings_seen <<- c(warnings_seen, paste(class(w)[1], conditionMessage(w)))
    invokeRestart("muffleWarning")
  }
)
published_n <- c(2396, 3250, 4222, 4965, 5676, 7097, 7250)
result("sensitivity-values",
       identical(as.integer(sens$n_ids[order(sens$max_missing)]),
                 as.integer(published_n)),
       paste("n_ids by tolerance:", paste(sens$n_ids, collapse = " ")))
result("sensitivity-colname", "max_gap_len" %in% names(sens) &&
         !("max_gap_max" %in% names(sens)),
       "output column renamed max_gap_max -> max_gap_len (expected under 0.4.0)")

# ---- 5. selectivity vs the published transcript -----------------------------
sel <- weasel::weasel_selectivity(ws_plan, "min3_of7")
pub <- data.frame(
  variable = c("age", "bmi", "fv", "female", "pa"),
  smd = c(0.217, 0.132, 0.064, -0.014, -0.011)
)
m <- merge(sel[c("variable", "smd")], pub, by = "variable",
           suffixes = c("_new", "_pub"))
result("selectivity-smd", all(abs(m$smd_new - m$smd_pub) < 6e-4),
       paste(sprintf("%s %.3f/%.3f", m$variable, m$smd_new, m$smd_pub),
             collapse = "  "))

# ---- 6. justification text vs the committed artifact ------------------------
new_txt <- weasel::weasel_justify_subset(ws_plan, "min3_of7")
old_txt <- paste(readLines(file.path("output",
                                     "sample_selection_justification.txt")),
                 collapse = " ")
old_txt <- trimws(gsub("\\s+", " ", old_txt))
new_cmp <- trimws(gsub("\\s+", " ", new_txt))
pop_sentence <- regmatches(
  new_cmp, regexpr("The planning population comprised [^.]*population\\.",
                   new_cmp))
stripped <- if (length(pop_sentence) == 1) {
  trimws(gsub("\\s+", " ", sub(paste0(" ", pop_sentence), "", new_cmp,
                               fixed = TRUE)))
} else {
  new_cmp
}
result("justify-diff", identical(new_cmp, old_txt) || identical(stripped, old_txt),
       "committed text matches the 0.4.0 output (directly, or as 0.3.1 plus the population sentence)")

# ---- 7. deprecation warnings inventory --------------------------------------
say("weasel warnings raised during the run:")
if (length(warnings_seen)) {
  for (w in unique(warnings_seen)) say("  - ", w)
} else {
  say("  (none)")
}
result("warnings-expected",
       sum(grepl("weasel_deprecated", warnings_seen)) == 0,
       "no deprecation warnings expected after the max_gap_len migration")

# ---- 8. secondary checks (qc items q7, q8) ----------------------------------
n_na_hh <- sum(is.na(liss$nohouse_encr[liss$wavenr %in% analysis_waves]))
say(sprintf("q8: %d row(s) with missing nohouse_encr in waves 1-7 (NA rows pool into one pseudo-household in cb55)",
            n_na_hh))

# ---- verdict ----------------------------------------------------------------
say(strrep("-", 72))
if (length(fail) == 0) {
  say("verdict: full agreement; the updated packages reproduce the published")
  say("selection and equivalisation exactly; only the documented textual and")
  say("column-name changes remain (see the revision plan).")
} else {
  say("verdict: DISCREPANCIES in ", paste(fail, collapse = ", "),
      "; see lines above and the revision plan's contingency section.")
}
for (n in note) say("note: ", n)

# suggested strengthened cb51 assertion (drop-in replacement for the
# stopifnot in analysis/reciprocal-health-dynamics.R, chunk cb51):
#   eq <- lissr::liss_equivalise_income(liss$nethh, liss$aantalhh,
#                                       liss$aantalki, verbose = FALSE)
#   ok <- is.finite(liss$stand_inc) & is.finite(eq)
#   stopifnot(
#     isTRUE(all.equal(liss$stand_inc[ok], eq[ok])),
#     all(which(is.finite(liss$stand_inc) & !is.finite(eq)) %in%
#           which(liss$aantalhh < 1 | liss$aantalki < 0 |
#                   (liss$aantalhh - liss$aantalki) < 1))
#   )
