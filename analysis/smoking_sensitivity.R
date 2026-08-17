# smoking_sensitivity.R
# smoking-adjusted sensitivity analysis for the grouped ri-clpm
#
# purpose: refit the covariate-augmented three-group ri-clpm with baseline
# smoking added to female, medication, and age as a time-invariant predictor of
# the random intercepts, and re-run the focal moderation test in that model, so
# the manuscript can state empirically that the focal conclusions do not depend
# on the exclusion of smoking.
#
# how to run: source this file at the end of an analysis session in which
# analysis/run_all.Rmd (or analysis/reciprocal-health-dynamics.R) has already
# been executed, so that the objects `liss` (long panel with the `smoke`
# column), `wide` (analysis frame), `ri_clpm_syntax`, and `all_model_fits`
# exist. from the compendium root:
#   Rscript -e 'rmarkdown::render("analysis/run_all.Rmd")'  # or your usual run
#   source("analysis/smoking_sensitivity.R")
# outputs are written next to the released tables in tables/.

# ---- 1. baseline smoking at each respondent's first observed wave ----

# the frozen merged panel carries `smoke` (vignette 02); the analysis subset
# drops it, so recover it from the long object and take the value at the first
# wave with a non-missing report, mirroring how age, gender, and medication are
# anchored to the first observed wave
stopifnot(exists("liss"), exists("wide"), exists("ri_clpm_syntax"))
id_col   <- intersect(c("id", "nomem_encr"), names(liss))[1]
wave_col <- intersect(c("t", "wavenr", "w"), names(liss))[1]
stopifnot(!is.na(id_col), !is.na(wave_col), "smoke" %in% names(liss))

smoke_first <- liss[!is.na(liss[["smoke"]]), c(id_col, wave_col, "smoke")]
smoke_first <- smoke_first[order(smoke_first[[id_col]], smoke_first[[wave_col]]), ]
smoke_first <- smoke_first[!duplicated(smoke_first[[id_col]]), c(id_col, "smoke")]
names(smoke_first) <- c("id", "smoke")

# coerce to a 0/1 current-smoker indicator; the frozen panel codes this as a
# cleaned binary, but guard against 1/2 (yes/no) survey coding
tab <- sort(unique(smoke_first$smoke))
if (identical(tab, c(1, 2))) smoke_first$smoke <- 2 - smoke_first$smoke
stopifnot(all(smoke_first$smoke %in% 0:1))
cat("baseline smoking distribution:\n")
print(table(smoke_first$smoke, useNA = "ifany"))

wide_s <- dplyr::left_join(wide, smoke_first, by = "id")
cat(sprintf(
  "smoking available for %d of %d respondents (%.1f%%); missing handled by fiml\n",
  sum(!is.na(wide_s$smoke)), nrow(wide_s), 100 * mean(!is.na(wide_s$smoke))
))

# ---- 2. covariate model with smoking added ----

# mirror the pipeline's per-covariate blocks (riBMI/riPA/riFV ~ x)
covar_blocks <- "
riBMI ~ female
riPA ~ female
riFV ~ female
riBMI ~ med
riPA ~ med
riFV ~ med
riBMI ~ age
riPA ~ age
riFV ~ age
riBMI ~ smoke
riPA ~ smoke
riFV ~ smoke
"
ri_clpm_smoke_syntax <- paste0(ri_clpm_syntax, "\n", covar_blocks)

fit_smoke <- lavaan::lavaan(
  model = ri_clpm_smoke_syntax,
  data = wide_s,
  group = "ses",
  group.label = c("ses1", "ses2", "ses3"),
  int.ov.free = TRUE,
  meanstructure = TRUE,
  estimator = "MLR",
  se = "robust",
  missing = "fiml",
  fixed.x = TRUE
)
cat("\nsmoke-augmented grouped ri-clpm converged:", lavaan::lavInspect(fit_smoke, "converged"), "\n")
print(lavaan::fitMeasures(fit_smoke, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr", "aic", "bic")))

# ---- 3. focal between-person associations under smoking adjustment ----

pe <- lavaan::parameterEstimates(fit_smoke, standardized = TRUE)
between <- pe[pe$op == "~~" & grepl("^ri", pe$lhs) & grepl("^ri", pe$rhs) & pe$lhs != pe$rhs, ]
utils::write.csv(between, "tables/smoke_sensitivity_between.csv", row.names = FALSE)

std <- lavaan::standardizedSolution(fit_smoke, level = 0.95)
focal <- std[std$op == "~~" &
  ((std$lhs == "riBMI" & std$rhs == "riPA") | (std$lhs == "riPA" & std$rhs == "riBMI")),
  c("group", "est.std", "se", "ci.lower", "ci.upper", "pvalue")]
focal$SES <- factor(focal$group, levels = 1:3, labels = c("ses1", "ses2", "ses3"))
utils::write.csv(focal, "tables/smoke_sensitivity_focal_by_ses.csv", row.names = FALSE)
cat("\nstandardised trait-level bmi-pa association by ses, smoking-adjusted:\n")
print(focal[, c("SES", "est.std", "ci.lower", "ci.upper", "pvalue")], digits = 3)

# ---- 4. moderation test within the smoke-augmented model ----

ri_clpm_smoke_syntax_eq <- sub(
  "riBMI ~~ riPA", "riBMI ~~ c(rc, rc, rc)*riPA",
  ri_clpm_smoke_syntax, fixed = TRUE
)
fit_smoke_eq <- lavaan::lavaan(
  model = ri_clpm_smoke_syntax_eq,
  data = wide_s,
  group = "ses",
  group.label = c("ses1", "ses2", "ses3"),
  int.ov.free = TRUE,
  meanstructure = TRUE,
  estimator = "MLR",
  se = "robust",
  missing = "fiml",
  fixed.x = TRUE
)
lrt <- lavaan::lavTestLRT(
  fit_smoke, fit_smoke_eq,
  method = "satorra.2000",
  model.names = c("riBMI~~riPA free", "riBMI~~riPA equal")
)
lrt_df <- cbind(model = rownames(lrt), as.data.frame(lrt))
utils::write.csv(lrt_df, "tables/smoke_sensitivity_moderation_lrt.csv", row.names = FALSE)
cat("\nmoderation test under smoking adjustment (scaled lrt):\n")
print(lrt_df, digits = 3)

# ---- 5. smoking's own covariate effects, for completeness ----

smoke_eff <- std[std$op == "~" & std$rhs == "smoke",
  c("group", "lhs", "est.std", "se", "ci.lower", "ci.upper", "pvalue")]
utils::write.csv(smoke_eff, "tables/smoke_covariate_effects.csv", row.names = FALSE)
cat("\nstandardised smoking effects on the random intercepts by ses group:\n")
print(smoke_eff, digits = 3)

# ---- 6. side-by-side comparison with the no-smoke covariate model ----

if (!is.null(all_model_fits$grouped$ri_clpm_grouped_covar_fit)) {
  std0 <- lavaan::standardizedSolution(all_model_fits$grouped$ri_clpm_grouped_covar_fit)
  focal0 <- std0[std0$op == "~~" &
    ((std0$lhs == "riBMI" & std0$rhs == "riPA") | (std0$lhs == "riPA" & std0$rhs == "riBMI")),
    c("group", "est.std")]
  cmp <- merge(focal0, focal[, c("group", "est.std")], by = "group",
    suffixes = c("_no_smoke", "_smoke_adjusted"))
  utils::write.csv(cmp, "tables/smoke_sensitivity_comparison.csv", row.names = FALSE)
  cat("\nfocal standardised association, covariate model without vs with smoking:\n")
  print(cmp, digits = 3)
}

# ---- 7. reporting guidance ----
# if the pattern reproduces (negative, closely similar per-stratum associations;
# non-significant scaled lrt; equal model preferred), add one sentence to
# section 4.8 alongside the covariate check, e.g.: "Fourth, adding baseline
# smoking status to the covariates left the between-person association pattern,
# including the absence of detectable SES moderation, unchanged (Supplementary
# Material S6)." then update the final clause of the smoking paragraph in
# section 3.2 from an identified check to a reported one, and export these
# tables with the released set so s6 carries them.
