#!/usr/bin/env Rscript
# wave-based sample selection with weasel, standalone
# the same rule runs inside the pipeline (cb112) with an equality assertion;
# this script exposes the full audit trail on its own

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

liss <- read_liss("liss_merged_long.sav")

analysis_waves <- 1:7
min_waves <- 3L
pa_ceiling <- 40

frame <- liss %>%
  dplyr::transmute(
    id = nomem_encr, t = wavenr,
    bmi,
    fv = frve,
    pa = dplyr::if_else(sport > pa_ceiling, NA_real_, sport),
    female = gender - 1,
    age = leeftijd
  )

# observed at a wave = at least one focal measure reported at that wave
presence <- frame %>%
  dplyr::filter(
    t %in% analysis_waves,
    !is.na(bmi) | !is.na(fv) | !is.na(pa)
  )

ws_plan <- weasel::weasel_plan(
  presence[c("id", "t", "female", "age", "bmi", "fv", "pa")],
  id = "id", wave = "t", span = "full",
  scenarios = data.frame(
    scenario = "min3_of7",
    require_endpoints = FALSE,
    max_missing = length(analysis_waves) - min_waves,
    n_gap_max = 5L,
    max_gap_len = 5L
  )
)
print(ws_plan)

weasel::weasel_print_table(
  weasel::weasel_sensitivity(
    ws_plan,
    require_endpoints = FALSE, max_missing = 0:6,
    n_gap_max = 5L, max_gap_len = 5L
  ),
  title = "sample size by minimum-wave tolerance"
)
weasel::weasel_print_table(
  weasel::weasel_selectivity(ws_plan, "min3_of7"),
  title = "retained vs excluded respondents"
)

kept <- unique(weasel::weasel_apply(ws_plan, "min3_of7")$id)
utils::write.csv(
  data.frame(id = kept), file.path("output", "sample_ids.csv"),
  row.names = FALSE
)
writeLines(
  weasel::weasel_justify_subset(ws_plan, "min3_of7"),
  file.path("output", "sample_selection_justification.txt")
)
message(length(kept), " respondents retained; justification written to output/")
