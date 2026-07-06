# ---- cb1 ---- load helpers and the merged LISS panel
# source helper functions from R/ and attach the pipe
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))
library(magrittr)

# load merged and cleaned LISS data
liss <- read_liss("liss_merged_long.sav")

# ---- cb51 ---- equivalised household income
liss %<>%
  dplyr::mutate(
    stand_inc = nethh / ((aantalhh - aantalki + 0.8 * aantalki)^0.5)
  )

# cross-check: lissr's weighted_sqrt scale reproduces this equivalisation
if (requireNamespace("lissr", quietly = TRUE)) {
  eq <- lissr::liss_equivalise_income(
    liss$nethh, liss$aantalhh, liss$aantalki, verbose = FALSE
  )
  ok <- is.finite(liss$stand_inc) & is.finite(eq)
  stopifnot(isTRUE(all.equal(liss$stand_inc[ok], eq[ok])))
}

# ---- cb52 ---- per-wave modal income
liss %<>%
  dplyr::group_by(wavenr) %>%
  dplyr::mutate(
    modaal_per_wave =
      mean(stand_inc, na.rm = TRUE) * 0.79
  ) %>%
  dplyr::ungroup()

# ---- cb53 ---- above-modal income indicator
liss %<>%
  dplyr::mutate(
    stand_inc_sqrt = sqrt(stand_inc),
    boven_modaal = dplyr::case_when(
      !is.na(stand_inc_sqrt) &
        stand_inc_sqrt < sqrt(modaal_per_wave) ~ 0,
      stand_inc_sqrt >= sqrt(modaal_per_wave) ~ 1,
      TRUE ~ NA_real_
    )
  )

# ---- cb54 ---- recode education into four levels
liss$oplmet <- type.convert(liss$oplmet, as.is = TRUE)
liss %<>%
  dplyr::mutate(
    educ = dplyr::case_when(
      oplmet == 1 ~ 1, # primary school
      oplmet == 2 ~ 2, # vmbo (intermediate sec. edc.)
      oplmet == 3 ~ 2, # havo/vwo (higher sec./preparatory uni. educ.)
      oplmet == 4 ~ 3, # mbo (intermediate vocational educ.
      oplmet == 5 ~ 4, # hbo (higher vocational educ.)
      oplmet == 6 ~ 4, # wo (university)
      oplmet == 7 ~ 1, # other
      oplmet == 8 ~ 1, # not yet completed any educ.
      oplmet == 9 ~ 1, # not yet started any educ.
      TRUE ~ NA_real_
    )
  )

# ---- cb55 ---- highest household education per wave
liss %<>%
  dplyr::arrange(wavenr) %>%
  dplyr::group_by(nohouse_encr, wavenr) %>%
  dplyr::mutate(
    highest_educ_in_hh = ifelse(any(!is.na(educ)),
      max(educ, na.rm = TRUE), NA_real_
    )
  ) %>%
  dplyr::ungroup()

# ---- cb56 ---- derive the three-level SES grouping
liss %<>%
  dplyr::mutate(
    hh_educ_3_groups = dplyr::if_else(
      highest_educ_in_hh == 1, 2,
      highest_educ_in_hh
    ) - 1,
    # ses: household-maximum education in three levels; ses1 to ses3 denote
    # increasing educational attainment
    ses = hh_educ_3_groups
  )

# ---- cb57 ---- select analysis columns and describe
liss$female <- liss$gender - 1
liss <- liss[c(
  "nomem_encr",
  "wavenr",
  "nohouse_encr",
  "female",
  "ses",
  "boven_modaal",
  "hh_educ_3_groups",
  "hhinc",
  "stand_inc",
  "aantalhh",
  "aantalki",
  "stand_inc_sqrt",
  "modaal_per_wave",
  "highest_educ_in_hh",
  "oplmet",
  "bmi",
  "frve",
  "sport",
  "medicine",
  "smoke",
  "leeftijd"
)]
describe(liss, exclude = c("nomem_encr", "nohouse_encr"))

# ---- cb106 ---- wave-scope helper functions
check_scope <- function(kill = FALSE) {
  # checks that scope exists
  if (!exists("weasel_env", globalenv(), mode = "environment")) {
    msg <- paste(
      "No scope has been set. Please use the", dQuote("scope()"),
      "function to define a scope before proceeding."
    )
    if (kill) stop(msg, call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}
create_scope <- function(data,
                         id,
                         wave,
                         size = NULL,
                         lower = NULL,
                         upper = NULL,
                         gap = 0,
                         n_gap = 0,
                         override = TRUE) {
  # create analysis scope
  scope_exists <- check_scope(kill = FALSE)
  if (scope_exists & !override) {
    yes <- utils::askYesNo("Override current scope?")
  } else {
    yes <- TRUE
  }
  if (yes) {
    mget(names(formals()), envir = environment()) %>%
      list2env() %>%
      assign(x = "weasel_env", envir = globalenv())
  }
}

if_null_then <- function(x, new) {
  # replace NULL with default value
  if (length(x) == 0 || is.null(x)) {
    new
  } else {
    x
  }
}

bounds_to_seq <- function(x) {
  # create sequence from range
  range(x, na.rm = TRUE) %>%
    sequence(nvec = diff(.) + 1)
}

eval_scope <- function() {
  # evaluate scope parameters
  with(weasel_env, {
    bounds <- bounds_to_seq(data[[wave]])
    size %<>% `[`(. >= 3) %<>%
      if_null_then(3:max(bounds, na.rm = TRUE))
    lower %<>% if_null_then(head(bounds, 1))
    upper %<>% if_null_then(tail(bounds, 1))
    n_gap <- ifelse(gap > 0, 1, 0)
  })
}

make_set <- function() {
  # generate all wave combinations
  check_scope(kill = TRUE)
  with(weasel_env, {
    set <- lapply(size, combn,
      x = c(lower:upper), simplify = FALSE
    ) %>%
      unlist(recursive = FALSE)
  })
}

as_string <- function(x, na.rm = FALSE) {
  # collapse vector to space-separated string
  `if`(na.rm, x[!is.na(x)], x) %>%
    unlist() %>%
    paste0(collapse = " ")
}

as_sequence <- function(x) {
  # convert string to numeric sequence
  if (is.character(x)) {
    suppressWarnings(as.numeric(strsplit(x, " ")[[1]]))
  } else {
    x
  }
}

gap_to_na <- function(x) {
  # replace missing values with NA
  x[match(bounds_to_seq(x), x, nomatch = NA)]
}

complete_seq <- function(x) {
  # replace missing values in sequence to make it complete
  s <- as_sequence(x)
  bounds_to_seq(s) -> r
  lapply(list(replace(
    r,
    which(is.na(s)), NA
  ), s), paste0, collapse = " ") %>%
    Reduce(f = identical) %>%
    `if`(r, s)
}

count_gaps <- function(x) {
  # count gaps in a sequence
  g <- rle(is.na(gap_to_na(x)))
  c(gap = max(c(0, g$lengths[g$values])), n_gap = sum(g$values))
}

filter_gaps <- function(s, r) {
  # filter wave combinations by gap criteria
  x <- count_gaps(s)
  if ((x[1] <= r[1]) && (x[2] <= r[2])) {
    return(gap_to_na(s))
  }
}

filter_set <- function() {
  # filter wave combinations
  with(weasel_env, {
    refs <- c(gap, n_gap)
    set <- Filter(lapply(set, filter_gaps, refs), f = length)
  })
}

max0 <- function(x) {
  # replace 0 with NA
  if (length(x[!is.na(x)]) == 0) NA else max(x, na.rm = TRUE)
}

pivot <- function() {
  # pivot data frame
  check_scope(kill = TRUE)
  cli::cli_alert_info("Gathering data matching scope criteria.")
  with(weasel_env, {
    pivot <- dplyr::select(data, !!id, !!wave) %>%
      tidyr::pivot_wider(names_from = !!wave, values_from = !!wave) %>%
      dplyr::group_split(dplyr::across(!!id), .keep = TRUE) %>%
      purrr::map_df(~ purrr::map_if(.x, is.numeric, max0)) %>%
      `[`(c(1, order(as.numeric(names(.)[-1])) + 1)) %>%
      dplyr::slice(which(rowSums(!is.na(dplyr::select(., -1))) >= 3))
  })
}

build_view <- function() {
  # build summary
  cli::cli_alert_info("Creating summary view.")
  with(weasel_env, {
    p <- pivot[, !colnames(pivot) %in% c(id, "waves")]
    s <- vapply(set, as_string, character(1)) %>% unique()
    pivot$waves <- vapply(asplit(p, 1), as_string, character(1))
    pivot %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(waves = stringr::str_extract_all(waves, s) %>%
        Filter(f = length) %>% list()) %>%
      dplyr::ungroup()

    view <- dplyr::pull(pivot, waves) %>%
      unlist() %>%
      table() %>%
      stack() %>%
      dplyr::mutate(
        n = stringr::str_count(ind, "[0-9]+|NA"),
        ids = values, waves = ind
      ) %>%
      `[`(c("waves", "n", "ids"))

    view %<>% as.data.frame() %>%
      format(width = 5) %>%
      dplyr::arrange(desc(n)) %>%
      data.table::as.data.table()
  })
}

filter_view <- function(n_range = NULL, ids_range = NULL) {
  # filtering view
  filter_data <- function(x, n, ids) {
    .f <- function(x, y, z) {
      dplyr::filter(x, as.numeric(x[[y]]) %in% bounds_to_seq(z))
    }
    `if`(length(n) >= 2, .f(x, "n", n), x) -> v
    `if`(length(ids) >= 2, .f(v, "ids", ids), v)
  }
  fdf <- filter_data(with(weasel_env, view), n_range, ids_range)
  fdf[order(fdf$ids, decreasing = TRUE), ]
}

get_row <- function(i = NULL) {
  # get subset of data for a view row
  if (is.null(i)) {
    cli::cli_alert_warning(
      "No row selected, returning entire view."
    )
    return(weasel_env$view)
  }
  assign("row", ifelse(is.null(i), 1, i),
    envir = get("weasel_env", mode = "environment")
  )
  with(weasel_env, {
    t_to_keep <- stringr::str_squish(
      view[row, waves]
    ) %>% list(as_sequence(.))
    ids_to_keep <- pivot[pivot$waves %>%
      sapply(function(r) any(t_to_keep[[1]] %in% r)), ]$id

    tbl_from_row <- dplyr::filter(
      data,
      id %in% ids_to_keep & t %in% t_to_keep[[2]]
    )
    return(tbl_from_row)
  })
}

# ---- cb107 ---- rename long-frame columns
liss %<>%
  dplyr::rename(
    id = nomem_encr,
    t = wavenr,
    pa = sport,
    fv = frve,
    female = female,
    age = leeftijd,
    med = medicine
  )
describe(
  liss,
  exclude = c("id", "nohouse_encr"),
  caption = "Descriptive statistics (pooled)"
)

# ---- cb108 ---- configure the wave scope and pivot
# create a scope environment to define parameters
create_scope(
  data = liss,
  id = "id", # column name for unique subject IDs
  wave = "t", # column name for time/wave indicator
  gap = 0, # maximum allowed gap between waves
  size = 7, # range of number of waves per subject (id)
  upper = 11, # upper bound on wave number
  override = TRUE # override any existing scope environment
)

eval_scope()
make_set()
filter_set()
pivot()

# ---- cb109 ---- build the wave-pattern view
build_view()

# ---- cb110 ---- show the wave-pattern view
filter_view()

# ---- cb111 ---- physical-activity plausibility filter
# set implausible sport hours to missing before the analysis subset, so the
# filter propagates to liss_subset, the wide frame and every analysis-sample
# table. bmi (observed max about 62) and fv (range 1 to 3) were verified within
# range and left unchanged. pa_ceiling is sport hours per week; values above it
# exceed any plausible general-population sport load.
pa_ceiling <- 40
liss %<>% dplyr::mutate(pa = dplyr::if_else(pa > pa_ceiling, NA_real_, pa))

# ---- cb112 ---- select the analysis sample (at least 3 of waves 1 to 7)
# retain partial respondents: keep everyone observed on at least min_waves of
# the seven analysis waves, so fiml uses all available cases under mar
analysis_waves <- 1:7
min_waves <- 3L
liss_subset <- liss %>%
  dplyr::filter(t %in% analysis_waves) %>%
  dplyr::group_by(id) %>%
  dplyr::filter(sum(!is.na(bmi) | !is.na(fv) | !is.na(pa)) >= min_waves) %>%
  dplyr::ungroup()
save(
  liss_subset,
  file = "liss_subset.RData"
)

describe(liss_subset,
  exclude = c("id", "nohouse_encr"),
  caption = "Descriptive statistics (subset)"
)

# weasel cross-check and selection audit: the same rule as an explicit,
# named scenario (observed = at least one focal measure at a wave)
presence <- liss %>%
  dplyr::filter(
    t %in% analysis_waves,
    !is.na(bmi) | !is.na(fv) | !is.na(pa)
  )
ws_plan <- weasel::weasel_plan(
  presence[c("id", "t", "female", "age")],
  id = "id", wave = "t", span = "full",
  scenarios = data.frame(
    scenario = "min3_of7",
    require_endpoints = FALSE,
    max_missing = length(analysis_waves) - min_waves,
    n_gap_max = 5L,
    max_gap_max = 5L
  )
)
stopifnot(setequal(
  unique(weasel::weasel_apply(ws_plan, "min3_of7")$id),
  unique(liss_subset$id)
))
weasel::weasel_print_table(
  weasel::weasel_sensitivity(
    ws_plan,
    require_endpoints = FALSE, max_missing = 0:6,
    n_gap_max = 5L, max_gap_max = 5L
  ),
  title = "sample size by minimum-wave tolerance"
)
weasel::weasel_print_table(
  weasel::weasel_selectivity(ws_plan, "min3_of7"),
  title = "retained vs excluded respondents"
)
cat(weasel::weasel_justify_subset(ws_plan, "min3_of7"), "\n")

# ---- cb113 ---- reshape long to wide, one row per respondent
transform_to_wide <- function(long_data) {
  wide_data <- long_data %>%
    dplyr::rename(
      id = id,
      w = t,
      BMI = bmi,
      FV = fv,
      PA = pa
    ) %>%
    transform(w = w - min(w) + 1)

  time_invariant <- which(names(wide_data) %in%
    c(
      "id", "w", "age", "med", "female", "ses",
      "hhedu3", "hhedu", "modaal", "hhinc3", "hhinc"
    ))

  df1 <- wide_data[time_invariant] %>%
    dplyr::group_by(id) %>%
    dplyr::slice_min(order_by = w, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-w)

  df2 <- wide_data[-time_invariant[-c(1:2)]]

  df2 <- stats::reshape(
    data = df2,
    idvar = "id",
    timevar = "w",
    direction = "wide",
    sep = "",
  )
  df2 <- df2[order(
    as.numeric(gsub("\\D", "", names(df2))),
    na.last = FALSE
  )]

  dplyr::full_join(df1, df2, by = "id")
}

cols_to_keep <- c(
  "id", "t", "ses", "bmi", "age",
  "fv", "pa", "med", "female"
)

wide <- liss_subset_wide <- transform_to_wide(liss_subset[, cols_to_keep])
save(liss_subset_wide, file = "liss_subset_wide.RData")
describe(liss_subset_wide,
  exclude = "id",
  caption = "Descriptive statistics (wide subset)"
)

# ---- cb114 ---- finalise SES labels and columns
wide$ses <- paste0("ses", wide$ses)
wide <- wide[, grep(paste0(cols_to_keep, collapse = "|"),
  names(wide),
  ignore.case = TRUE, value = TRUE
)]

# ---- cb115 ---- covariate inclusion flags
# Covariate Specification
female <- TRUE
med <- TRUE
age <- TRUE

# ---- cb117 ---- CLPM model syntax
clpm_syntax <- "
# Estimate the lagged effects between the observed variables.
FV2 ~ FV1 + BMI1 + PA1
BMI2 ~ FV1 + BMI1 + PA1
PA2 ~ FV1 + BMI1 + PA1

FV3 ~ FV2 + BMI2 + PA2
BMI3 ~ FV2 + BMI2 + PA2
PA3 ~ FV2 + BMI2 + PA2

FV4 ~ FV3 + BMI3 + PA3
BMI4 ~ FV3 + BMI3 + PA3
PA4 ~ FV3 + BMI3 + PA3

FV5 ~ FV4 + BMI4 + PA4
BMI5 ~ FV4 + BMI4 + PA4
PA5 ~ FV4 + BMI4 + PA4

FV6 ~ FV5 + BMI5 + PA5
BMI6 ~ FV5 + BMI5 + PA5
PA6 ~ FV5 + BMI5 + PA5

FV7 ~ FV6 + BMI6 + PA6
BMI7 ~ FV6 + BMI6 + PA6
PA7 ~ FV6 + BMI6 + PA6

# Estimate the covariance between the observed variables at the first wave.
## Covariance
FV1 ~~ BMI1
FV1 ~~ PA1
BMI1 ~~ PA1

# Estimate the covariances between the residuals of the observed variables.
FV2 ~~ BMI2
FV2 ~~ PA2
BMI2 ~~ PA2

FV3 ~~ BMI3
FV3 ~~ PA3
BMI3 ~~ PA3

FV4 ~~ BMI4
FV4 ~~ PA4
BMI4 ~~ PA4

FV5 ~~ BMI5
FV5 ~~ PA5
BMI5 ~~ PA5

FV6 ~~ BMI6
FV6 ~~ PA6
BMI6 ~~ PA6

FV7 ~~ BMI7
FV7 ~~ PA7
BMI7 ~~ PA7

# Estimate the (residual) variance of the observed variables.
FV1 ~~ FV1 # Variances
BMI1 ~~ BMI1
PA1 ~~ PA1

## Residual variances
FV2 ~~ FV2
BMI2 ~~ BMI2
PA2 ~~ PA2

FV3 ~~ FV3
BMI3 ~~ BMI3
PA3 ~~ PA3

FV4 ~~ FV4
BMI4 ~~ BMI4
PA4 ~~ PA4

FV5 ~~ FV5
BMI5 ~~ BMI5
PA5 ~~ PA5

FV6 ~~ FV6
BMI6 ~~ BMI6
PA6 ~~ PA6

FV7 ~~ FV7
BMI7 ~~ BMI7
PA7 ~~ PA7
"

# ---- cb119 ---- CLPM syntax with covariates
comment <- "
# Specify regressions of covariates on variables at time 1."

female_syntax <- "
BMI1 ~ female
PA1 ~ female
FV1 ~ female
"

med_syntax <- "
BMI1 ~ med
PA1 ~ med
FV1 ~ med
"

age_syntax <- "
BMI1 ~ age
PA1 ~ age
FV1 ~ age
"

female_syntax %<>% ifelse(test = female, "")
med_syntax %<>% ifelse(test = med, "")
age_syntax %<>% ifelse(test = age, "")

clpm_covar_syntax <- paste0(
  clpm_syntax,
  comment,
  female_syntax,
  med_syntax,
  age_syntax,
  collapse = "\n"
)

# ---- cb122 ---- RI-CLPM model syntax
ri_clpm_syntax <- "
# between components (random intercepts)
riBMI =~ 1*BMI1 + 1*BMI2 + 1*BMI3 + 1*BMI4 + 1*BMI5 + 1*BMI6 + 1*BMI7
riPA  =~ 1*PA1  + 1*PA2  + 1*PA3  + 1*PA4  + 1*PA5  + 1*PA6  + 1*PA7
riFV  =~ 1*FV1  + 1*FV2  + 1*FV3  + 1*FV4  + 1*FV5  + 1*FV6  + 1*FV7

# within-person components
wBMI1 =~ 1*BMI1
wPA1  =~ 1*PA1
wFV1  =~ 1*FV1

wBMI2 =~ 1*BMI2
wPA2  =~ 1*PA2
wFV2  =~ 1*FV2

wBMI3 =~ 1*BMI3
wPA3  =~ 1*PA3
wFV3  =~ 1*FV3

wBMI4 =~ 1*BMI4
wPA4  =~ 1*PA4
wFV4  =~ 1*FV4

wBMI5 =~ 1*BMI5
wPA5  =~ 1*PA5
wFV5  =~ 1*FV5

wBMI6 =~ 1*BMI6
wPA6  =~ 1*PA6
wFV6  =~ 1*FV6

wBMI7 =~ 1*BMI7
wPA7  =~ 1*PA7
wFV7  =~ 1*FV7

# autoregressive and cross-lagged effects among within components
# each component is regressed on all three components at the preceding wave
wBMI2 + wPA2 + wFV2 ~ wBMI1 + wPA1 + wFV1
wBMI3 + wPA3 + wFV3 ~ wBMI2 + wPA2 + wFV2
wBMI4 + wPA4 + wFV4 ~ wBMI3 + wPA3 + wFV3
wBMI5 + wPA5 + wFV5 ~ wBMI4 + wPA4 + wFV4
wBMI6 + wPA6 + wFV6 ~ wBMI5 + wPA5 + wFV5
wBMI7 + wPA7 + wFV7 ~ wBMI6 + wPA6 + wFV6

# within covariances at wave 1 (freed)
wBMI1 ~~ wPA1
wBMI1 ~~ wFV1
wPA1  ~~ wFV1

# within residual covariances, waves 2 to 7
wBMI2 ~~ wPA2
wBMI2 ~~ wFV2
wPA2  ~~ wFV2

wBMI3 ~~ wPA3
wBMI3 ~~ wFV3
wPA3  ~~ wFV3

wBMI4 ~~ wPA4
wBMI4 ~~ wFV4
wPA4  ~~ wFV4

wBMI5 ~~ wPA5
wBMI5 ~~ wFV5
wPA5  ~~ wFV5

wBMI6 ~~ wPA6
wBMI6 ~~ wFV6
wPA6  ~~ wFV6

wBMI7 ~~ wPA7
wBMI7 ~~ wFV7
wPA7  ~~ wFV7

# random intercept variances and covariances
riBMI ~~ riBMI
riPA  ~~ riPA
riFV  ~~ riFV

riBMI ~~ riPA
riBMI ~~ riFV
riPA  ~~ riFV

# within variances at wave 1 and residual variances, waves 2 to 7
wBMI1 ~~ wBMI1
wPA1  ~~ wPA1
wFV1  ~~ wFV1

wBMI2 ~~ wBMI2
wPA2  ~~ wPA2
wFV2  ~~ wFV2

wBMI3 ~~ wBMI3
wPA3  ~~ wPA3
wFV3  ~~ wFV3

wBMI4 ~~ wBMI4
wPA4  ~~ wPA4
wFV4  ~~ wFV4

wBMI5 ~~ wBMI5
wPA5  ~~ wPA5
wFV5  ~~ wFV5

wBMI6 ~~ wBMI6
wPA6  ~~ wPA6
wFV6  ~~ wFV6

wBMI7 ~~ wBMI7
wPA7  ~~ wPA7
wFV7  ~~ wFV7

# observed residual variances fixed to zero, routing all variance to the
# random intercepts and within components
BMI1 ~~ 0*BMI1
PA1  ~~ 0*PA1
FV1  ~~ 0*FV1

BMI2 ~~ 0*BMI2
PA2  ~~ 0*PA2
FV2  ~~ 0*FV2

BMI3 ~~ 0*BMI3
PA3  ~~ 0*PA3
FV3  ~~ 0*FV3

BMI4 ~~ 0*BMI4
PA4  ~~ 0*PA4
FV4  ~~ 0*FV4

BMI5 ~~ 0*BMI5
PA5  ~~ 0*PA5
FV5  ~~ 0*FV5

BMI6 ~~ 0*BMI6
PA6  ~~ 0*PA6
FV6  ~~ 0*FV6

BMI7 ~~ 0*BMI7
PA7  ~~ 0*PA7
FV7  ~~ 0*FV7
"

# ---- cb124 ---- RI-CLPM syntax with covariates
comment <- paste0(
  "# Regressions of covariates female and ",
  "medication use at t1 on random intercepts."
)

female_syntax <- "
riBMI ~ female
riPA ~ female
riFV ~ female
"

med_syntax <- "
riBMI ~ med
riPA ~ med
riFV ~ med
"

age_syntax <- "
riPA ~ age
riFV ~ age
riBMI ~ age
"

female_syntax %<>% ifelse(test = female, "")
med_syntax %<>% ifelse(test = med, "")
age_syntax %<>% ifelse(test = age, "")

ri_clpm_covar_syntax <- paste0(
  ri_clpm_syntax,
  comment,
  female_syntax,
  med_syntax,
  age_syntax,
  collapse = "\n"
)

# ---- cb125 ---- fit pooled CLPM
(clpm_pooled_fit <-
  lavaan::lavaan(
    # Model specification
    model = clpm_syntax,
    fixed.x = FALSE,
    data = wide,
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb127 ---- pooled CLPM fit report
report_performance(
  model = clpm_pooled_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb128 ---- spacing
cat("\n")

# ---- cb129 ---- pooled CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(clpm_pooled_fit)
)

# ---- cb132 ---- parameter table
tabs[[1]]

# ---- cb133 ---- spacing
cat("\n")

# ---- cb134 ---- fit-index table
tabs[[2]]

# ---- cb135 ---- fit grouped CLPM
(clpm_grouped_fit <-
  lavaan::lavaan(
    # Model specification
    model = clpm_syntax,
    fixed.x = FALSE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb137 ---- fit grouped CLPM by SES group
# by group
clpm_grouped_fit.g <-
  sapply(c("ses1", "ses2", "ses3"), function(g) {
    lavaan::lavaan(
      # Model specification
      model = clpm_syntax,
      fixed.x = FALSE,
      data = dplyr::filter(wide, ses == g),
      int.ov.free = TRUE,
      meanstructure = TRUE,
      # Method specification
      estimator = "MLR",
      se = "robust",
      missing = "fiml"
    )
  })

# ---- cb138 ---- grouped CLPM fit report
report_performance(
  clpm_grouped_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb139 ---- spacing
cat("\n")

# ---- cb140 ---- grouped CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(clpm_grouped_fit)
)

# ---- cb143 ---- parameter table
tabs[[1]]

# ---- cb144 ---- spacing
cat("\n")

# ---- cb145 ---- fit-index table
tabs[[2]]

# ---- cb146 ---- fit grouped CLPM with covariates
(clpm_grouped_covar_fit <-
  lavaan::lavaan(
    # Model specification
    model = clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb148 ---- fit grouped covariate CLPM by SES group
# by group
clpm_grouped_covar_fit.g <-
  sapply(c("ses1", "ses2", "ses3"), function(g) {
    lavaan::lavaan(
      # Model specification
      model = clpm_covar_syntax,
      fixed.x = TRUE,
      data = dplyr::filter(wide, ses == g),
      int.ov.free = TRUE,
      meanstructure = TRUE,
      # Method specification
      estimator = "MLR",
      se = "robust",
      missing = "fiml"
    )
  })

# ---- cb149 ---- grouped covariate CLPM fit report
report_performance(
  clpm_grouped_covar_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb150 ---- spacing
cat("\n")

# ---- cb151 ---- grouped covariate CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(clpm_grouped_covar_fit)
)

# ---- cb154 ---- parameter table
tabs[[1]]

# ---- cb155 ---- spacing
cat("\n")

# ---- cb156 ---- fit-index table
tabs[[2]]

# ---- cb157 ---- fit pooled RI-CLPM
(ri_clpm_pooled_fit <-
  lavaan::lavaan(
    # Model specification
    model = ri_clpm_syntax,
    fixed.x = FALSE,
    data = wide,
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb159 ---- pooled RI-CLPM fit report
report_performance(
  ri_clpm_pooled_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb160 ---- spacing
cat("\n")

# ---- cb161 ---- pooled RI-CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(ri_clpm_pooled_fit)
)

# ---- cb164 ---- parameter table
tabs[[1]]

# ---- cb165 ---- spacing
cat("\n")

# ---- cb166 ---- fit-index table
tabs[[2]]

# ---- cb167 ---- fit grouped RI-CLPM
(ri_clpm_grouped_fit <-
  lavaan::lavaan(
    # Model specification
    model = ri_clpm_syntax,
    fixed.x = FALSE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb169 ---- fit grouped RI-CLPM by SES group
# by group
ri_clpm_grouped_fit.g <-
  sapply(c("ses1", "ses2", "ses3"), function(g) {
    lavaan::lavaan(
      # Model specification
      model = ri_clpm_syntax,
      fixed.x = FALSE,
      data = dplyr::filter(wide, ses == g),
      int.ov.free = TRUE,
      meanstructure = TRUE,
      # Method specification
      estimator = "MLR",
      se = "robust",
      missing = "fiml"
    )
  })

# ---- cb170 ---- grouped RI-CLPM fit report
report_performance(
  ri_clpm_grouped_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb171 ---- spacing
cat("\n")

# ---- cb172 ---- grouped RI-CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(ri_clpm_grouped_fit)
)

# ---- cb175 ---- parameter table
tabs[[1]]

# ---- cb176 ---- spacing
cat("\n")

# ---- cb177 ---- fit-index table
tabs[[2]]

# ---- cb178 ---- fit grouped RI-CLPM with covariates
(ri_clpm_grouped_covar_fit <-
  lavaan::lavaan(
    # Model specification
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,

    # Method specification
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb180 ---- fit grouped covariate RI-CLPM by SES group
ri_clpm_grouped_covar_fit.g <-
  sapply(c("ses1", "ses2", "ses3"), function(g) {
    lavaan::lavaan(
      # Model specification
      model = ri_clpm_covar_syntax,
      fixed.x = TRUE,
      data = dplyr::filter(wide, ses == g),
      int.ov.free = TRUE,
      meanstructure = TRUE,
      # Method specification
      estimator = "MLR",
      se = "robust",
      missing = "fiml"
    )
  })

# ---- cb181 ---- grouped covariate RI-CLPM fit report
report_performance(
  ri_clpm_grouped_covar_fit,
  as_report = TRUE,
  print_only = TRUE
)

# ---- cb182 ---- spacing
cat("\n")

# ---- cb183 ---- grouped covariate RI-CLPM fit tables
tabs <- suppressMessages(
  model_fit_table(ri_clpm_grouped_covar_fit)
)

# ---- cb186 ---- parameter table
tabs[[1]]

# ---- cb187 ---- spacing
cat("\n")

# ---- cb188 ---- fit-index table
tabs[[2]]

# ---- cb189 ---- assemble all model fits
all_model_fits <- list(
  pooled = list(
    clpm_pooled_fit = clpm_pooled_fit,
    ri_clpm_pooled_fit = ri_clpm_pooled_fit
  ),
  grouped = list(
    clpm_grouped_fit = clpm_grouped_fit,
    clpm_grouped_covar_fit = clpm_grouped_covar_fit,
    ri_clpm_grouped_fit = ri_clpm_grouped_fit,
    ri_clpm_grouped_covar_fit = ri_clpm_grouped_covar_fit
  ),
  subgrouped = list(
    clpm_grouped_fit.g = clpm_grouped_fit.g,
    clpm_grouped_covar_fit.g = clpm_grouped_covar_fit.g,
    ri_clpm_grouped_fit.g = ri_clpm_grouped_fit.g,
    ri_clpm_grouped_covar_fit.g = ri_clpm_grouped_covar_fit.g
  )
)

all_model_syntax <- named_list(
  clpm_syntax,
  clpm_covar_syntax,
  ri_clpm_syntax,
  ri_clpm_covar_syntax
)

save(all_model_syntax, file = "all_model_syntax.RData")

rds_file_name <- paste0(
  "all_model_fits_",
  gsub("[-.: ]", "_", Sys.time()), ".Rds"
)
cli::cli_alert_info("Saving model fits to {rds_file_name}")

# ---- cb190 ---- save model fits to disk
saveRDS(all_model_fits, file = rds_file_name)

# ---- cb191 ---- per-group RI-CLPM path estimates
compare_ses_groups(
  ri_clpm_grouped_fit,
  include_p_values = FALSE,
  hide_non_significant = TRUE
) %>%
  dplyr::rename(
    `$est_{SES1}$` = est_SES_1,
    `$sig_{SES1}$` = p_SES_1,
    `$est_{SES2}$` = est_SES_2,
    `$sig_{SES2}$` = p_SES_2,
    `$est_{SES3}$` = est_SES_3,
    `$sig_{SES3}$` = p_SES_3
  ) -> df
df$effect <- sapply(df$effect, function(x) {
  paste0("$", gsub("~", "\\\\sim", gsub("([0-9]+)", "_\\1", x)), "$")
}, USE.NAMES = FALSE)
knitr::kable(df,
  format = "markdown",
  row.names = FALSE,
  caption = "Cross-Lagged Effects between BMI, Diet, and Physical Activity"
)

# ---- cb192 ---- per-group covariate RI-CLPM path estimates
compare_ses_groups(
  all_model_fits$grouped$ri_clpm_grouped_covar_fit,
  include_p_values = FALSE,
  hide_non_significant = TRUE
) %>%
  dplyr::rename(
    `$est_{SES1}$` = est_SES_1,
    `$sig_{SES1}$` = p_SES_1,
    `$est_{SES2}$` = est_SES_2,
    `$sig_{SES2}$` = p_SES_2,
    `$est_{SES3}$` = est_SES_3,
    `$sig_{SES3}$` = p_SES_3
  ) -> df

df$effect <- sapply(df$effect, function(x) {
  paste0("$", gsub("~", "\\\\sim", gsub("([0-9]+)", "_\\1", x)), "$")
}, USE.NAMES = FALSE)

knitr::kable(df,
  format = "markdown",
  row.names = FALSE,
  caption = "Cross-Lagged Effects Adjusting for Covariates"
) %>% kableExtra::kable_styling(latex_options = "hold_position")

# ---- cb194 ---- load lavaan
library(lavaan)

# ---- cb195 ---- load minvariance
library(minvariance)
library(stringr)
library(dplyr)

# ---- cb196 ---- variables for measurement invariance
var_list <- list(
  BMI = c("BMI1", "BMI2", "BMI3", "BMI4", "BMI5", "BMI6", "BMI7"),
  PA = c("PA1", "PA2", "PA3", "PA4", "PA5", "PA6", "PA7"),
  FV = c("FV1", "FV2", "FV3", "FV4", "FV5", "FV6", "FV7")
)

# ---- cb197 ---- invariance levels
model_types <- c("configural", "weak", "strong", "strict")
purrr::iwalk(model_types, ~ {
  assign(paste0(.x, "_syntax"),
    minvariance::long_minvariance_syntax(var_list, model = .x),
    envir = .GlobalEnv
  )
})

# ---- cb198 ---- configural syntax
cat(configural_syntax)

# ---- cb200 ---- weak (metric) syntax
cat(weak_syntax)

# ---- cb202 ---- strong (scalar) syntax
cat(strong_syntax)

# ---- cb204 ---- strict syntax
cat(strict_syntax)

# ---- cb206 ---- fit the configural model
configural_model <- cfa(configural_syntax, data = wide,
  estimator = "MLR", se = "robust", missing = "fiml",
  fixed.x = FALSE)
weak_model <- cfa(weak_syntax, data = wide,
  estimator = "MLR", se = "robust", missing = "fiml",
  fixed.x = FALSE)
strong_model <- cfa(strong_syntax, data = wide,
  estimator = "MLR", se = "robust", missing = "fiml",
  fixed.x = FALSE)
strict_model <- cfa(strict_syntax, data = wide,
  estimator = "MLR", se = "robust", missing = "fiml",
  fixed.x = FALSE)

# ---- cb207 ---- extract invariance fit statistics
fit_stats <- extract_fit(
  configural_model,
  weak_model,
  strong_model,
  strict_model
) %>%
  mutate(model = dplyr::case_when(
    model == 1 ~ "configural",
    model == 2 ~ "weak",
    model == 3 ~ "strong",
    model == 4 ~ "strict"
  ))

# ---- cb208 ---- longitudinal invariance fit table
fit_stats %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  knitr::kable(format = "markdown", digits = 3, align = "l")

# ---- cb209 ---- longitudinal invariance models
long_meas_inv_models <-
  list(configural_model, weak_model, strong_model, strict_model)
comp_long_meas_inv <- semTools::compareFit(long_meas_inv_models)

# ---- cb210 ---- model-naming helper
add_model_names <- function(...) {
  stringr::str_replace_all(
    ...,
    c(
      "long_meas_inv_models.1" = "config_inv",
      "long_meas_inv_models.2" = "weak_inv",
      "long_meas_inv_models.3" = "strong_inv",
      "long_meas_inv_models.4" = "strict_inv"
    )
  )
}

comp_long_meas_inv@name %<>% add_model_names()
rownames(comp_long_meas_inv@nested) %<>% add_model_names()
rownames(comp_long_meas_inv@fit) %<>% add_model_names()
rownames(comp_long_meas_inv@fit.diff) %<>% add_model_names()

# ---- cb211 ---- longitudinal invariance comparison
summary(comp_long_meas_inv)

# ---- cb213 ---- invariance parameters to compare
model_parameters <- c(
  "loadings",
  "regressions",
  "residuals",
  "residual.covariances"
)

ses_models <- list(
  config = NULL,
  metric = model_parameters[1],
  scalar = model_parameters[1:2],
  strict = model_parameters[1:3],
  residu = model_parameters
)

# ---- cb214 ---- fit configural model across SES
(configural_ses <-
  lavaan::lavaan(
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb216 ---- fit metric model across SES
(metric_ses <-
  lavaan::lavaan(
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    group.equal = c("loadings"),
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb218 ---- fit scalar model across SES
(scalar_ses <-
  lavaan::lavaan(
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    group.equal = c("loadings", "regressions"),
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb220 ---- fit strict model across SES
(strict_ses <-
  lavaan::lavaan(
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    group.equal = c("loadings", "regressions", "residuals"),
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb222 ---- fit residual model across SES
(residual_ses <-
  lavaan::lavaan(
    model = ri_clpm_covar_syntax,
    fixed.x = TRUE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    group.equal = c(
      "loadings", "regressions",
      "residuals", "residual.covariances"
    ),
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  ))

# ---- cb224 ---- across-SES invariance of the within-person dynamics
# within-wave residual covariances, freed across groups (the error structure is
# not the target of the cross-group test).
covariances_of_residuals <-
  c(
    "wBMI2 ~~ wPA2",
    "wBMI2 ~~ wFV2",
    "wPA2 ~~ wFV2",
    "wBMI3 ~~ wPA3",
    "wBMI3 ~~ wFV3",
    "wPA3 ~~ wFV3",
    "wBMI4 ~~ wPA4",
    "wBMI4 ~~ wFV4",
    "wPA4 ~~ wFV4",
    "wBMI5 ~~ wPA5",
    "wBMI5 ~~ wFV5",
    "wPA5 ~~ wFV5",
    "wBMI6 ~~ wPA6",
    "wBMI6 ~~ wFV6",
    "wPA6 ~~ wFV6",
    "wBMI7 ~~ wPA7",
    "wBMI7 ~~ wFV7",
    "wPA7 ~~ wFV7"
  )

# fit the invariance models on the within-person dynamics only (no covariates):
# covariates predict the random intercepts but are not part of the dynamics, and
# the constrained covariate model fails to converge under FIML.
fit <- lapply(names(ses_models), function(i) {
  .fit <- lavaan::lavaan(
    model = ri_clpm_syntax,
    fixed.x = FALSE,
    data = wide,
    group = "ses",
    group.label = c("ses1", "ses2", "ses3"),
    int.ov.free = TRUE,
    meanstructure = TRUE,
    check.start = TRUE,
    group.equal = ses_models[[i]],
    group.partial = covariances_of_residuals,
    estimator = "MLR",
    se = "robust",
    missing = "fiml"
  )
  cli::cli_progress_step(i)
  return(.fit)
})

# ---- cb225 ---- name the SES invariance fits
names(fit) <- names(ses_models)

# ---- cb226 ---- across-SES invariance: fit by level and the focal structural test
# robust fit at each constraint level. levels: config (nothing equal), metric
# (loadings, automatic under unit loadings), scalar (loadings + regressions),
# strict (+ residuals), residu (+ residual covariances). built from base
# lavaan::fitMeasures to sidestep the semTools/lavaan version skew.
ses_inv_fit <- as.data.frame(t(sapply(
  fit[c("config", "metric", "scalar", "strict", "residu")],
  function(m) {
    lavaan::fitMeasures(m, c(
      "npar", "chisq.scaled", "df.scaled",
      "cfi.robust", "tli.robust", "rmsea.robust", "srmr", "aic", "bic"
    ))
  }
)))
ses_inv_fit <- cbind(level = rownames(ses_inv_fit), ses_inv_fit)
knitr::kable(
  ses_inv_fit, row.names = FALSE, digits = 3,
  caption = "across-SES invariance of the within-person dynamics: robust fit by level"
)

# focal cross-group structural test: can the within-person regression paths be
# held equal across SES (configural vs equal regressions; metric is automatic).
ses_struct_lrt <- lavaan::lavTestLRT(
  fit$config, fit$scalar,
  method = "satorra.2000",
  model.names = c("configural", "equal regressions")
)
knitr::kable(
  cbind(model = rownames(ses_struct_lrt), as.data.frame(ses_struct_lrt)),
  row.names = FALSE, digits = 3,
  caption = "within-person regressions free vs equal across SES (scaled LRT)"
)

# ---- cb227 ---- SES invariance nested tests
comp_ses_meas_inv_ls$nested %>%
  knitr::kable(
    format = "markdown",
    digits = 4,
    caption = paste(
      "Model fit comparisons between adjacently nested",
      "models that are ordered by their degrees of freedom (df)"
    )
  )

# ---- cb228 ---- SES invariance fit indices
comp_ses_meas_inv_ls$fit %>%
  knitr::kable(
    format = "markdown",
    digits = 4,
    caption = paste0(
      "Fit measures of all models specified,",
      " ordered by their df"
    )
  )

# ---- cb229 ---- SES invariance fit differences
comp_ses_meas_inv_ls$fit.diff %>%
  knitr::kable(
    format = "markdown",
    digits = 3,
    caption = "Sequential differences in fit measures"
  )

# ---- cb230 ---- maximum standardised residual variances
msdv <- lapply(fit, max_std_residual_variance)

# ---- cb231 ---- residual-variance table helper
create_msdv_table <- function(residual_variances,
                              sort_by_total_diff = TRUE,
                              print_table = TRUE,
                              model_name = "") {
  ses_groups <- c("low_ses", "mid_ses", "high_ses")

  df <- data.frame(residual_variances)
  names(df) <- ses_groups

  df <- within(df, {
    mean_across_groups <- rowMeans(df)
    total_abs_diff <- abs(low_ses - mean_across_groups) +
      abs(mid_ses - mean_across_groups) +
      abs(high_ses - mean_across_groups)
  })

  if (sort_by_total_diff) {
    df <- dplyr::arrange(df, desc(total_abs_diff))
  }

  output_df <- df[c(ses_groups, "mean_across_groups", "total_abs_diff")]

  n_digits <- round_sig(min(unlist(residual_variances)),
    return_n_digits = TRUE
  )

  table <- knitr::kable(
    output_df,
    format = "markdown",
    digits = n_digits,
    caption = paste0(
      "Squared Standardized Residual Variance by SES Group for ",
      model_name,
      ". Ordered by Total Absolute Difference.",
      " Larger Values and Differences Indicate
  Greater Discrepancy Between SES Groups."
    ),
  )

  if (print_table) {
    return(table)
  }

  return(output_df)
}

# ---- cb232 ---- configural residual-variance table
create_msdv_table(msdv$config, model_name = "Configural Model")

# ---- cb233 ---- metric residual-variance table
create_msdv_table(msdv$metric, model_name = " Metric Invariance Model")

# ---- cb234 ---- scalar residual-variance table
create_msdv_table(msdv$scalar, model_name = "Scalar Invariance Model")

# ---- cb235 ---- strict residual-variance table
create_msdv_table(msdv$strict, model_name = "Strict Invariance Model")

# ---- cb236 ---- residual-invariance residual-variance table
create_msdv_table(msdv$residu, model_name = "Residual Invariance Model")

# ---- cb237 ---- html post-processing helper
process_text <- function(x) {
  sapply(x, function(i) {
    if (stringr::str_detect(i, " w/")) {
      i <-
        stringr::str_replace_all(
          i,
          " w/",
          paste(
            "<p style='display: contents;'>",
            "&#65374;",
            "</p>"
          )
        )
    }
    if (stringr::str_detect(i, stringr::regex("[0-9]+</td>"))) {
      i <- stringr::str_replace_all(
        i,
        "(BMI|FV|PA)([0-9])",
        paste0(
          "\\1",
          "<sub class='subscript'>",
          "\\2",
          "</sub>"
        )
      )
    }
    i <- stringr::str_replace_all(
      i,
      "<td>([0-9])",
      paste0(
        "<td><div style='",
        "display:inline-block;",
        "opacity:0!important;'>",
        "&#8722;</div>", "\\1"
      )
    )
    stringr::str_replace_all(i, "<td>-([0-9])", "<td>&#8722;\\1")
  })
}

sem_table <- function(fit, table_number, table_caption) {
  est_se_lab <- "Est(SE)"
  out <- semTable::semTable(
    object = fit,
    paramSets = c(
      "composites",
      "slopes",
      "intercepts",
      # "residualvariances",    > all fixed to 0 as expected
      # "residualcovariances",  > not substantively meaningful
      "latentvariances",
      "latentcovariances",
      "fits"
    ),
    type = "html",
    fits = c(
      "chisq",
      "df",
      "pvalue",
      "baseline.chisq",
      "baseline.df",
      "baseline.pvalue",
      "cfi",
      "tli",
      "rmsea",
      "rmsea.pvalue",
      "srmr",
      "aic",
      "bic"
    ),
    fitLabels = c(
      # Observed chi-square
      "chisq" = "&chi;<sup>2</sup><sub><i>O</i></sub>",
      # Degrees of Freedom
      "df" = "df",
      # p-value
      "pvalue" = "<i>p</i>",
      # Baseline chi-square
      "baseline.chisq" = "<i>&chi;<sup>2</sup></i><sub>baseline</sub>",
      # Baseline Degrees of Freedom
      "baseline.df" = "df<sub>baseline</sub>",
      # p-value under independence assumption
      "baseline.pvalue" = "<i>p</i><sub>&perp;</sub>",
      # Comparative Fit Index
      "cfi" = "CFI",
      # Tucker-Lewis Index
      "tli" = "TLI",
      # Root Mean Square Error of Approximation
      "rmsea" = "RMSEA",
      # p-value for test of close fit (RMSEA)
      "rmsea.pvalue" = "<i>p</i><sub>&dagger;</sub>",
      # Standardized Root Mean Square Residual
      "srmr" = "SRMR",
      # Akaike Information Criterion
      "aic" = "AIC",
      # Bayesian Information Criterion
      "bic" = "BIC",
      # Scaled chi-square
      "scaled.chisq" = "Scaled <i>&chi;<sup>2</sup></i>"
    ),
    columns = c("estse", "p"),
    columnLabels = c("estse" = "", "p" = ""),
    groups = c("ses1", "ses2", "ses3"),
    caption = "Results for the SEM.",
    table.float = TRUE,
    print.results = FALSE
  )
  out <- sapply(out, process_text)
  custom_style <- paste0(
    "<style>.subscript{vertical-align:baseline;",
    "font-size:small;}</style>"
  )
  table_out <- paste0(custom_style, out)
  return(list(table_out, table_number, table_caption))
}

make_sem_table <- function(fit, table_number, table_caption) {
  html_lines <- sem_table(fit, table_number, table_caption)
  table_number <- html_lines[[2]]
  table_caption <- html_lines[[3]]

  html_lines <- strsplit(html_lines[[1]], "<td>\\.")[[1]]
  html_lines <- sapply(html_lines, function(x) {
    p_value <- stringr::str_extract(x, "^([0-9]{3})</td>", group = 1)
    if (is.na(p_value)) {
      return(x)
    } else {
      new_p <- paste0(p_to_asterisk(p_value), "</td>")
      y <- stringr::str_replace(x, paste0(p_value, "</td>"), new_p)
    }
    paste0("<td>", y, "")
  })

  html_lines <- unname(html_lines) %>%
    paste0(collapse = "") %>%
    strsplit("\n") %>%
    `[[`(1)


  html_lines <- gsub("</td><td><sup>", "<sup>", html_lines)
  grepl("ses1", html_lines)
  html_lines <- gsub(
    "align = 'center'>ses([0-9])</td>",
    paste0(
      "align = 'center'><p style='border-bottom: 1px solid black;",
      "text-align: center;'>SES<sub>\\1</sub></p></td>"
    ),
    html_lines
  )

  html_lines <- gsub(
    "align = 'center'>ri_clpm_pooled_fit</td>",
    paste0(
      "align = 'center'><p style='border-bottom: 1px solid black;",
      "text-align: center;'>RI-CLPM<sub>pooled</sub></p></td>"
    ),
    html_lines
  )

  html_lines <- gsub(
    "align = 'center'>clpm_pooled_fit</td>",
    paste0(
      "align = 'center'><p style='border-bottom: 1px solid black;",
      "text-align: center;'>CLPM<sub>pooled</sub></p></td>"
    ),
    html_lines
  )
  html_lines <- paste0(
    paste0(
      "<caption><b>Table ",
      table_number,
      ". </b>", table_caption,
      "</caption>"
    ),
    paste0(html_lines, collapse = "\n")
  )

  # Responsive table style
  responsive_style <- paste0(
    "<style>",
    ".responsive-table {",
    "overflow-x: auto;",
    "margin: auto;",
    "}",
    "table {",
    "width: 100%;",
    "max-width: 100%;",
    "margin-bottom: 1rem;",
    "background-color: transparent;",
    "border-collapse: collapse;",
    "}",
    "table, th, td {",
    "border: 1px solid #ddd;",
    "}",
    "th, td {",
    "text-align: left;",
    "padding: 8px;",
    "}",
    "th {",
    "background-color: #f2f2f2;",
    "}",
    "</style>"
  )

  # Wrap table with a div that has the responsive-table class
  html_lines <- paste0(
    responsive_style,
    "<div class='responsive-table'>",
    "<table>",
    paste0(
      "<caption><b>Table ",
      table_number,
      ". </b>", table_caption,
      "</caption>"
    ),
    paste0(html_lines, collapse = "\n"),
    "</table>",
    "</div>"
  )

  return(html_lines)
}

# ---- cb238 ---- build SEM results table 1
i <- 1
table1 <- make_sem_table(
  all_model_fits$pooled,
  table_number = i,
  table_caption = "Pooled CLPM and RI-CLPM Results"
)

# ---- cb239 ---- build SEM results table 2
i <- i + 1
table2 <- make_sem_table(
  all_model_fits$grouped$clpm_grouped_fit,
  table_number = i,
  table_caption = "CLPM Results by SES Group"
)

# ---- cb240 ---- build SEM results table 3
i <- i + 1
table3 <- make_sem_table(
  all_model_fits$grouped$clpm_grouped_covar_fit,
  table_number = i,
  table_caption = "CLPM Results by SES Group, Adjusting for Covariates"
)

# ---- cb241 ---- build SEM results table 4
i <- i + 1
table4 <- make_sem_table(
  all_model_fits$grouped$ri_clpm_grouped_fit,
  table_number = i,
  table_caption = "RI-CLPM Results by SES Group"
)

# ---- cb242 ---- build SEM results table 5
i <- i + 1
table5 <- make_sem_table(
  all_model_fits$grouped$ri_clpm_grouped_covar_fit,
  table_number = i,
  table_caption = "RI-CLPM Results by SES Group, Adjusting for Covariates"
)

# ---- cb243 ---- render SEM results table 1
htmltools::knit_print.html(table1, inline = TRUE)

# ---- cb244 ---- render SEM results table 2
htmltools::knit_print.html(table2)

# ---- cb245 ---- render SEM results table 3
htmltools::knit_print.html(table3)

# ---- cb246 ---- render SEM results table 4
htmltools::knit_print.html(table4)

# ---- cb247 ---- render SEM results table 5
htmltools::knit_print.html(table5)

