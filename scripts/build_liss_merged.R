# raw -> merged construction for the rhd compendium: a documented record of how
# the raw liss extracts were cleaned and merged into liss_merged_long.sav.
#
# this is provenance, not a verified reproducer. it targets the original extract
# schema and does not run end-to-end against the extracts currently in data/:
# cb16 joins background to income on a `wave` column that the current
# liss_income.sav no longer carries (it has only `wavenr`), so the inputs have
# diverged from this code. the canonical, audit-pinned analysis input is the
# committed liss_merged_long.sav, alongside the frozen
# tests/testthat/_baseline/reference.rds; keep this script for the record of the
# cleaning logic, not for regeneration from today's data/.
#
# relocated from reciprocal-health-dynamics.R chunks cb2-cb50; cb27
# (p_to_asterisk) now lives in R/. paths resolve through liss_data_dir().
# build-ignored.

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

# from cb2
read_spss_wrapper <- function(file_name) {
  path <- fs::path(liss_data_dir(), file_name)
  haven::read_spss(path, user_na = TRUE)
}

# from cb3
health <- read_spss_wrapper("liss_health.sav")
sport <- read_spss_wrapper("liss_sport.sav")
income <- read_spss_wrapper("liss_income.sav")
background <- read_spss_wrapper("liss_background.sav")
liss_merged <- named_list(
  health = health, sport = sport, income = income, background = background
) # named args: current named_list() leaves unnamed args unnamed (see tests)

# from cb4
library(haven)
library(dplyr)
library(zoo)
library(stats)
library(sjlabelled)
library(imputeTS)

# from cb5
mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# from cb6
rename <- function(df, old, new) {
  for (i in seq_along(old)) {
    x <- which(names(df) == old[i])
    names(df)[x] <- new[i]
  }
  return(df)
}

# from cb7
rna <- function(x, flat = FALSE) {
  if (flat) {
    x <- unlist(x)
  }
  if (is.list(x)) {
    x <- na.omit(x)
  } else {
    x <- x[!is.infinite(x)]
  }
  return(x)
}

# from cb8
rows <- function(argument, ind = FALSE) {
  arg <- substitute(argument)
  x <- as.character(arg)
  obj <- x[grep("[$|:]", x)]
  i <- which(eval(arg))
  val <- eval(parse(text = paste0(obj, "[", arg, "]")))
  if (!ind) {
    return(val)
  } else {
    return(i)
  }
}

# from cb9
dummy_na <- function(df,
                     ind,
                     col,
                     dummy,
                     copy_value = TRUE,
                     print_length = FALSE) {
  if (copy_value == TRUE) {
    d_value <- df[[col]][ind]
  } else {
    d_value <- 1
  }
  df[[dummy]][ind] <- d_value
  df[[col]][ind] <- NA
  if (print_length == TRUE) {
    print(length(ind))
  }

  return(df)
}

# from cb10
mav <- function(...) {
  x <- na.omit(...)
  rollmean(x, k = 1)
}

# from cb11
get_power10 <- function(x, type = "nearest", cutoff = 0.75) {
  x <- na.omit(x)
  trunc_x <- 10^trunc(log10(x))
  ceiling_x <- 10^ceiling(log10(x))
  if (type == "trunc") {
    p <- trunc_x
  } else if (type == "ceiling") {
    p <- ceiling_x
  } else if (type == "nearest") {
    p <- sapply(seq_along(x), function(i) {
      if (x[i] / trunc_x[i] > (cutoff * 10)) {
        ceiling_x[i]
      } else {
        trunc_x[i]
      }
    })
  }
  return(p)
}

# from cb12
lag_diff <- function(df, power10 = TRUE) {
  x <- na.omit(df$nethh)
  change <- function(k) {
    delt <- log(k / lag(k, 1))
    trim <- signif(mean(abs(delt)), 2)
    return(trim)
  }
  d <- sapply(seq_along(x), function(i) {
    change(x[(i - 1):(i + 1)])
  })
  df$diff[!is.na(df$nethh)] <- d
  if (power10) {
    df$power10[!is.na(df$nethh)] <- get_power10(x)
  }
  return(df)
}

# from cb13
row_by_col <- function(x = 1:nrow(df),
                       col = c("nethh_min", "nethh_max"),
                       value = "median") {
  value <- match.fun(value)
  if (length(col) > 1) {
    val <- value(rowMeans(df[x, col], na.rm = TRUE))
  } else {
    val <- value(df[x, col], na.rm = TRUE)
  }
  return(val)
}

# from cb14
ref <- function(x) {
  rr <- which(df$nethh %in% df$nethh[x])
  rr <- c(
    df$nethh[-rr],
    mav(df$nethh),
    row_by_col(col = 5:6, value = "mean"),
    row_by_col(col = 5:6, value = "median")
  )
  return(rr)
}

# from cb15
similar_cases <- function(x,
                          y,
                          ref,
                          key,
                          method = "max",
                          value = "median",
                          min_n = 1) {
  m <- match.fun(method)
  v <- match.fun(value)
  base <- which(!is.na(y[ref]))
  if (is.integer(key)) {
    key <- names(y)[key]
  }
  key <- names(y)[colSums(is.na(y[key])) != nrow(y)]
  for (i in seq_along(key)) {
    k <- which(y[key[i]] == m(x[key[i]]))
    if (length(intersect(base, k)) >= min_n) {
      base <- intersect(base, k)
    }
  }
  p <- v(y[base, ref])
  return(p)
}

# from cb16
background <- liss_merged$background
income <- liss_merged$income

# merge background variables with income data
# use inner_join to add `nomem_encr` and `wave` columns
# use full_join to add all remaining columns
background_ <- inner_join(background, income[, 1:3],
  by = c("nomem_encr", "wave")
)
income_ <- full_join(background_, income,
  by = c("nomem_encr", "wave", "wavenr")
)

# select and rename variables for analysis
# ci00a339 = Total net household income
# ci00a229 = Net household income category
# ci00a001 = Position in household
income <- rename(
  income_,
  c("ci00a339", "ci00a229", "ci00a001", "wave"),
  c("nethh", "nethh_min", "positiehh", "wavenr")
)

# add max value for income category
income$nethh_max <- income$nethh_min

# select columns for analysis
income <- income[, c(
  "nomem_encr",
  "nohouse_encr",
  "wavenr",
  "nethh",
  "nethh_min",
  "nethh_max",
  "brutoink",
  "nettoink",
  "aantalhh",
  "positiehh",
  "belbezig",
  "leeftijd",
  "oplmet",
  "geslacht"
)]

# convert income variables to numeric
i <- grep("net|brut", names(income))
income[, i] <- lapply(income[, i], function(x) {
  abs(as.numeric(x))
})

# mark invalid scores as NA
# default is NA because 0 could be a valid score
income$user_na <- income$is_na <- NA

i <- which(income$nethh_min < 1 | income$nethh_min > 7)
income <- dummy_na(income, i, "nethh_min", "user_na")
income <- dummy_na(income, i, "nethh_max", "user_na")

# only mark is_na as NA if user_na is NA
income$is_na[income$user_na == NA] <- NA

# find and replace labeled NA values and large outliers
na <- unique(unlist(sapply(
  income,
  function(x) {
    c(attr(x[[1]], "na_values", attr(x[[1]], "na_range")))
  }
)))

for (c in 1:ncol(income)) {
  i <- which(is.element(unlist(income[, c]), na) |
    abs(unlist(income[, c])) > 9999999)
  if (length(i)) {
    income[i, c] <- NA
    income$user_na[i] <- 1
  }
}

# standardize user_na as 0/1 factor
income$user_na <- factor(income$user_na, levels = c(0, 1))

# remove labels for faster computation
income <- data.frame(remove_all_labels(income))

# find potential outliers
x <- which(with(income, nethh < 10 |
  (!is.na(brutoink) & nethh <= 100) |
  (nethh < 10000 & ((abs(nethh - nettoink) < 100) |
    (abs(nethh - brutoink) < 100)
  ))))

# store original values
income$is_na[x] <- income$nethh[x]

# replace with NA
income$nethh[x] <- NA

# get min/max values for income categories
upper <- c(8000, 16000, 24000, 36000, 48000, 60000, 120000)
lower <- c(0, 8000, 16000, 24000, 36000, 48000, 60000)

income$nethh_min <- lower[match(income$nethh_min, 1:7)]
income$nethh_max <- upper[match(income$nethh_max, 1:7)]

# reverse coding for analysis
i <- which(income$oplmet > 6)
income$oplmet[i] <- -as.numeric(income$oplmet[i])

i <- which(income$belbezig > 1)
income$belbezig[i] <- -as.numeric(income$belbezig[i])

income$positiehh <- (8 - income$positiehh)

# get unique household ids
hh <- unique(income$nohouse_encr)

# initialize analysis variables
income$valid_hh <-
  income$power10 <- income$outlier <- income$diff <- 0

# empty list for insufficient data
rest_hh <- c()

# from cb17
for (h in 1:length(hh)) {
  df <- income[which(income$nohouse_encr == hh[h]), ]

  if (length(rna(df$nethh)) > 1) {
    loop <- rep <- err <- 0
    df <- lag_diff(df)
    mode_power10 <- mode(rows(df$power10 > 0))

    if (all(df$power10 < 10000)) {
      i <- which(df$power10 > 0)
      df$outlier[i] <- df$nethh[i]
      ifelse(all(df$diff[i] < 0.6),
        x <- (10000 / df$power10[i]),
        x <- min(rna(df$power10[i]))
      )
      df$nethh[i] <- df$nethh[i] * x
    }

    if (mode(df$power10) == 1000) {
      i <- which(df$power10 == 1000)
      m <- mean(df$nethh[i])
      if (which.min(abs(row_by_col() - c(m, m * 10))) == 2) {
        df$nethh[i] <- df$nethh[i] * 10
      }
    }

    wave_cases <- Filter(nrow, lapply(
      unique(df$wavenr),
      function(w) {
        rna(unique(df[df$wavenr == w, c("wavenr", "nethh")]))
      }
    ))

    cases <- sapply(wave_cases, `[[`, 1)
    if (any(lengths(cases)) > 1) {
      for (i in seq_along(cases)) {
        w <- which(df$wavenr == 4)
        if (any(df$diff[w] >= 0.5)) {
          k <- w[which.min(df$diff[w])]
          w <- setdiff(w, k)
          for (i in 1:nrow(w)) {
            if (k$outlier[w] == 0) {
              df$outlier[w[i]] <- df$nethh[w[i]]
              df$nethh[w[i]] <- df$nethh[k]
            }
          }
        }
      }
    }

    while (length(err)) {
      df <- lag_diff(df)

      if (length(unique(rows(df$power10 > 0))) > 2) {
        mode_power10 <- 10000
        skip_imp <- TRUE
      } else {
        mode_power10 <- mode(rows(df$power10 > 1000))
        skip_imp <- FALSE
      }

      err <- which(df$diff >= 0.6 & df$power10 != mode_power10)

      if (!length(err) & any(!is.na(df$nethh_max))) {
        d <- unlist(lapply(unique(df$wavenr), function(i) {
          w <- which(df$wavenr == i)

          return(df$nethh[w] - ifelse(any(!is.na(
            df$nethh_max[w]
          )),
          max(rna(
            df$nethh_max[w]
          )), df$nethh[w]
          ))
        }))

        if (any(!is.na(d)) && max(rna(d)) >= 1000) {
          if (df$diff[which.max(d)] >= 0.6) {
            err <- which.max(d)
          }
        }
      }

      if (!length(err)) {
        m <- mean(ref(err))
        diff <- sapply(df$nethh, function(x) {
          abs(log1p((x - m) / m))
        })
        if (any(rna(diff) > 0.9)) {
          err <- which.max(diff)
        }
      }

      quart1 <- quantile(df$nethh, 0.25)
      quart3 <- quantile(df$nethh, 0.75)
      below <- df$nethh < quart1 - 1.5 * IQR(df$nethh)
      above <- df$nethh > quart3 + 1.5 * IQR(df$nethh)
      df <- df[!(below | above), ]

      if (!length(err)) {
        break
      }
      i <- ifelse(all(abs(diff(
        trunc(sapply(err, function(e) {
          mean(abs(ref(err) - e))
        }) / 100)
      )) < 10),
      err[which.max(df$diff[err])],
      err[which.max(sapply(err, function(e) {
        mean(abs(ref(err) - e))
      }))]
      )

      r <- which(rep == i)
      if (any(r)) {
        if (length(err) > 1) {
          err_ <- setdiff(err, i)
          i <- err_[which.max(df$diff[err_])]
        } else if (length(r) > 2) {
          break
        }
      }

      rep <- c(rep, i)
      if (skip_imp == FALSE) {
        df_ <- df[, 4:9]

        df_[which((. <- df$power10) == df$power10[i] &
          . != mode_power10), 1] <- NA

        x <- sapply(df_, function(i) {
          list(length(rna(i)), length(unique(rna(i))))
        })

        x_ <- which(x[1, ] > 2 & x[2, ] > 2)
        if ("nethh" %in% names(df_[x_])) {
          imp <-
            na_kalman(df_[x_],
              model = "StructTS",
              smooth = TRUE
            )[i, 1]
        } else if (length(rna(df_$nethh[-i])) > 1) {
          imp <- na_ma(df_$nethh)[i]
        }
      } else {
        imp <- 0
      }

      wave_cluster <- which(df$wavenr == df$wavenr[i])

      sub <- c(
        rna(df$nethh[setdiff(wave_cluster, i)]),
        df$nethh[i] * mode_power10 / df$power10[i],
        df$nethh[i] + mode_power10,
        df$nethh[i] - mode_power10,
        imp,
        mean(rna(df$nethh_max)),
        similar_cases(df, income, "nethh", 9:13, value = "mean")
      )

      sub <- sub[sub >= 8000]

      if (df$outlier[i] == 0 & is.na(df$outlier[i])) {
        df$outlier[i] <- df$nethh[i]
      }

      df$nethh[i] <- sub[which.min(sapply(sub, function(e) {
        mean(abs(ref(err) - e))
      }))]

      loop <- loop + 1

      if (loop == nrow(df)) {
        break
      }

      if (any(df$outlier > 0)) {
        income[
          which(income$nohouse_encr == hh[h]),
          c(4, 15:20)
        ] <- df[, c(4, 15:20)]

        income_temp <- income
      } else {
        rest_hh <- c(rest_hh, hh[h])
      }
    }
  }
}
liss_merged$income <- income

# from cb18
is_convertible_to_numeric <- function(x) {
  !anyNA(suppressWarnings(as.numeric(x)))
}

mokken_assumptions <- function(df, wave) {
  df <- as.data.frame(df)
  coefs <- mokken::coefH(df, se = FALSE, results = FALSE)
  monot <- summary(mokken::check.monotonicity(df))
  rbind(c(
    t = wave,
    `Hij (min)` = min(coefs$Hij),
    `Hi (min)` = min(coefs$Hi),
    H = (coefs$H),
    apply(monot[, 9:10], 2, max)
  )) %>%
    as.data.frame() %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), function(x) {
        if (is_convertible_to_numeric(x)) {
          as.numeric(x)
        } else {
          x
        }
      })
    )
}

# from cb19
meds <- liss_merged$health %>%
  dplyr::select(
    any_meds = ch00a184,
    cholesterol = ch00a169,
    blood_pressure = ch00a170,
    heart_brain = ch00a171,
    diabetes = ch00a174,
    dplyr::everything()
  ) %>%
  dplyr::mutate(any_meds = abs(any_meds - 1))

meds_df <- meds %>%
  dplyr::select(
    Any = any_meds,
    Cholesterol = cholesterol,
    `Blood Pressure` = blood_pressure,
    `Heart/Brain` = heart_brain,
    Diabetes = diabetes
  ) %>%
  table() %>%
  as.data.frame() %>%
  dplyr::arrange(dplyr::desc(Freq)) %>%
  dplyr::filter(Freq > 0)

knitr::kable(meds_df,
  format = "markdown",
  caption = "Frequency of Medication Use Among Respondents"
)

# from cb20
med_cols <- c(
  "any_meds",
  "cholesterol",
  "blood_pressure",
  "heart_brain",
  "diabetes"
)

meds_df <- meds %>%
  dplyr::select(wavenr, dplyr::all_of(med_cols)) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::arrange(wavenr)

pooled <- list(mokken_assumptions(meds_df[-1], "pooled"))
by_wave <- meds_df %>%
  dplyr::group_by(wavenr) %>%
  dplyr::group_split() %>%
  purrr::map(~ mokken_assumptions(
    .x[, -1],
    dplyr::first(.x$wavenr)
  ))

mokken_table <- do.call(rbind, c(by_wave, pooled))
colnames(mokken_table) <- c(
  "$t$", "$H_{ij}\\\ (min)$",
  "$H_{i}\\\ (min)$", "$H$", "$z_{\\text{sig}}$", "$crit$"
)

meds$medicine <- ifelse(complete.cases(meds[, med_cols]),
  rowSums(meds[, med_cols]),
  NA_integer_
)

dplyr::select(meds,
  Any = any_meds,
  Cholesterol = cholesterol,
  `Blood Pressure` = blood_pressure,
  `Heart/Brain` = heart_brain,
  Diabetes = diabetes,
  `Medication Use (scale)` = medicine
) %>%
  describe(
    caption = "Summary statistics of medication variables"
  )

# from cb21
knitr::kable(mokken_table,
  format = "markdown", digits = 4,
  caption = paste(
    "Mokken scale analysis of medication",
    "variables across waves and pooled sample"
  )
)

# from cb22
liss_merged$meds <- meds
rm(meds)

# from cb23
sport <- liss_merged$sport
sport <- sport[, c(1, 2, 3, 86, 87)] %>%
  dplyr::rename(
    active = cs00a104,
    hours = cs00a105
  ) %>%
  type.convert(as.is = TRUE)

sport$active <- ifelse(sport$active == 2, 0, sport$active)

na_rows <- sport$active == 0 & is.na(sport$hours)
sport$hours[na_rows] <- 0

table(sport$active, useNA = "ifany") %>%
  cbind.data.frame() %>%
  setNames(c("Value", "Frequency")) %>%
  knitr::kable(
    format = "markdown",
    caption = "Frequency distribution of sports participation"
  )

# from cb24
sport_by_id <- split(sport, sport$nomem_encr)

has_na <- sapply(sport_by_id, function(x) {
  any(x$active == 1 & is.na(x$hours)) &
    nrow(na.omit(x)) > 0
})

incomplete <- names(which(has_na))
for (i in incomplete) {
  act <- sport_by_id[[i]]$active == 1
  hrs <- sport_by_id[[i]]$hours[act]
  imputed <- mean(hrs, na.rm = TRUE)
  na_rows <- sport_by_id[[i]]$active == 1 &
    is.na(sport_by_id[[i]]$hours)
  sport_by_id[[i]]$hours[na_rows] <- imputed
}

sport_imp <- do.call(rbind, sport_by_id)
describe(sport_imp,
  exclude = c("nomem_encr", "wave", "wavenr"),
  caption = "Summary statistics for sports participation"
)

# from cb25
liss_merged$sport <- sport_imp
rm(sport)

# from cb26
bmi <- health %>%
  dplyr::select(nomem_encr, wave, wavenr, ch00a016, ch00a017) %>%
  dplyr::rename(cm = ch00a016, kilo = ch00a017) %>%
  dplyr::mutate_at(rlang::quos(cm, kilo), as.numeric)

background <- liss_merged$background %>%
  dplyr::rename(gender = geslacht)

bmi <- dplyr::inner_join(
  bmi,
  background[c("nomem_encr", "wave", "gender")],
  by = c("nomem_encr", "wave")
)

# from cb28
corr_md <- correlation::correlation(bmi[c("kilo", "cm", "gender")])
corr_md$p <- sapply(corr_md$p, p_to_asterisk, type = "latex", truncate_p = TRUE)

bmi_out <- capture.output(corr_md)
table_caption <- gsub("# ", "", crayon::strip_style(bmi_out)[1])
names(corr_md) <- c(
  "$\\text{Var}_1$", "$\\text{Var}_2$", "$r$", "$CI$", "$CI_{low}$",
  "$CI_{high}$", "$t$", "$df_{error}$",
  "$p$", "$\\text{Method}$", "$n_{Obs}$"
)
corr_md <- knitr::kable(corr_md,
  format = "markdown",
  digits = 3,
  caption = table_caption
)
footnotes <- bmi_out[-seq_len(max(which(bmi_out == "")))][1]
kableExtra::footnote(corr_md) %>%
  kableExtra::add_footnote(footnotes, notation = "number")

# from cb30
condition <- bmi$cm <= 38 | bmi$cm == 100

bmi$cm[which(condition)] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("cm", "n")) %>%
  knitr::kable(
    caption =
      paste0("Frequencies (n=", length(which(condition)), ")")
  )

# from cb31
bmi$cm <- ifelse(condition, NA, bmi$cm)

# from cb32
condition <- bmi$cm < 100 & !is.na(bmi$cm)

bmi$cm[which(condition)] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("cm", "n")) %>%
  knitr::kable(
    caption =
      paste0("Frequencies (n=", length(which(condition)), ")")
  )

# from cb33
bmi$cm <- ifelse(condition, bmi$cm + 100, bmi$cm) # 107 cases

# from cb34
condition <- bmi$kilo <= 20 & !is.na(bmi$kilo)

bmi$cm[which(condition)] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("kilo", "n")) %>%
  knitr::kable(
    caption =
      paste0("Frequencies (n=", length(which(condition)), ")")
  )

# from cb35
bmi$kilo <- ifelse(condition, NA, bmi$kilo)

# from cb36
condition <- bmi$kilo >= 600 & !is.na(bmi$kilo)
bmi$kilo[which(condition)] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("kilo", "n")) %>%
  knitr::kable(caption = paste0("Frequencies (n=", length(which(condition)), ")"))

# from cb37
bmi$kilo <- ifelse(condition, bmi$kilo / 10, bmi$kilo) # 15 cases

# from cb38
as_finite <- function(x) {
  na.omit(x)[!sapply(na.omit(x), is.infinite)]
}
locf_imputation <- function(df, imp) {
  df[is.na(df[imp]), imp] <- 1
  imputeTS::na_locf(df)
}
bmi[c("cm_outs", "kg_outs", "cm_imp", "kg_imp")] <- 0

# from cb39
condition <- bmi$gender == 2 & bmi$cm >= 205
bmi$cm[which(condition)] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("cm", "n")) %>%
  knitr::kable(
    caption =
      paste0("Frequencies (n=", length(which(condition)), ")")
  )

# from cb40
bmi$cm_outs[which(condition)] <- bmi$cm[which(condition)]
bmi$cm[which(condition)] <- bmi$cm[which(condition)] - 100

# from cb41
update_kilo <- function(df, id, t, from, to) {
  df$kilo[df$nomem_encr == id &
    df$wavenr == t &
    df$kilo == from] <- to
  return(df)
}
bmi <- bmi %>%
  update_kilo(834051, 3, 198, 98) %>%
  update_kilo(884957, 7, 30, 130) %>%
  update_kilo(880023, 12, 150, 73) %>%
  update_kilo(851831, 9, 178, 78) %>%
  update_kilo(831413, 11, 175, 75) %>%
  update_kilo(883654, 2, 190, 90) %>%
  update_kilo(884271, 6, 175, 75)

# from cb42
per_id <- split(bmi, bmi$nomem_encr)
impute_missing_cm <- function(y, f = median) {
  finite_cm <- as_finite(y$cm)
  if (length(finite_cm) > 1) {
    y$cm_imp[is.na(y$cm)] <- 1
    y$cm <- f(finite_cm)
  }
  return(y)
}
max_cm_diff <- sapply(per_id, function(y) {
  finite_cm_diff <- diff(as_finite(y$cm))
  if (length(finite_cm_diff) > 0) {
    return(max(abs(finite_cm_diff)))
  } else {
    return(NA)
  }
})
bmi$cm[max_cm_diff] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("cm", "n")) %>%
  knitr::kable(
    caption = paste0("Frequencies (n=", length(max_cm_diff), ")"),
    format = "markdown"
  )

# from cb43
cm_indices <- which(sapply(per_id, function(y) {
  length(as_finite(y$cm)) > 2 && max(abs(diff(as_finite(y$cm))) > 0)
}))
bmi$cm[cm_indices] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("cm", "n")) %>%
  knitr::kable(
    caption = paste0("Frequencies (n=", length(cm_indices), ")"),
    format = "markdown"
  )

# from cb44
per_id[cm_indices] <- lapply(per_id[cm_indices],
  impute_missing_cm,
  f = median
)
cm_variation_indices <- which(sapply(per_id, function(y) {
  finite_cm <- as_finite(y$cm)
  return(length(finite_cm) > 1 && max(abs(diff(finite_cm))) > 0)
}))
per_id[cm_variation_indices] <- lapply(
  per_id[cm_variation_indices], impute_missing_cm,
  f = mean
)

# from cb45
kilo_outlier_indices <- sapply(per_id, function(y) {
  finite_kilo_diff <- quantmod::Delt(as_finite(y$kilo), type = "log")
  if (length(finite_kilo_diff) > 0) {
    return(round(max(abs(finite_kilo_diff)), 2) >= 0.65)
  } else {
    return(FALSE)
  }
})
kilo_outlier_indices <- which(kilo_outlier_indices)
length(kilo_outlier_indices)

# from cb47
normalize_kilo <- function(y) {
  k <- abs(log(abs(y$kilo - median(as_finite(y$kilo)))))
  k <- which(is.finite(k) & k > 3)
  for (k_index in k) {
    y$kg_imp[k_index] <- y$kilo[k_index]
    if (y$kilo[k_index] - 100 >= min(as_finite(y$kilo[-k]))) {
      y$kilo[k_index] <- y$kilo[k_index] - 100
    } else {
      y$kilo[k_index] <- mean(as_finite(y$kilo[-k]))
    }
  }
  return(y)
}
per_id[kilo_outlier_indices] <- lapply(
  per_id[kilo_outlier_indices],
  normalize_kilo
)

# from cb48
na_kilo_indices <- which(sapply(per_id, function(y) {
  any(is.na(y$kilo)) & any(!is.na(y$kilo))
}))
bmi$kilo[na_kilo_indices] %>%
  table() %>%
  cbind.data.frame() %>%
  setNames(c("kilo", "n")) %>%
  knitr::kable(
    caption = paste0("Frequencies (n=", length(na_kilo_indices), ")"),
    format = "markdown"
  )

# from cb49
per_id[na_kilo_indices] <- lapply(
  per_id[na_kilo_indices],
  locf_imputation, "kg_imp"
)
bmi <- do.call(rbind, per_id)
bmi$bmi <- with(bmi, ifelse(
  !is.na(kilo) & !is.na(cm),
  kilo / (cm / 100)^2,
  NA
))
liss_merged$bmi <- bmi
rm(bmi)

# from cb50
liss <- liss_merged %>%
  `[`(-which(names(.) == "background")) %>%
  purrr::reduce(dplyr::full_join,
    by = join_by(nomem_encr, wavenr)
  )

# `liss` now holds the merged long frame (old cb50 output).
# write only when run as a build; verification sources this with writing off.
if (isTRUE(getOption("rhd.write_merged", TRUE))) {
  out_path <- fs::path(liss_data_dir(), "liss_merged_long.sav")
  haven::write_sav(liss, out_path)
  message("wrote ", out_path)
}
