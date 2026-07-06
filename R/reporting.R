#' descriptive-statistics table with a normality test
#'
#' @param df a data frame
#' @param exclude optional names of columns to drop before describing
#' @param kable_format passed to knitr::kable (default "html")
#' @param caption table caption
#' @param normality if TRUE, include the normality column and footnote
#' @param print_only if TRUE, return the rendered table; if FALSE, the data frame
#' @param add_footnote if TRUE, add the normality-test footnote (html only)
#' @param latex retained for signature parity; headers are plain text regardless
#' @param copy_to_clipboard if TRUE, copy the table via clipr (optional)
#' @return a kable (print_only = TRUE) or the underlying data frame
#' @export
describe <- function(df, exclude = NULL, kable_format = "html", caption = "Descriptive statistics", normality = TRUE, print_only = TRUE, add_footnote = TRUE, latex = TRUE, copy_to_clipboard = FALSE) {
  if (!is.null(exclude)) {
    df <- dplyr::select(df, -dplyr::any_of(exclude))
  }

  normality_cols <- sapply(as.data.frame(df), function(.) {
    all(is.numeric(.)) & length(unique(.)) > 4
  })

  normality_test <- purrr::map2_dfr(normality_cols, names(normality_cols), ~ {
    if (.x) {
      if (nrow(df) > 5000) {
        test_result <- DescTools::LillieTest(df[[.y]])
      } else {
        test_result <- stats::shapiro.test(df[[.y]])
      }
      tibble::tibble(
        variable = .y,
        normality = test_result$statistic,
        p_value = test_result$p.value
      )
    } else {
      tibble::tibble(variable = .y, normality = NA, p_value = NA)
    }
  }, .id = "variable")

  out <- psych::describe(df, ranges = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-vars) %>%
    cbind(normality_test) %>%
    dplyr::select(
      " " = variable,
      "n" = n,
      "mean" = mean,
      "min" = min,
      "max" = max,
      "sd" = sd,
      "skew" = skew,
      "kurtosis" = kurtosis,
      "SE" = se,
      "Normality" = normality,
      "p" = p_value
    ) %>%
    `rownames<-`(NULL)

  out$p <- sapply(out$p, function(p) {
    ifelse(is.na(p),
      "\u2014",
      format(
        ifelse(p < 0.001, "<0.001", round(p, 3)),
        nsmall = 3,
        justify = "right"
      )
    )
  })

  if (!normality) {
    x <- grep("Normality", names(out))
    out <- out[, -c(x, x + 1)]
  }

  options(knitr.kable.NA = "\u2014")

  out[, -1] <- lapply(out[, -1], function(x) {
    if (is.numeric(x)) round(x, 3) else x
  })

  out_table <- knitr::kable(out,
    format = kable_format,
    digits = 3,
    row.names = FALSE,
    caption = caption,
    align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")
  )

  if (normality && add_footnote && kable_format == "html") {
    out_table <- kableExtra::footnote(out_table,
      general = paste(
        "Normality:",
        ifelse(nrow(df) <= 5000,
          "Shapiro-Wilk Test",
          "Lilliefors (Kolmogorov-Smirnov) Test"
        )
      ), footnote_as_chunk = TRUE
    )
  }

  if (print_only) {
    if (copy_to_clipboard) {
      if (!requireNamespace("clipr", quietly = TRUE)) {
        stop("copy_to_clipboard = TRUE requires the 'clipr' package.", call. = FALSE)
      }
      clipr::write_clip(out_table)
    } else {
      out_table
    }
  } else {
    out
  }
}

#' compare path estimates across SES groups
#'
#'
#' @param fit_model a fitted multigroup lavaan model
#' @param include_p_values if TRUE keep numeric p-values, else show stars
#' @param hide_non_significant if TRUE blank estimates with p > 0.1
#' @param only_lagged_effect if TRUE keep only single-step lagged effects
#' @return a data frame of effects with per-group estimate and p-value columns
#' @export
compare_ses_groups <- function(fit_model, include_p_values, hide_non_significant, only_lagged_effect) {
  convert_p_value <- function(df, as_numeric = FALSE) {
    if (as_numeric) {
      return(df)
    }
    replace_p_value_with_significance <- function(x) {
      ifelse(x <= 0.001, "***",
        ifelse(x <= 0.01, "**",
          ifelse(x <= 0.05, "*",
            ifelse(x <= 0.1, ".", "")
          )
        )
      )
    }
    sapply(df, replace_p_value_with_significance)
  }
  missing_to_false <- function(x) {
    ifelse(missing(x), FALSE, x)
  }

  include_p_values <- missing_to_false(include_p_values)
  hide_non_significant <- missing_to_false(hide_non_significant)
  only_lagged_effect <- missing_to_false(only_lagged_effect)

  lavaan::parameterEstimates(fit_model,
    pvalue = TRUE,
    output = "data.frame", standardized = TRUE
  ) %>%
    dplyr::filter(op == "~") %>%
    dplyr::mutate(effect = paste(lhs, op, rhs)) %>%
    dplyr::select(
      -c(1:4), -se, -z, -tidyselect::starts_with("ci"),
      -tidyselect::starts_with("std")
    ) %>%
    dplyr::select(effect, tidyselect::everything()) %>%
    dplyr::group_by(group) %>%
    dplyr::group_split(.keep = FALSE) -> output_df
  if (hide_non_significant) {
    output_df <- output_df %>%
      purrr::map(~ dplyr::mutate(.x, est = ifelse(pvalue > 0.1, NA, est)))
  }
  output_df %>%
    purrr::map2(.y = seq_along(.), ~ {
      if ("label" %in% names(.x)) {
        x <- dplyr::select(.x, -label)
      } else {
        x <- .x
      }
      purrr::set_names(x, c(
        "effect",
        paste0("est_SES_", .y), paste0("p_SES_", .y)
      ))
    }) -> output_df
  output_df %>%
    purrr::reduce(dplyr::full_join, by = "effect") %>%
    {
      dplyr::bind_cols(
        dplyr::select(., effect),
        dplyr::select(., -effect) %>% round(3)
      )
    } %>%
    dplyr::select(
      effect, tidyselect::starts_with("est"),
      tidyselect::everything()
    ) -> output_df
  if (!include_p_values) {
    output_df <- output_df %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::starts_with("p_"), ~ convert_p_value(.)
        )
      )
  }
  output_df %>%
    dplyr::select(
      effect, tidyselect::ends_with("_1"),
      tidyselect::ends_with("_2"),
      tidyselect::ends_with("_3")
    ) %>%
    as.data.frame() -> output_df
  if (only_lagged_effect) {
    output_df$lagged_effect <- sapply(output_df$effect, function(r) {
      gsub("[w~0-9]+", "", r) %>%
        gsub("\\s+", " ", .) %>%
        strsplit(" ") %>%
        `[[`(1) %>%
        unique() %>%
        length() %>%
        `-`(1)
    }, USE.NAMES = FALSE)
    output_df <- output_df %>%
      dplyr::filter(lagged_effect == 1) %>%
      dplyr::select(-lagged_effect)
  }
  if (hide_non_significant) {
    output_df <- output_df %>% dplyr::mutate(
      dplyr::across(tidyselect::everything(), ~ as.character(.) %>%
        {
          ifelse(is.na(.), "", .)
        })
    )
  }
  return(output_df)
}

#' two-part model-fit table (parameters and fit indices)
#'
#' @param fit a fitted lavaan model
#' @return a list of two kables: the parameter table and the fit-index table
#' @export
model_fit_table <- function(fit) {
  if (!requireNamespace("report", quietly = TRUE)) {
    stop("model_fit_table() requires the 'report' package.", call. = FALSE)
  }
  p_to_asterisks <- function(p) {
    if (is.na(p)) {
      return("")
    } else if (p <= 0.001) {
      return("***")
    } else if (p <= 0.01) {
      return("**")
    } else if (p <= 0.05) {
      return("*")
    } else if (p <= 0.1) {
      return(".")
    } else {
      return("")
    }
  }

  df <- report::report_table(fit) %>% as.data.frame()
  df$p <- sapply(df$p, p_to_asterisks)
  df$Fit <- as.numeric(df$Fit)
  for (name in names(df)) {
    if (is.numeric(df[[name]])) {
      temp <- round(df[[name]], 3)
      if (length(temp) > 0) {
        if (all(temp == floor(temp), na.rm = TRUE)) {
          df[[name]] <- as.integer(temp)
        } else {
          df[[name]] <- temp
        }
      }
    }
  }

  df$Fit <- as.character(signif(as.numeric(df$Fit), 3))

  df[is.na(df)] <- ""

  if ("Group" %in% names(df)) {
    df <- df %>% dplyr::rename(SES = Group)
  }

  model_fit_rows <- which(nchar(df$Fit) > 0)
  df1 <- df[-c(model_fit_rows[1] - 1, model_fit_rows), ]
  df2 <- df[model_fit_rows, ]

  components <- c("Loading" = "L", "Regression" = "R", "Correlation" = "C")

  df1 <- df1 %>%
    dplyr::select(-Fit) %>%
    dplyr::mutate(Component = stringr::str_replace_all(Component, components))

  note <- paste0(
    paste(components, "=", names(components), collapse = ", "),
    ". Significance: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1"
  )

  table1 <- knitr::kable(df1,
    digits = 3,
    format = "html", row.names = FALSE
  )

  table1 <- table1 %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    kableExtra::footnote(
      general = note,
      general_title = "Note: ",
      footnote_as_chunk = FALSE
    )

  table2 <- df2 %>%
    dplyr::select(Parameter, Fit) %>%
    knitr::kable(digits = 3, format = "markdown", row.names = FALSE)

  return(list(table1, table2))
}

#' console report of common SEM fit indices
#'
#' @param model a fitted lavaan model
#' @param as_report if TRUE, phrase the lines as prose rather than marks
#' @param print_only if TRUE, print via cli; if FALSE, return the lines
#' @return invisibly prints, or returns a character vector of lines
#' @export
report_performance <- function(model, as_report = FALSE, print_only = TRUE) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    bold  <- crayon::bold
    green <- crayon::green
    red   <- crayon::red
  }
  perf <- c("\u2714", "\u2716")
  the <- suggest_a <- fit. <- ""
  if (as_report) {
    the <- "The "
    suggest_a <- "suggest a "
    fit. <- " fit."
    perf <- c("satisfactory", "poor")
  }

  fit_measures <- c()
  `+` <- function(x) {
    assign("fit_measures", c(fit_measures, x), envir = parent.frame())
  }

  header <- "Model Fit Report"

  +paste0("{bold(header)}")
  measures <- as.list(lavaan::fitmeasures(model))
  sig_diff <- "significantly different"
  insig_diff <- "not significantly different"
  +paste0(
    "The model is ",
    ifelse(measures$chisq[1] < 0.001,
      "{green(sig_diff)}",
      "{red(insig_diff)}"
    ),
    " from a baseline model (",
    format(measures$chisq[1], digits = 3), ", p < 0.001)."
  )

  +paste0(
    the, "GFI ",
    "(", format(measures$gfi[1], digits = 3),
    ifelse(measures$gfi[1] > 0.95, " > 0.95", " < 0.95"),
    ") ", suggest_a,
    ifelse(measures$gfi[1] > 0.95,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "NNFI ",
    "(", format(measures$nfi[1], digits = 3),
    ifelse(measures$nfi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$nfi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "AGFI ",
    "(", format(measures$agfi[1], digits = 3),
    ifelse(measures$agfi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$agfi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "RMSEA ",
    "(", format(measures$rmsea[1], digits = 3),
    ifelse(measures$rmsea[1] < 0.05, " < 0.05", " > 0.05"),
    ") ", suggest_a,
    ifelse(measures$rmsea[1] < 0.05,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "NFI ",
    "(", format(measures$nfi[1], digits = 3),
    ifelse(measures$nfi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$nfi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "SRMR ",
    "(", format(measures$srmr[1], digits = 3),
    ifelse(measures$srmr[1] < 0.08, " < 0.08", " > 0.08"),
    ") ", suggest_a,
    ifelse(measures$srmr[1] < 0.08,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "CFI ",
    "(", format(measures$cfi[1], digits = 3),
    ifelse(measures$cfi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$cfi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "RFI ",
    "(", format(measures$rfi[1], digits = 3),
    ifelse(measures$rfi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$rfi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "PNFI ",
    "(", format(measures$pnfi[1], digits = 3),
    ifelse(measures$pnfi[1] > 0.50, " > 0.50", " < 0.50"),
    ") ", suggest_a,
    ifelse(measures$pnfi[1] > 0.50,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  +paste0(
    the, "IFI ",
    "(", format(measures$ifi[1], digits = 3),
    ifelse(measures$ifi[1] > 0.90, " > 0.90", " < 0.90"),
    ") ", suggest_a,
    ifelse(measures$ifi[1] > 0.90,
      "{green(perf[1])}",
      "{red(perf[2])}"
    ), fit.
  )

  if (print_only) {
    cli::cli_alert_info(paste0(fit_measures, collapse = "\n"))
  } else {
    return(fit_measures)
  }
}

#' format a p-value as significance marks (html or latex)
#'
#' internal helper shared by the construction stage (data-raw) and the analysis
#' script. relocated from reciprocal-health-dynamics.R cb27.
#' @param p_value a p-value
#' @param type "html", "latex", or any other value for plain marks
#' @param truncate_p if TRUE, render a very small latex p-value as <= 0.001
#' @return a formatted string
#' @noRd
p_to_asterisk <- function(p_value, type = "html", truncate_p = FALSE) {
  nbsp <- function(x) strrep("&nbsp;", x)
  low_ast <- function(x) strrep("&#x2217;", x)
  formatted_p <- sprintf("%.3f", round(as.numeric(p_value), 3))

  if (is.na(p_value)) {
    ast <- ""
    formatted_p <- ""
  } else if (p_value <= 0.001) {
    ast <- if (type == "html") low_ast(3) else "***"
  } else if (p_value <= 0.01) {
    ast <- if (type == "html") paste0(low_ast(2), nbsp(1)) else "**"
  } else if (p_value <= 0.05) {
    ast <- if (type == "html") paste0(low_ast(1), nbsp(2)) else "*"
  } else if (p_value <= 0.1) {
    ast <- if (type == "html") paste0("<b>&#x2d9;</b>", nbsp(2)) else "\u00b7"
  } else {
    ast <- if (type == "html") nbsp(3) else ""
  }

  if (type == "latex") {
    if (as.numeric(formatted_p) < 0.001 & truncate_p) {
      formatted_p <- paste0("\\leq", "0.001")
    }
    return(paste0(
      "$", formatted_p,
      ifelse(ast != "", paste0("^{", ast, "}"), ""), "$"
    ))
  } else if (type == "html") {
    return(paste0("<sup>", ast, "</sup><td></td>"))
  } else {
    return(paste0(formatted_p, ast))
  }
}
