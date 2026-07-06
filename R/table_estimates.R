#' per-group path-estimate table
#'
#' @param fit a fitted lavaan model (single- or multi-group)
#' @param keep_p_values if TRUE keep numeric p-values, else show stars
#' @param lagged_effects_only if TRUE keep only single-step lagged effects
#' @param as_markdown if TRUE return a markdown kable, else a data frame
#' @param table_caption caption used when as_markdown is TRUE
#' @return a data frame, or a markdown kable when as_markdown is TRUE
#' @export
table_estimates <- function(
    fit,
    keep_p_values = FALSE,
    lagged_effects_only = FALSE,
    as_markdown = FALSE,
    table_caption = NULL) {

  format_p_values <- function(.df, .as_numeric = FALSE) {
    if (.as_numeric) {
      return(.df)
    }
    replace_p_value <- function(x) {
      if (x <= 0.001) {
        return("***")
      } else if (x <= 0.01) {
        return("**")
      } else if (x <= 0.05) {
        return("*")
      } else {
        return("")
      }
    }
    return(sapply(.df, replace_p_value))
  }

  if (fit@pta$ngroups == 1) {
    df_out <- lavaan::parameterEstimates(fit,
      pvalue = TRUE,
      output = "data.frame", standardized = TRUE
    ) %>%
      dplyr::filter(op == "~") %>%
      dplyr::mutate(effect = paste(lhs, op, rhs)) %>%
      dplyr::select(
        -c(1:3), -se, -z, -tidyselect::starts_with("ci"),
        -tidyselect::starts_with("std")
      ) %>%
      dplyr::select(effect, tidyselect::everything()) %>%
      dplyr::rename(p_value = pvalue)
  } else {
    df_out <- lavaan::parameterEstimates(fit,
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
      dplyr::group_split(.keep = FALSE) %>%
      purrr::map2(.y = seq_along(.), ~ {
        if ("label" %in% names(.x)) {
          x <- dplyr::select(.x, -label)
        } else {
          x <- .x
        }
      })

    names_ls <- list(
      rep("effect", length(df_out)),
      paste0("est_SES_", seq_along(df_out)),
      paste0("p_SES_", seq_along(df_out))
    ) %>%
      purrr::transpose() %>%
      lapply(unlist, recursive = FALSE)

    df_out <- purrr::map2(df_out, names_ls, ~ purrr::set_names(.x, .y))

    df_out <- df_out %>%
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
      )

    df_out <- df_out %>%
      dplyr::select(
        effect, tidyselect::ends_with("_1"),
        tidyselect::ends_with("_2"),
        tidyselect::ends_with("_3")
      ) %>%
      as.data.frame()
  }

  if (!keep_p_values) {
    df_out <- df_out %>%
      dplyr::mutate(dplyr::across(
        tidyselect::starts_with("p_"), ~ format_p_values(.)
      ))
  }



  if (lagged_effects_only) {
    df_out$lagged_effects_only <- sapply(df_out$effect, function(r) {
      gsub("[w~0-9]+", "", r) %>%
        gsub("\\s+", " ", .) %>%
        strsplit(" ") %>%
        `[[`(1) %>%
        unique() %>%
        length() %>%
        `-`(1)
    }, USE.NAMES = FALSE)
    df_out <- df_out %>%
      dplyr::filter(lagged_effects_only == 1) %>%
      dplyr::select(-lagged_effects_only)
  }
  if (as_markdown) {
    cap <- ifelse(is.null(table_caption), "", table_caption)
    knitr::kable(df_out, format = "markdown", digits = 3, caption = cap)
  } else {
    return(df_out)
  }
}
