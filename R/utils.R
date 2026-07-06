#' build a list whose names default to the argument expressions
#'
#' @param ... values to collect; unnamed arguments take their expression as name
#' @return a named list
#' @export
named_list <- function(...) {
  infered_names <- rlang::enquos(...)
  values <- list(...)
  given_names <- names(values)
  infered_names <- as.character(sapply(infered_names, rlang::as_label))
  given_names[given_names == ""] <- infered_names[given_names == ""]
  stats::setNames(lapply(values, eval), given_names)
}

#' count the leading-zero significant digits of a numeric vector
#'
#' @param input_vector numeric vector
#' @param return_n_digits if TRUE, return the digit count instead of the input
#' @return the input vector, or the number of leading-zero digits
#' @export
round_sig <- function(input_vector, return_n_digits = FALSE) {
  decimal_places <- gsub(".*\\.", "", as.character(min(abs(input_vector), na.rm = TRUE)))
  num_leading_zeros <- Filter(f = Negate(nchar), strsplit(decimal_places, "0")[[1]])
  n_digits <- length(num_leading_zeros)
  if (return_n_digits) {
    return(n_digits)
  }
  input_vector
}

#' format a chi-bar-square difference-test result for reporting
#'
#' @param out output of a chi-bar-square difference test
#' @return a list with elements `cr` (critical value), `we` (weights), `no` (note)
#' @export
nice_chi_bar_diff <- function(out) {
  cr <- paste0(
    "The critical value for the $\\bar{\\chi}^2$ difference test is $",
    round(out$critical_value, 3), "$."
  )
  we <- paste0(
    "The $\\bar{\\chi}^2$ weights used in the test are: ",
    paste0("$", as.character(round(out$ChiBar2_weights, 3)), "$", collapse = ", "), "."
  )
  no <- out$message
  no <- gsub("([0-9]+)", "$\\1$", no)
  no <- gsub(" u ", " $u$ ", no)
  no <- gsub(" k ", " $k$ ", no)
  no <- gsub("\\+", "$+$", no)
  no <- gsub("=", "$=$", no)
  no <- gsub("Chi-bar-square", "$\\\\bar{\\\\chi}^2$", no)
  no <- gsub("Chi2's", "$\\\\chi^2$'s", no)
  no <- gsub("p-value", "$p$-value", no)
  no <- gsub("\\$ \\$", " ", no)
  no <- gsub("\\s+", " ", no)
  no <- paste("_Note_: ", no)
  list(cr = cr, we = we, no = no)
}

#' squared standardized residual covariances per group
#'
#' @param grouped_model_fit a fitted multigroup lavaan model
#' @return a list with one numeric vector per group, the squared diagonal
#'   z-statistics of the residual covariances
#' @export
max_std_residual_variance <- function(grouped_model_fit) {
  # calculate standardized residuals
  std_residuals <-
    lavaan::lavResiduals(
      object = grouped_model_fit,
      zstat = TRUE
    )

  # get group names
  group_names <- names(std_residuals)

  # function to extract covariance matrix for each group
  get_covariance_matrix <- function(group) {
    std_residuals[[group]]$cov.z
  }

  # calculate covariance matrices for each group
  covariance_matrices <- lapply(group_names, get_covariance_matrix)

  # Extract diagonal elements and square them
  squared_diagonals <-
    lapply(
      covariance_matrices,
      function(cov_matrix) {
        diag(cov_matrix)^2
      }
    )

  # take the maximum across groups
  # list(sapply(squared_diagonals, Reduce, f = pmax))
  return(squared_diagonals)
}
