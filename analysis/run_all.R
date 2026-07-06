#!/usr/bin/env Rscript
# render the RI-CLPM analysis as a GitHub-renderable transcript

# ---- 0. packages ----
pkgs <- c(
  "broom", "DescTools", "dplyr", "fs", "haven", "here", "kableExtra", "knitr",
  "lavaan", "magrittr", "psych", "purrr", "rlang", "rmarkdown", "semTools",
  "sjlabelled", "stringr", "tibble", "tidyr", "tidyselect", "cli",
  "data.table", "report"
)
missing <- setdiff(pkgs, rownames(utils::installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

# semTable (archived from CRAN on 2024-03-24) and minvariance (GitHub only) are
# not installable from CRAN; fetch them from their real sources, once.
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("minvariance", quietly = TRUE)) {
  remotes::install_github("milanwiedemann/minvariance", upgrade = "never")
}
if (!requireNamespace("weasel", quietly = TRUE)) {
  remotes::install_github("siardv/weasel", upgrade = "never")
}
if (!requireNamespace("lissr", quietly = TRUE)) {
  remotes::install_github("siardv/lissr", upgrade = "never")
}

# ---- 1. render the transcript ----
if (!rmarkdown::pandoc_available()) {
  stop("pandoc not found: install pandoc or render run_all.Rmd inside RStudio")
}
rmarkdown::render("run_all.Rmd", envir = new.env(), quiet = TRUE)
cat("wrote run_all.md and figures under run_all_files/\n")
