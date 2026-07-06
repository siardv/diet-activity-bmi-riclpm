#' resolve the liss data directory
#'
#' @return a length-one path string
#' @export
liss_data_dir <- function() {
  Sys.getenv("LISS_DATA_DIR", unset = here::here("data"))
}

#' read a liss spss file as a plain data frame
#'
#' reads one `.sav` from the configured data directory, strips value labels, and
#' returns a base data frame. there is no path discovery and no hardcoded object
#' name; the file and directory are explicit arguments.
#'
#' @param file file name within the data directory, e.g. "liss_merged_long.sav"
#' @param data_dir directory holding the file; defaults to [liss_data_dir]
#' @return a data frame with value labels removed
#' @importFrom magrittr %>%
#' @export
read_liss <- function(file, data_dir = liss_data_dir()) {
  path <- fs::path(data_dir, file)
  if (!fs::file_exists(path)) {
    stop("liss file not found: ", path, call. = FALSE)
  }
  haven::read_spss(path, user_na = TRUE) %>%
    sjlabelled::remove_all_labels() %>%
    as.data.frame()
}
