# verify that scripts/build_liss_merged.R reproduces the committed merged file.
# reconstructs `liss` from the raw extracts with writing disabled, then compares
# it (rows, columns, values) against read_liss("liss_merged_long.sav").
# run from the repo root in a clean session:
#   Rscript scripts/verify_liss_merged.R

library(magrittr)
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE), source))

# 1. the committed artifact, read exactly as the analysis and run_all read it
from_file <- read_liss("liss_merged_long.sav")

# 2. reconstruct from raw with writing off, isolated in its own environment so the
#    construction's helper redefinitions (mode, rename, ...) do not leak out
old_opt <- options(rhd.write_merged = FALSE)
recon_env <- new.env(parent = globalenv())
sys.source("scripts/build_liss_merged.R", envir = recon_env)
options(old_opt)
reconstructed <- as.data.frame(recon_env$liss)

# 3. harmonise representation: strip value labels, factors -> character, sort rows
#    by key and columns by name, then compare
zap <- function(v) {
  v <- sjlabelled::remove_all_labels(v)
  if (is.factor(v)) as.character(v) else v
}
harmonise <- function(d, key = c("nomem_encr", "wavenr")) {
  d <- as.data.frame(lapply(d, zap), stringsAsFactors = FALSE, check.names = FALSE)
  k <- intersect(key, names(d))
  if (length(k)) d <- d[do.call(order, d[k]), , drop = FALSE]
  rownames(d) <- NULL
  d[order(names(d))]
}
a <- harmonise(from_file)
b <- harmonise(reconstructed)

cat("\n--- dimensions (rows, cols) ---\n")
print(rbind(from_file = dim(a), reconstructed = dim(b)))

cat("\n--- column set differences ---\n")
cat("only in file:         ", paste(setdiff(names(a), names(b)), collapse = ", "), "\n")
cat("only in reconstructed:", paste(setdiff(names(b), names(a)), collapse = ", "), "\n")

common <- intersect(names(a), names(b))
per_col <- do.call(rbind, lapply(common, function(nm) {
  x <- a[[nm]]; y <- b[[nm]]
  if (length(x) != length(y)) {
    return(data.frame(column = nm, equal = FALSE, n_diff = NA_integer_))
  }
  xn <- suppressWarnings(as.numeric(as.character(x)))
  yn <- suppressWarnings(as.numeric(as.character(y)))
  numeric_ok <- !any(is.na(xn) & !is.na(x)) && !any(is.na(yn) & !is.na(y))
  if (numeric_ok) {
    d <- !((is.na(xn) & is.na(yn)) | (abs(xn - yn) < 1e-8))
  } else {
    xc <- as.character(x); yc <- as.character(y)
    d <- (xc != yc) | (is.na(xc) != is.na(yc))
  }
  data.frame(column = nm, equal = !any(d, na.rm = TRUE), n_diff = sum(d, na.rm = TRUE))
}))

cat("\n--- per-column comparison (common columns) ---\n")
print(per_col, row.names = FALSE)

cat("\n--- summary ---\n")
cat(sprintf("rows equal:           %s (file=%d, reconstructed=%d)\n",
            identical(nrow(a), nrow(b)), nrow(a), nrow(b)))
cat(sprintf("column set identical: %s\n", setequal(names(a), names(b))))
cat(sprintf("all values equal:     %s\n", nrow(per_col) > 0 && all(per_col$equal)))

if (requireNamespace("waldo", quietly = TRUE) &&
    !(setequal(names(a), names(b)) && nrow(per_col) > 0 && all(per_col$equal))) {
  cat("\n--- waldo::compare (first differences) ---\n")
  print(waldo::compare(a[common], b[common], max_diffs = 20))
}
