#!/usr/bin/env Rscript
# provide the merged long panel the analysis reads
#
# routes (RHD_BUILD_ROUTE):
#   historical (default)  the committed liss_merged_long.sav is the canonical,
#                         audit-pinned analysis input; this route checks it is
#                         in place
#   provenance            sources scripts/build_liss_merged.R, the preserved
#                         record of the original construction; runs only
#                         against the original extract schema (see its header)
#   lissr                 recipe-driven merge from the raw per-wave modules;
#                         the forward path, documented in vignette 02

route <- Sys.getenv("RHD_BUILD_ROUTE", "historical")
data_dir <- Sys.getenv("LISS_DATA_DIR", "data")

if (route == "historical") {
  merged <- file.path(data_dir, "liss_merged_long.sav")
  if (file.exists(merged)) {
    message("canonical input in place: ", merged)
    message("optional: scripts/02_select_sample.R runs the standalone selection audit in minutes")
    message("then scripts/03_run_analysis.R renders the full analysis")
  } else {
    stop(
      "liss_merged_long.sav not found in ", data_dir, "\n",
      "  copy the committed file there; it is the canonical analysis input.\n",
      "  regenerating it from the current extracts is out of scope by design:\n",
      "  scripts/build_liss_merged.R is a preserved construction record that\n",
      "  targets the original extract schema (see its header). holders of the\n",
      "  frozen originals can set RHD_BUILD_ROUTE=provenance.",
      call. = FALSE
    )
  }
} else if (route == "provenance") {
  source(file.path("scripts", "build_liss_merged.R"), chdir = FALSE)
} else {
  ch <- lissr::merge_liss_module(
    lissr::liss_recipe("ch"),
    data_dir = data_dir, output_dir = "output"
  )
  cs <- lissr::merge_liss_module(
    lissr::liss_recipe("cs"),
    data_dir = data_dir, output_dir = "output"
  )
  panel <- lissr::merge_liss_panel(
    list(ch, cs),
    join_by = c("nomem_encr", "wave_year")
  )

  # demographics live in the monthly background variables files, one per
  # wave (the november fieldwork snapshots; see vignette 02 for the list
  # and the wave_year join on nomem_encr)

  saveRDS(panel, file.path("output", "ch_cs_panel.rds"))

  # note: the published analysis consumed the archived extracts, in which the
  # fruit-and-vegetable composite (frve) and bmi were already derived; deriving
  # them from the raw items is documented in vignette 02 and must be validated
  # against the extract before substitution
}
