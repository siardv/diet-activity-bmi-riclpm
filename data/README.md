# data/

This folder is intentionally empty in the release. LISS microdata may not be redistributed.

Expected contents after `scripts/00_acquire_data.R`:

- per-wave module files, flat in this folder as unzipped by `lissr::liss_download(selection, .dir = "data")` with waves `1:7` selected: `ch07a_2p_EN.sav` through `ch13g_EN_1.0p.sav` for health, `cs08a` through `cs14g` for social integration and leisure, plus the English codebook PDFs if selected
- the monthly Background Variables files for the main fieldwork month (November) of each health wave, downloaded separately from the archive (the module downloader covers the ten core modules only) and unzipped anywhere under this folder, flat or in per-zip subfolders: `avars_200711_EN_3.0p.zip`, `avars_200811_EN_2.0p.zip`, `avars_200911_EN_2.0p.zip`, `avars_201011_EN_2.0p.zip`, `avars_201111_EN_2.0p.zip`, `avars_201211_EN_1.0p.zip`, `avars_201311_EN_1.0p.zip`

In addition, for the analysis itself:

- `liss_merged_long.sav`, the merged long panel the analysis reads; this committed file is the canonical Route A input, placed here rather than regenerated (`scripts/01_build_panel.R` checks for it; see vignette 02 for the provenance and lissr routes)

Access to the LISS Data Archive (https://www.lissdata.nl) is free for research after registration and a signed data-use statement. Set the environment variable `LISS_DATA_DIR` if the data live outside this folder.
