#!/usr/bin/env Rscript
# acquire the liss source files through lissr (interactive; run once)
# archive access is free for research after registration at lissdata.nl

# one-time credential storage (prompts for the password, keyring-backed)
# lissr::liss_store_credentials("<your liss user number>")

lissr::liss_login()

# orient: module catalogue and the module-by-wave availability matrix
# (the matrix and the downloader cover the ten core longitudinal modules)
print(lissr::liss_modules())
print(lissr::liss_wave_matrix())

# interactively tick what the study needs:
#   modules: health + social integration and leisure
#   waves: enter 1:7 (positional archive indices, not year codes; this
#   yields ch07a to ch13g and cs08a to cs14g, whereas 7:13 would fetch
#   2013 to 2020)
#   file types: spss (.sav), optionally the english codebooks
# note: confirming an empty selection returns NULL, and liss_download(NULL)
# offers the entire archive; cancel and reselect instead
selection <- lissr::liss_select()

lissr::liss_download(selection, .dir = "data")

# the monthly background variables file (avars) is not part of the module
# download flow: obtain the november file of each health wave's main
# fieldwork month from the archive (avars_200711_EN_3.0p.zip,
# avars_200811_EN_2.0p.zip, avars_200911_EN_2.0p.zip,
# avars_201011_EN_2.0p.zip, avars_201111_EN_2.0p.zip,
# avars_201211_EN_1.0p.zip, avars_201311_EN_1.0p.zip) and unzip them
# anywhere under data/ (flat or per-zip subfolders); the join locates the
# .sav files recursively and keys on nomem_encr plus the wave's calendar
# year (see vignette 02)

message("downloads complete; see data/README.md for the expected layout")
