R := Rscript

.PHONY: all acquire panel sample frve analysis vignettes clean

all: analysis vignettes

acquire:
	$(R) scripts/00_acquire_data.R

panel:
	$(R) scripts/01_build_panel.R

sample:
	$(R) scripts/02_select_sample.R

frve:
	$(R) scripts/04_frve_reliability.R

analysis:
	$(R) scripts/03_run_analysis.R

vignettes:
	quarto render vignettes

clean:
	rm -rf output/* vignettes/*_files vignettes/*.html
