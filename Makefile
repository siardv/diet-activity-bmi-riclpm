R := Rscript

.PHONY: all acquire panel sample frve audit mokken analysis release-tables vignettes clean

all: analysis vignettes

acquire:
	$(R) scripts/00_acquire_data.R

panel:
	$(R) scripts/01_build_panel.R

sample:
	$(R) scripts/02_select_sample.R

frve:
	$(R) scripts/04_frve_reliability.R

audit:
	$(R) scripts/05_bmi_construction_audit.R

mokken:
	$(R) scripts/07_medication_mokken.R

analysis:
	$(R) scripts/03_run_analysis.R

# copy the rendered result tables into the released root tables/ so the two
# locations cannot drift silently; run after every `make analysis`
release-tables:
	cp -p analysis/tables/*.csv tables/

vignettes:
	quarto render vignettes

# remove rendered site output, quarto caches, and per-render analysis
# intermediates; released artefacts in output/ and tables/ are never touched
clean:
	rm -rf vignettes/_site vignettes/.quarto
	rm -f vignettes/*.html
	rm -f analysis/liss_subset.RData analysis/liss_subset_wide.RData analysis/all_model_syntax.RData
