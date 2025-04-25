main.pdf latex.out/main.aux: main.tex references.bib figures/output/regions_diag.pdf figures/output/challenges.pdf figures/output/sim-results.pdf figures/output/CIS_final.pdf figures/output/CIS_vary.pdf .PHONY
	python3 latexrun $<

supplemental.pdf: supplemental.tex latex.out/main.aux references.bib figures/output/prior_predictive_survival.pdf  figures/output/CIS_ntot.pdf figures/output/table1.tex .PHONY
	python3 latexrun $<

all: main.pdf supplemental.pdf

figures/output/challenges.pdf: figures/R/challenges.R
	Rscript $<

figures/output/regions_diag.pdf: figures/R/regions_diag.R
	Rscript $<

figures/output/sim-results.pdf: figures/R/sim_survival.R data/all_posteriors.rds data/input_curves.rds
	Rscript $<

figures/output/CIS_final.pdf figures/output/CIS_vary.pdf: figures/R/CIS_survival.R figures/R/utils.R data/STATS17701/draws.rds
	Rscript $<

figures/output/CIS_ntot.pdf: figures/R/CIS_ntot.R figures/R/utils.R data/STATS18744/means.rds
	Rscript $<

figures/output/prior_predictive_survival.pdf: figures/R/surv_priors.R figures/R/utils.R
	Rscript $<

figures/output/table1.tex: figures/R/demographics_table.R data/STATS22084/age-groups-table.csv data/STATS22084/sex-groups-table.csv data/STATS22084/eth-groups-table.csv data/STATS22084/gor_name-groups-table.csv data/STATS22084/hhsize-groups-table.csv data/pop-estimates-2020.csv data/by-ethnicity-5-groups-table.csv
	Rscript $<

.PHONY: FORCE