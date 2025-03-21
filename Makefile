main.pdf: main.tex references.bib figures/output/regions_diag.pdf figures/output/challenges.pdf figures/output/sim-results.pdf figures/output/sim-sensitivity.pdf figures/output/CIS_final.pdf figures/output/CIS_vary.pdf .PHONY latex.out/supplemental.aux
	python3 latexrun $<

latex.out/supplemental.aux supplemental.pdf: supplemental.tex references.bib figures/output/prior_predictive_survival.pdf  figures/output/CIS_ntot.pdf .PHONY
	python3 latexrun $<

figures/output/challenges.pdf: figures/R/challenges.R
	Rscript $<

figures/output/regions_diag.pdf: figures/R/regions_diag.R
	Rscript $<

figures/output/sim-results.pdf: figures/R/sim_survival.R data/all_posteriors.rds data/input_curves.rds
	Rscript $<

figures/output/CIS_final.pdf figures/output/CIS_vary.pdf: figures/R/CIS_survival.R figures/R/utils.R data/STATS17701/draws.rds data/ATACCC-posterior.rds
	Rscript $<

figures/output/CIS_ntot.pdf: figures/R/CIS_ntot.R figures/R/utils.R data/STATS18744/means.rds data/ATACCC-posterior.rds
	Rscript $<

figures/output/prior_predictive_survival.pdf: figures/R/surv_priors.R figures/R/utils.R
	Rscript $<

.PHONY: FORCE