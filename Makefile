# The manuscript figures, as submitted. SVG is the only vector format that can
# express these plots' semi-transparency, so nothing on the page is ever
# flattened to a bitmap; EPS and the classic postscript() device both force
# that. Figure order matches their citation order in main.tex.
SUBMISSION_FIGURES = figures/output/challenges.svg figures/output/regions_diag.svg \
  figures/output/sim-results.svg figures/output/CIS_final.svg figures/output/CIS_vary.svg

SUPP_FIGURES = figures/output/prior_predictive_survival.svg figures/output/CIS_ntot.svg

FIGURE_OUTPUTS = $(SUBMISSION_FIGURES) $(SUPP_FIGURES) figures/output/table1.tex

# pdflatex cannot read SVG itself. main.tex loads the `svg` package, which
# converts each figure during the LaTeX run (hence -shell-escape) and caches the
# result under svg-inkscape/. That cache is scratch: gitignored, never
# submitted. figures/output holds the SVG, which is the figure as published.

main.pdf latex.out/main.aux: main.tex references.bib $(SUBMISSION_FIGURES) .PHONY
	python3 latexrun --latex-args=-shell-escape $<

supplemental.pdf: supplemental.tex latex.out/main.aux references.bib $(SUPP_FIGURES) figures/output/table1.tex .PHONY
	python3 latexrun --latex-args=-shell-escape $<

main-diff.pdf: main-diff.tex
	python3 latexrun --latex-args=-shell-escape $<

main-diff.tex: main.pdf main-old.tex
	latexdiff --append-mboxsafecmd=autocite,textcite,cref --math-markup=3 main-old.tex main.tex > $@

all: main.pdf supplemental.pdf $(SUBMISSION_FIGURES)

# Assemble the files for the final Biometrics submission into
# submissions/2026-08_Biometrics-final. See make-submission.sh.
submission: main.pdf supplemental.pdf $(SUBMISSION_FIGURES) FORCE
	./make-submission.sh

figures/output/challenges.svg: figures/R/challenges.R figures/R/utils.R
	Rscript $<

figures/output/regions_diag.svg: figures/R/regions_diag.R figures/R/utils.R
	Rscript $<

figures/output/sim-results.svg: figures/R/sim_survival.R figures/R/utils.R data/all_posteriors.rds data/input_curves.rds
	Rscript $<

figures/output/CIS_final.svg figures/output/CIS_vary.svg: figures/R/CIS_survival.R figures/R/utils.R data/STATS17701/draws.rds
	Rscript $<

figures/output/CIS_ntot.svg: figures/R/CIS_ntot.R figures/R/utils.R data/STATS18744/means.rds
	Rscript $<

figures/output/prior_predictive_survival.svg: figures/R/surv_priors.R figures/R/utils.R
	Rscript $<

figures/output/table1.tex: figures/R/demographics_table.R data/STATS22850/age-groups-table.csv data/STATS22850/sex-groups-table.csv data/STATS22850/eth-groups-table.csv data/STATS22850/gor_name-groups-table.csv data/STATS22850/hhsize-groups-table.csv data/pop-estimates-2020.csv data/by-ethnicity-5-groups-table.csv
	Rscript $<

.PHONY: FORCE
