main.pdf: main.tex references.bib figures/output/regions_diag.pdf figures/output/challenges.pdf .PHONY
	./latexrun $<

figures/output/challenges.pdf: figures/R/challenges.R
	Rscript $<

figures/output/regions_diag.pdf: figures/R/regions_diag.R
	Rscript $<

.PHONY: FORCE