main.pdf: main.tex references.bib figures/output/regions_diag.pdf .PHONY
	./latexrun --bibtex-cmd=biber $<

figures/output/regions_diag.pdf: figures/R/regions_diag.R
	Rscript $<

.PHONY: FORCE