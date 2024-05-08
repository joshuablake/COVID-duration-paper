main.pdf: main.tex references.bib .PHONY
	./latexrun --bibtex-cmd=biber $<

.PHONY: FORCE