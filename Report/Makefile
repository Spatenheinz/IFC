MAINFILE = main.tex
CC = latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f
ALL = $(wildcard *.tex */*.tex */*.sty *.bib)

.PHONY: all
all: $(MAINFILE).pdf

$(MAINFILE).pdf: $(ALL)
	$(CC) $(MAINFILE)
	latexmk -CA

.PHONY: clean
clean:
	latexmk -CA
	rm main.bbl
	rm main.run.xml
	rm main.pdf
