MAINFILE = main.tex
CC = latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -f
ALL = $(wildcard *.tex */*.tex */*.sty *.bib)

.PHONY: all
all: $(MAINFILE).pdf

$(MAINFILE).pdf: $(ALL)
	$(CC) $(MAINFILE)

.PHONY: clean
clean:
	latexmk -c
	rm main.bbl
	rm main.run.xml
