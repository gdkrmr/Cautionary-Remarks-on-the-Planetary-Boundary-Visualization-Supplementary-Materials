.PHONY: all clean

all: Supplement.html Supplement.pdf fig/mergefig4.png fig/mergefig4.pdf

Supplement.html: Supplement.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'html_document')"

Supplement.pdf: Supplement.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

# for some reason building the pdf with knitr fails but running pdflatex on the tex file works :shrug:
fig/mergefig4.pdf: fig/mergefig4.tex # Supplement.pdf
	pdflatex $<

fig/mergefig4.png: fig/mergefig4.pdf
	magick -density 300 $< $@

clean:
	rm -f Supplement.html
	rm -rf Supplement_files
