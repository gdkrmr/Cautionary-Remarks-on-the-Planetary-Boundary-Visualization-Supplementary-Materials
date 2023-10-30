.PHONY: all clean

all: Supplement.html

Supplement.html: Supplement.Rmd
	Rscript -e "rmarkdown::render('$<')"

clean:
	rm -f Supplement.html
	rm -rf Supplement_files
