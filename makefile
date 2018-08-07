RFILE = README

all: knith open 

knith: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',output_file='$(RFILE).html')" | R --no-save -q

knitr: $(RFILE).Rmd
	echo "rmarkdown::render('$(RFILE).Rmd',rmarkdown::md_document(variant='markdown_github'))" | R --no-save -q

open: $(RFILE).html
	xdg-open $(RFILE).html &

report:
	echo "rmarkdown::render('report.Rmd',output_file='report.html')" | R --no-save -q

clean:
	rm -rf *.html
