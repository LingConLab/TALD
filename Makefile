R_OPTS = --vanilla

.DEFAULT_GOAL: all
.PHONY: clean all

all: compile clean

docs/%.html: %.Rmd
	Rscript -e 'rmarkdown::render("$^", output_dir = "docs")'
compile:
	Rscript code/compile_website.R
clean:
	rm -rf docs/data docs/html docs/tests docs/data docs/DESCRIPTION docs/LICENSE docs/Makefile docs/code docs/test_logs.txt
test:
	Rscript code/test_and_write_logs.R
