R_OPTS = --vanilla

.DEFAULT_GOAL: all
.PHONY: clean all

all: compile clean

docs/%.html: %.Rmd
	Rscript -e 'rmarkdown::render("$^", output_dir = "docs")'
compile:
	Rscript compile_website.R
clean:
	rm -rf docs/data docs/html docs/tests docs/data docs/DESCRIPTION docs/LICENSE docs/Makefile
test:
	R -q -e 'file.remove("test_logs.txt"); testthat::test_dir("tests"); test_logs <- read_lines("test_logs.txt"); write_lines(test_logs[test_logs != "everything is ok"], "test_logs.txt")'	
