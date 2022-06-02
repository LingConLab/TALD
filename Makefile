.PHONY: all clean

R_OPTS=--vanilla

all:
	Rscript compile_website.R
clean:
	rm -rf docs/data docs/html docs/tests docs/data docs/DESCRIPTION docs/LICENSE docs/Makefile
