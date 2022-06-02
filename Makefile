R_OPTS=--vanilla

.DEFAULT_GOAL: all

all: compile clean

compile:
	Rscript compile_website.R
clean:
	rm -rf docs/data docs/html docs/tests docs/data docs/DESCRIPTION docs/LICENSE docs/Makefile
	