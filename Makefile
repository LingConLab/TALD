.DEFAULT_GOAL := compile_quietly

compile_quietly:
	R CMD BATCH compile_website.R
compile:
	Rscript compile_website.R
clean:
    rm -r docs/data/*; rm -r docs/html/*; rm -r docs/tests/*; rm -r docs/data/*;
