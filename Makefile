.DEFAULT_GOAL := compile

compile:
	Rscript compile_website.R
clean:
	rm -r docs/data/*; rm -r docs/html/*; rm -r docs/tests/*; rm -r docs/data/*;
