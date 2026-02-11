.FORCE:

clean: .FORCE
	rm vignettes/*.md
	rm vignettes/*.html
	rm vignettes/*.log
	rm vignettes/*.synctex.gz
	rm vignettes/*.tex
	rm vignettes/*.pdf
	rm -rf vignettes/figure
	rm vignettes/knitr.sty
