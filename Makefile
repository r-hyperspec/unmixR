.FORCE:

clean: .FORCE
	rm pkg/unmixR/vignettes/*.md
	rm pkg/unmixR/vignettes/*.html
	rm pkg/unmixR/vignettes/*.log
	rm pkg/unmixR/vignettes/*.synctex.gz
	rm pkg/unmixR/vignettes/*.tex
	rm pkg/unmixR/vignettes/*.pdf
	rm -rf pkg/unmixR/vignettes/figure
	rm pkg/unmixR/vignettes/knitr.sty
