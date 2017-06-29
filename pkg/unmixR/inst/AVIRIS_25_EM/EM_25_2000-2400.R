### Exploration of AVIRIS Cuprite Endmembers
### This version restricts analysis to 2000-2400 region where hydroxides, carbonates and sulfates absorb
### See https://speclab.cr.usgs.gov/map.intro.html for mineral maps & background

library("unmixR")
library("R.utils")
library("viridis")
library("plotrix")

Cup350 <- loadObject("Cuprite350x350.Rdata") # a hyperSpec object
# select bands of interest, omitting water vapor
keep <- c(min~1345, 1430~1805, 1955~max)
Cup350 <- Cup350[,,keep]
Cup350N <- sweep(Cup350, 1, mean, "/") # normalize

Cup350N <- Cup350N[,,2000~2400] # restrict to region of interest

CupEM <- vca(Cup350N, p = 25, method = "Lopez2012") # get endmembers

mypal <- inferno(20) # palette to use
myticks <- seq(50, 300, 50) # native plot dimensions are pixels, want km

pdf("Cuprite25_2000-2400.pdf")
for (i in 1:25) {
	
	tmp <- AEM(Cup350N, CupEM, EMs = i, plotMap = TRUE, plotEM = FALSE,
		col.regions = mypal,
		scales = list(at = myticks, labels = myticks/50),
		xlab = "km", ylab = "km",
		main = paste("Endmember", i, sep = " "))
		
	tmp <- AEM(Cup350N, CupEM, EMs = i, plotMap = FALSE, plotEM = TRUE,
		lines.args = list(pch = 20, type = "p"),
		title.args = list(main = paste("Endmember", i, sep = " ")))
}
dev.off()

### Analysis: This restricted wavelength range makes it much easier to see
### what's going on.  EM 15 is a very good match to K-alunite distribution.
### EM 18 is a very good match to buddingtonite.  EM 20 is a very good
### match to chalcedony.
