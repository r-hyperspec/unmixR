### Exploration of AVIRIS Cuprite Endmembers
### This version uses all wavelengths
### See https://speclab.cr.usgs.gov/map.intro.html for mineral maps & background

library("unmixR")
library("R.utils")
ibrary("viridis")
library("plotrix")

Cup350 <- loadObject("Cuprite350x350.Rdata") # a hyperSpec object
# select bands of interest, omitting water vapor
keep <- c(min~1345, 1430~1805, 1955~max)
Cup350 <- Cup350[,,keep]
Cup350N <- sweep(Cup350, 1, mean, "/") # normalize

CupEM <- vca(Cup350N, p = 25, method = "Lopez2012") # get endmembers

mypal <- inferno(20) # palette to use
myticks <- seq(50, 300, 50) # native plot dimensions are pixels, want km

pdf("Cuprite25_all.pdf")
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

### Analysis: These maps are a bit harder to make sense of than the ones
### in which the wavelength range is restricted.

