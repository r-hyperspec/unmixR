
# Import and inspect the Cuprite data set using hyperSpec

library("hyperSpec")
library("R.utils")
library("viridis")

wl <- 667 # wavelength to plot
mypal <- inferno(20) # palette to use

# Read in the scene
cuprite <- read.ENVI(file = "f970619t01p02_r02_sc04.a.rfl",
	headerfile = "f970619t01p02_r02_sc04.a.hdr")
# OK to ignore warnings about NA

spec <- read.table(file = "f970619t01p02_r02.a.spc")
colnames (spec) <- c ("wl", "refl", "V3", "V4", "i")
spec <- spec [order (spec$i),]
wl (cuprite) <- spec$wl
labels (cuprite, ".wavelength") <- expression (lambda / nm)
cuprite <- cuprite[]/10000 # back to the original scale (see NASA documentation)

saveObject(cuprite, file = "Cuprite_sc04.Rdata")

# Inspect
cuprite <- loadObject("Cuprite_sc04.Rdata")

plotmap (cuprite [,, wl], col.regions = mypal)

# Trim to a 350 x 350 image of the good stuff

selX <- cuprite$x > 263
selY <- cuprite$y < 350
Cup350 <- cuprite[(selX & selY), , ]

# Restore geographic north to up & reset pixel reference to 0,0 at lower left

Cup350$y <- 349 - Cup350$y
Cup350$x <- Cup350$x - min(Cup350$x)

saveObject(Cup350, file = "Cuprite350x350.Rdata")

plotmap (Cup350 [,, wl], col.regions = mypal)

# Plot with km as tick marks
# 20 m resolution so 50 pixels is 1,000 m or 1 km
myticks <- seq(50, 300, 50)
plotmap (Cup350 [,, wl], col.regions = mypal,
	scales = list(at = myticks, labels = myticks/50),
	xlab = "km", ylab = "km")

# Things look better if normalized!
Cup350N <- sweep(Cup350, 1, rowMeans(Cup350[[]]), "/")

#pdf("Cuprite350.pdf")
plotmap (Cup350N [,, wl], col.regions = mypal,
	scales = list(at = myticks, labels = myticks/50),
	xlab = "km", ylab = "km",
	main = bquote("Cuprite, Nevada" ~ lambda == .(wl) ~ "nm"))
#dev.off()
