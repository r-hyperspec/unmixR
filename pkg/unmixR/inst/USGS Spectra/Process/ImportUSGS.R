
# Script to import USGS reference spectra
# Data recorded for physical specimens using Beckman 5270 spectrometer

# Bryan Hanson, June 2017

# Get wavelengths (480 values, all files have the same frequency axis)
lambda <- read.table(file = "AluniteWavelengthMicrons.txt", skip = 1)

# Get intensities. Note -1.23e+34 is their "mark" for a deleted channel
Alun <- read.table(file = "AluniteIntensities.txt", skip = 1)
Budd <- read.table(file = "BuddingtoniteIntensities.txt", skip = 1)

DF <- data.frame(lambda*1000, Alun, Budd) # convert to nm
names(DF) <- c("Wavelength", "Alunite", "Buddingtonite")

# Fix deleted channels

DF$Alunite <- ifelse(DF$Alunite == -1.23e34, NA, DF$Alunite)
DF$Buddingtonite <- ifelse(DF$Buddingtonite == -1.23e34, NA, DF$Buddingtonite)

# Inspect
plot(DF$Wavelength, DF$Alunite, type = "l")
lines(DF$Wavelength, DF$Buddingtonite, col = "red") # so far so good

# Convert to a hyperSpec object
library("hyperSpec")

USGS <- new("hyperSpec", spc = t(as.matrix(DF[,2:3])), wavelength = DF$Wavelength)
labels(USGS, ".wavelength") <- "nanometers"
labels(USGS, "spc") <- "reflectance"
USGS$cmpd <- c("alunite", "buddingtonite")
plotspc(USGS, stacked = TRUE) # looks good

# Save
library("R.utils")
saveObject(USGS, file = "USGS.Rdata")

