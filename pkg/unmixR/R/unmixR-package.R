##' unmixR implements the N-FINDR and Vertex Component Analysis (VCA)
##' algorithms which can recover pure component spectra and their
##' respective concentrations from a hyperspectral data set.
##'
##' @name unmixR-package
##' @title Hyperspectral Unmixing Methods
##' @docType package
##'
##' @rdname unmixR-package
##' @aliases unmixR
##' @keywords package
##' @import hyperSpec
##' @import nnls
##' @import MASS
# @import svUnit

# This code needs to be here due to the order in which
# things are sourced, apparently.  It made more sense to
# me to put it unittests.R but that causes problems.  BH.
	
	if (!require('svUnit', quietly = TRUE)) {
		'.test<-' <- function(f, value) {
			cat("svUnit is not available\n")
			class(value) <- c('svTest', 'function')
			attr(f, 'test') <- value
			f
		}
	} else {
		'.test<-' <- svUnit::'test<-'
		cat("svUnit is installed\n")
		print(svUnit::'test<-') # this is valid code/real function
	}

