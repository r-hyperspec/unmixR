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

if (!require('svUnit', quietly = TRUE)) {
	'.test<-' <- function(f, value) {
		class(value) <- c('svTest', 'function')
		attr(f, 'test') <- value
		f
	}
} else {
	'.test<-' <- svUnit::'test<-'
}
