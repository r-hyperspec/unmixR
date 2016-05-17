##' Construct an image from a matrix of spectra using selected bands
##'
##' Convert a set of spectra into a form suitable for
##' making a false color image.  The spectra are first
##' subset according to the requested bands (wavelengths/frequencies).
##' Then, the spectra are
##' stacked into columns (as R is a column-major language)
##' to create the final image, which is a matrix ready
##' to be passed to \code{image}.
##'
##' @param M Matrix containing spectra in rows.
##' The matrix should have dimensions no. of samples x
##' no. of wavelengths.
##'
##' @param nrow Integer. The number of rows in the final image.
##
##' @param ncol Integer. The number of columns in the final image.
##'
##' @param band Integer.  A vector of integers giving the bands to be
##' displayed in the image.  The intensity of the specified bands are
##' summed to give a value which will be the intensity in the image.
##' Defaults to all bands.  One can also use any expression that 
##' evaluates to a vector of \code{TRUE/FALSE} for subsetting.
##'
##' @param ... Arguments to be passed downstream.
##'
##' @return A matrix suitable for display with \code{image}.
##'
##' @export

abundanceImage <- function(M = NULL, nrow = 0, ncol = 0,
	band = 1:ncol(M), ...) {
	
	if (is.null(M)) stop("A matrix of spectra must be provided")
	if (nrow == 0) stop("nrow cannot be zero")
	if (ncol == 0) stop("ncol cannot be zero")

	# Local Helper Function
	# Perhaps we need a stackByRow to accommodate any input format
	
	stackByColumn <- function(IN, nrow, ncol){
		if (length(IN)/nrow != ncol) stop("Dimensions don't make sense in stackByColumn")
		OUT <- matrix(NA, nrow = nrow, ncol = ncol)
		idx <- seq(nrow, length(IN), nrow)
		for (n in 1:ncol){
			OUT[,n] <- IN[(idx[n]-nrow+1):(idx[n])]
			}
		OUT
		}
	# test: stackByColumn(IN = 1:20, nrow = 5, ncol = 4)

	# Subset the spectra by requested band
	
	M <- M[, band]
	if (length(band) > 1) M <- rowSums(M)
	
	# Stack the results into the image matrix
	
	IM <- stackByColumn(M, nrow, ncol)
	IM
	} # end of abundanceImage


