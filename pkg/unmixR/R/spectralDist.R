##' Compute distances between spectra
##'
##' Compute the distances between spectra in
##' a set, providing a measure of their similarity.
##' Common distance measures are available.
##' Optionally, a particular spectrum may be specified as
##' a reference spectrum.  In all cases it is assumed
##' that the spectra are in similar units and appropriately
##' scaled.
##'
##' @param M Matrix containing spectra to be compared in rows.
##' The matrix should have dimensions no. of samples x
##' no. of wavelengths. 
##' BH: SHOULD THE MATRIX BE MEAN-CENTERED?
##  CB: NO. Why should it? 
##'
##' @param method String.  The method to be used in computing the distance.
##' One of \code{c("name them")}.
##'
##' @param ref Defaults to \code{NULL}.  Otherwise, an integer
##' giving the row of M to be used as the reference spectrum.
##'
##' @param reportAsRank If \code{ref} is given, this flag will
##' cause the vector to be reported as a rank.
##'
##' @param ... Arguments to be passed downstream, for instance, the power
##' for the Minkowski computation.  See \code{\link{dist}} for details.
##'
##' @return If \code{ref = NULL}, an object of class \code{dist}; basically
##' the lower triangle of the pair-wise distance matrix.  If \code{ref != NULL},
##' a vector of length \code{nrow(M)} containing the
##' distances between each spectrum and the reference spectrum.  If
##' \code{reportAsRank = TRUE} the vector will be reported as a ranked vector.
##'
##' @export
##' @importFrom ChemoSpec Dist

spectralDist <- function(M = NULL, method = "euclidean",
	ref = NULL, reportAsRank = FALSE, ...) {
	
	# Bryan A. Hanson, DePauw University, June 2014
	# Patterned after rowDist in ChemoSpec
	# A quick, error-free & lazy approach is used here, 
	# namely, compute the entire distance matrix
	# but then select only the desired entries.
	# This will have a small speed penalty.
	
	# Will require package amap for the Dist function.
	
	if (is.null(M)) stop("Missing matrix of spectra")
	
	#M <- scale(M, center = TRUE, scale = FALSE) # not sure if this necessary
	
	method <- match.arg(method, c("cosineAlpha", "pearson", "correlation", "spearman", "kendall",
		"euclidean", "maximum", "manhattan", "canberra","binary", "minkowski"))

	if (method %in% c("pearson", "correlation", "spearman", "kendall") ) {
		D <- Dist(M, method = method)
		}
	
  ## methods implemented in stats::dist
	if (method %in% c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski") ) {
		D <- dist(M, method = method, ...)
		}

	if (method == "cosineAlpha") {
		# Not sure I have correctly implemented this!
		
		# definition from Varmuza & Filzmoser pg 46
		# does not seem to give sensible answers
		#D = crossprod(M)/(sqrt(colSums(M)^2) * sqrt(rowSums(M)^2))	
		D = crossprod(M)/sqrt(sum(M)) # this is not right either!
		}

	if (is.null(ref)) return(D)
	
	# Continue if ref != NULL
	# Convert D to a symmetric matrix, class dist is hard to use,
	# and grab the desired column on the fly
	
	if (ref > nrow(M)) stop("Requested reference spectrum doesn't exist")
	D <- as.numeric(as.matrix(D)[,ref])
	if (reportAsRank) D <- rank(D)
	D
	}
	
.test(spectralDist) <- function() {

  td <- matrix(1, ncol = 5, nrow = 5)
  #td <- matrix(rnorm(25), ncol = 5, nrow = 5)

  # test: spectralDist produces error for invalid method
  checkException(spectralDist(td, method = "invalid"))
  
  # test: spectralDist returns a vector when ref is given
  checkTrue(is.vector(spectralDist(td, ref = 1)))
 
   # test: spectralDist returns a dist object when ref is NULL
  checkTrue(class(spectralDist(td)) == "dist")
 
  # test correct calculations for the available methods
  # probably don't need this for anything but cosineAlpha
  
  meth <- c("pearson", "correlation", "spearman", "kendall", "cosineAlpha",
    "euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")
  
  for (m in meth) {
  	}
  
  }
