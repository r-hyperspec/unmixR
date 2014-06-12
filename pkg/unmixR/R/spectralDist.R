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
##' @return pair-wise distance matrix of all cases in \code{M} against those indicated by \code{ref}.  
##' 
##' If \code{ref} is not given, a dist object with all pair-wise distances.
##' 
##' If \code{reportAsRank = TRUE} the vector will be reported as a ranked vector.
##' 
##' @author Bryan Hanson, with contributions by Claudia Beleites
##' @seealso  \code{\link[amap]{Dist}}, \code{\link[stats]{dist}},  \code{\link{cos.dist}}
##' @export
##' @include unmixR-package.R
##' @importFrom amap Dist

spectralDist <- function (M = stop ("Missing matrix of spectra"), 
                          method = c ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski",  
                                      "pearson", "correlation", "spearman", "kendall", "abspearson", "abscorrelation",
                                      "cosineAlpha"),
                          ref, reportAsRank = FALSE, ...) {
	
	# Bryan A. Hanson, DePauw University, June 2014
	# Patterned after rowDist in ChemoSpec
	# A quick, error-free & lazy approach is used here, 
	# namely, compute the entire distance matrix
	# but then select only the desired entries.
	# This will have a small speed penalty.
	
	method <- match.arg (method)

	D <- switch (method,
	             minkowski = dist (M, "minkowski", ...),
               cosineAlpha = cos.dist (M, ...),
               Dist (M, method = method, ...)
	)
	
  if (! missing (ref))
	  D <- as.matrix (D) [, ref, drop = FALSE]

  ## TODO: @BH should we really include this. This is just one line, and I'm not sure when global vs. case-wise ranking is required. 
  if (reportAsRank) 
    D [TRUE] <- rank (D)
  
  D
}
	
.test(spectralDist) <- function() {

  td <- matrix(1 : 25, ncol = 5, nrow = 5)

  ## test: spectralDist produces error for invalid method
  checkException(spectralDist(td, method = "invalid"))
  
  ## test: spectralDist returns a matrix when ref is given
  checkTrue (is.matrix (spectralDist (td, ref = 1)))
  
  checkEquals (dim (spectralDist (td, ref = 1 : 3)), c (5, 3))
  checkTrue (is.matrix (spectralDist (td, ref = 1 : 3)))
  
  ## test: spectralDist returns a dist object when ref is missing
  checkTrue(class(spectralDist(td)) == "dist")

}


