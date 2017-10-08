#'
#' Match an ICE-Estimated Endmember with a Real Pixel
#'
#' ICE returns estimates of the endmembers; the results are expected to
#' be similar to the real spectra but not identical to any of them.
#' This function uses the result of unmixing with the ICE method, and returns the
#' indices of the real spectra (pixels) that match most closely.  Matching is
#' done by calculating the distance between each real pixel and each estimated
#' endmember, using a distance function chosen by the user.
#' 
#' This function is a wrapper to compute the distance between rows of a matrix
#' using a number of methods.  Methods \code{c("euclidean", "maximum", "manhattan",
#' "canberra","binary", "minkowski")} are sent to function \code{\link{dist}} in package
#' \code{\link{stats}} while methods \code{c("pearson", "correlation",
#' "spearman", "kendall")} are handled by \code{Dist} in package \code{amap}.
#' See the respective help pages for details. \code{"cosine"} is handled
#' locally.
#' 
#' @param ice An object of class \code{ice}.
#'
#' @param data The original unmixed data set (will be coerced to a matrix).
#'
#' @param method A character; one of \code{c("pearson", "correlation",
#' "spearman", "kendall", "euclidean", "maximum", "manhattan",
#' "canberra","binary", "minkowski", "cosine")}.
#'
#' @return An integer vector of the indices of the best matches
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export matchEM
#'
#' @importFrom stats dist as.dist
#' @importFrom amap Dist
#'
#' @examples
#' tst <- ice(laser, 3)
#' best <- matchEM(tst, laser, "euclidean")
#' raw <- as.matrix(laser)
#' plot(tst$endmembers[1,], type = "l")
#' lines(raw[best[1],], col = "red")
#'
matchEM <- function(ice, data, method) {

# Part of this is based on ChemoSpec::rowDist
# This version will be slow for large data sets since we are using built-in
# functions which compute all pair-wise distances even though we don't need
# all of them.

	method <- match.arg(method, c("pearson", "correlation", "spearman", "kendall",
		"euclidean", "maximum", "manhattan", "canberra","binary", "minkowski",
		"cosine"))
	
	if (class(ice) != "ice") stop("You must provide the results of unmixing by ICE")
	data <- as.matrix(data) # coerce hyperSpec objects
	EMs <- ice[["endmembers"]]
	indices <- rep(NA_integer_, nrow(EMs))
	
	
	# Helper Function
	# Returns a vector of distances between a single endmember and the
	# entire data set
	emDist <- function(EM, data) {
		
		data <- rbind(EM, data)
		
		if (method %in% c("pearson", "correlation", "spearman", "kendall")) {
			if (!requireNamespace("amap", quietly = TRUE)) {
				stop("You need to install package amap to use this function/option")
				}
			distance <- amap::Dist(data, method = method)
			}
			
		if (method %in% c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")) {
			distance <- stats::dist(data, method = method)
			}
			
		if ( method == "cosine") { # code by Claudia Beleites/unmixR w/small modifications
			data <- tcrossprod(data)
			l <- rowSums(data^2)
			l <- sqrt(outer(l, l))
			distance <- as.dist(data/l)
			}
			
		# We have a distance matrix, get just the part we wont
		distance <- as.matrix(distance)
		distance <- c(distance[2:nrow(distance), 1])
		return(distance)
	} # end of emDist
	
	
	# Now the real work in one line!
	for (i in 1:length(indices)) indices[i] <- which.min(emDist(EMs[i,], data))
	return(sort(indices))
	}

.test(matchEM) <- function() {
  context ("matchEM")
  
  expect_true(require (hyperSpec))
  
  test_that ("Identical endmembers are matched", {
    tst <- ice(laser, 3)
    tst$endmembers <- as.matrix(laser)[c(19, 52, 74),]
    expect_equal (matchEM(tst, laser, "euclidean"), c(19, 52, 74))  
  })

}
