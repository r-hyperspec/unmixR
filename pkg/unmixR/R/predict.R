
# Calling this .predict but then using Rdname predict and export avoids
# conflicts with stats::predict and gives the proper options when doing ?predict

.predict <- function(object, newdata, ...) {

	OK <- ((class(object) == "nfindr") | (class(object) == "vca"))
	if (!OK) stop("You must provide either an nfindr or vca object")
	
	if (missing(newdata)) newdata <- object[["data"]]
		
	if (! is.matrix (newdata)) newdata <- as.matrix (newdata) # verify if passed in

	if (ncol(object[["data"]]) != ncol(newdata)) stop("Dimensions of newdata don't match")
	
	endmembers <- endmembers (object)
  	
  	raw <- t(apply(newdata, 1, function(spectrum) {nnls(t(endmembers), spectrum)[["x"]]}))
  	
  	return(raw/rowSums(raw))
}

##' Predict Endmember Abundances
##'
##' Predicts the abundance percentages of each endmember at all sample points
##' using the Non-Negative Least Squares method.
##'
##' @param object The N-FINDR/VCA structure returned by the
##'   \code{\link{nfindr}} or \code{\link{vca}} interface (note: \code{\link{ice}}
##'   intrinsically returns the abundances so this function isn't need for that method).
##'
##' @param newdata If the data stored in the object is not the data that
##'   should be checked for abundances, then this parameter allows for passing
##'   in new data.
##
##' @param ... Allow for extra parameters to match the signature of the base
##'   predict function.  For example, you may wish to pass different data in
##'   as argument \code{newdata}.  By default, the data in \code{object}
##'   will be used.
##'
##' @return A matrix where the abundances for an endmember are returned
##'   column-wise. Each value is in the range \code{[0 \dots 1]}.
##'
##' @name predict
##' @rdname predict
##' @export
##'
##' @examples
##' data(demo_data)
##' demo <- nfindr(demo_data, p = 3)
##' pred_wM <- predict(demo) # wM = weights matrix
##'
##' # The following is from ?demo_data
##' # Get the known wM (weights matrix)
##' set.seed(123)
##'
##' n <- 10 # no. of samples
##' p <- 20 # no. of frequencies
##'
##' ## endmembers / pure spectra / endmember matrix
##' em1 <- c(0, 0, 0, 5, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0)
##' em2 <- c(0, 0, 0, 0, 0, 8, 7, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0)
##' em3 <- c(0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0)
##' eM <- matrix(c(em1, em2, em3), byrow = TRUE, ncol = 20)
##'
##' ## weights matrix 
##' wM <- matrix(runif(30), nrow = n)
##' # set certain samples (weights) to pure endmembers
##' wM[3, c(2, 3)] <- 0 # em1
##' wM[7, c(1, 3)] <- 0 # em2
##' wM[9, c(1, 2)] <- 0 # em3
##' wM <- wM/rowSums(wM) # normalize weights matrix
##'
##' # Now compare the predicted abundances to the known values
##' hist(pred_wM - wM, breaks = 50)
##'

predict.nfindr <- .predict

##' @rdname predict
##' @export

predict.vca <- .predict

