##' Retrieve N-FINDR/VCA Endmembers
##'
##' Retrieves the endmembers from a dataset using the model returned by
##' \code{\link{nfindr}} or \code{\link{vca}}.
##'
##' @param object The N-FINDR/VCA structure returned by the general
##'   \code{\link{nfindr}} or \code{\link{vca}} interface.
##'
##' @return A matrix where each row is an endmember as calculated by the
##'   unmixing algorithm.
##'
##' @section Warning:
##' If dimension reduction was performed prior to unmixing this function
##' returns the scores of the corresponding endmembers.  In this case, to
##' get the original spectra you probably want
##' \code{raw_data[unmixR_results$indices,]}.
##'
##' @export
##'
##' @seealso \code{\link{vca}} and \code{\link{nfindr}} for examples.
##'

# BH: this doesn't handle drop = TRUE for nfindr series correctly.
# need to determine if we want to keep drop = TRUE

endmembers <- function(object) {
	d <- object[["data"]]
	i <- object[["indices"]]
	return(d[i,])
}
