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
##' @export
##'
##' @seealso \code{\link{vca}} and \code{\link{nfindr}} for examples.
##'

endmembers <- function(object) {
	d <- object[["data"]]
	i <- object[["indices"]]
	return(d[i,])
}
