##' Retrieve N-FINDR/VCA Endmembers
##'
##' Retrieves the endmembers from a dataset using the model returned by
##' \code{\link{nfindr}} or \code{\link{vca}}.
##'
##' @param object The N-FINDR/VCA structure returned by the general
##'   \code{\link{nfindr}} or \code{\link{vca}} interface.
##'
## @param newdata If the data stored in the object does not actually contain
##   the endmembers then this parameter allows for passing in new data
##'
##' @return A matrix where each row is an endmember as calculated by the
##'   unmixing algorithm
##'
##' @export

# original
# endmembers <- function(object, newdata=object$data) {
  # indices <- NULL # suppresses check warnings about no visible global binding
  # newdata[object$indices, ]
# }

# revised (newdata can be removed from documentation)
endmembers <- function(object) {
	d <- object[["data"]]
	i <- object[["indices"]]
	return(d[i,])
}
