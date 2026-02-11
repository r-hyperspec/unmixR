#' Retrieve Endmembers from Pure-Pixel Models
#'
#' Retrieves the endmembers from a dataset using the model returned by
#' \code{\link{nfindr}}, \code{\link{vca}}, or \code{\link{atgp}}.
#'
#' @param object A pure-pixel model structure (class \code{pure_endmembers})
#'   returned by \code{\link{nfindr}}, \code{\link{vca}}, or \code{\link{atgp}}.
#'
#' @param data The data used to calculate the endmembers.  If not provided
#'   the data used to calculate the endmembers will be retrieved from the
#'   object.
#'
#' @return A matrix where each row is an endmember as calculated by the
#'   unmixing algorithm.
#'
#' @section Warning:
#' If dimension reduction was performed prior to unmixing this function
#' returns the scores of the corresponding endmembers.  In this case, to
#' get the original spectra you probably want
#' \code{endmembers(object, raw_data)} which is equivalent to
#' \code{raw_data[object$indices,]}.
#'
#' @export
#'
#' @seealso \code{\link{vca}}, \code{\link{nfindr}}, \code{\link{atgp}}, and
#'   \code{\link{abundances}} for examples.
#'

# BH: this doesn't handle drop = TRUE for nfindr series correctly.
# need to determine if we want to keep drop = TRUE

endmembers <- function(object, data = NULL) {
  if (is.null(data)) {
    if (is.null(object[["data"]])) {
      stop("No data provided and none found in object.")
    }
    data <- object[["data"]]
  }

  if (is.null(object[["indices"]])) {
    stop("`pure_endmembers` objects must contain `indices`.")
  }
  i <- object[["indices"]]

  return(data[i, , drop = FALSE])
}
