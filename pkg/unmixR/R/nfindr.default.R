##' @name nfindr
##' @rdname nfindr
##' @include nfindr.R
##' @export
##' @importFrom stats prcomp

nfindr.default <- function(data, p,
                           method = "99", indices = sample(nrow(data), p), ...,
                           EMonly = FALSE) {

  ## get the selected nfindr method
  nfindrFunc <- get0 (paste0 ("nfindr", method), mode = "function")

  # check if the method passed in was found
  if (is.null (nfindrFunc)) {
    stop ('Invalid option for method parameter (', method ,') try: ', 
          paste (get.implementations ("nfindr"), collapse = ", "))
  }

  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }

  # ensure we are dealing with a matrix
  data <- as.matrix (data)

  # keep original data for possible return
  orig <- data

  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  if (ncol(data) != p - 1) {
    data <- stats::prcomp(data)[["x"]][, sequence(p-1), drop=FALSE]
  }

  # call the function to get the indices of the endmembers
  # at this point data has dimensions n samples x (p-1) abstract wavelengths
  indices <- nfindrFunc(data, p, indices, ...)
  
  # sort the indices to normalise the order between runs
  indices <- sort (indices) # redundant: these are also sorted in the individual implementations

  res <- list(data = if (!EMonly) orig else orig[indices,],
              indices = indices)
  class(res) <- "nfindr"
  return(res)
}
