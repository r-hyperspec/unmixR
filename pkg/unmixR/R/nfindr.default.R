##' @name nfindr
##' @rdname nfindr
##' @export

nfindr.default <- function(data, p,
                           method="LDU", indices=sample(nrow(data), p), ...,
                           drop=FALSE) {
  x <- NULL # suppresses check warnings about no visible global binding

  methods <- c("99", "LDU", "SeqLDU", "CB", "Brute") # valid methods
  
  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer greater than 2")
  }
  
  # check if the method passed in is valid
  if (!method %in% methods) {
    methodsStr <- paste(methods, collapse=", ")
    stop(paste("Invalid option for method parameter, try:", methodsStr))
  }
  
  # transform the input into a matrix
  data <- as.matrix(data)
  
  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  orig <- data
  if (ncol(data) != p - 1) {
    data <- prcomp(data)$x[, 1:(p-1), drop=FALSE]
  }
  
  # get the selected nfindr method
  nfindrFunc <- get(paste("nfindr", method, sep=""))
  # call the function to get the indices of the endmembers
  indices <- nfindrFunc(data, p, indices, ...)
  # sort the indices to normalise the order between runs
  indices <- sort(indices) # they should be returned already sorted by each method
  
  # return a model
  structure(list(
    data = if (!drop) orig else orig[indices,],
    indices = if (!drop) indices else 1:p
  ), class = "nfindr")
}
