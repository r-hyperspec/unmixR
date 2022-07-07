##' @name nfindr
##' @rdname nfindr
##' @include nfindr.R
##' @export
##' @importFrom stats prcomp
nfindr.default <- function(x, p, indices=sample(nrow(data), p),
                          iter = c("points", "endmembers", "both"),
                          estimator = c("Cramer", "volume", "height", "cofactor", "LDU"),
                          iter_max = 10,
                          keep_data=TRUE,
                          debug.level = 0, ...) {

  ## Parse and validate function arguments -----
  iter <- tolower(match.arg(iter))
  estimator <- tolower(match.arg(estimator))

  # get the selected nfindr method
  nfindr_func <- get0(
    paste(".nfindr", estimator, iter, sep = "_"),
    mode = "function"
  )
  if (is.null(nfindr_func)) {
    stop("Invalid options iter and/or estimator parameters")
  }

  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }

  # for "both" type iteration increase the number of iteration
  # to approximately similar amount that "points" estimator would have
  if (iter == "both") {
    iter_max <- iter_max * p
  }

  # transform the input into a matrix
  data <- as.matrix(x)
  m <- nrow(data)
  n <- ncol(data)

  ## Reduce dimension, if required ----
  # reduce the dimension of the data using PCA
  # do nothing if the data was passed in already reduced
  if (n != p - 1) {
    data <- stats::prcomp(data)[["x"]][, sequence(p - 1), drop = FALSE]
  }

  ## Do N-FINDR -----
  result <- nfindr_func(data, indices, iter_max = iter_max, debug.level = debug.level)
  
  # Keep original data in the object
  if (keep_data) {
    result$data <- data
  }
  
  # sort the indices to normalize the order between runs
  result$indices <- sort(result$indices)
  
  class(result) <- "nfindr"
  
  return(result)
}
