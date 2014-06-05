##' Simplex Generator
##' 
##' Simple helper function for generating initial simplex for N-FINDR
##' methods using indices that point to rows in a data matrix
##' 
##' @param data Matrix whose rows will be included in the simplex. This
##'   matrix should be reduced using using PCA or some other process
##'   so that it has p columns before calling this function.
##' @param p Number of endmembers that should be found using the simplex.
##'   Defines the size of the simplex (will be p x p)
##' @param indices Locations of the rows in the dataset to use in the simplex
##' @return The simplex, a p x p matrix whose first row contains only 1s
##' 
##' @rdname simplex
##' @include unmixR-package.R

# .simplex <- function(data, p, indices) {
  # rbind(rep(1, p), t(data[indices,]))
# }

# The following is Bryan's modification so that the function does what
# it is claimed to do, return a p x p matrix
# This was not really necessary, but does not harm; I was forgetting that
# nfinder.default was doing the PCA before coming here.

.simplex <- function(data, p, indices) {
  if (length(indices) != p) {
  	stop("indices should have length p")
  }
  data <- data[indices,]
  tdata <- t(data)
  tdata <- tdata[1:(p-1),]
  rbind(rep(1, p), tdata)
}

.test(.simplex) <- function() {
  # test: verify the output of .simplex
  
  p <- 3
  rows <- 5
  indices <- c(1, 3, 5)
  
  data <- matrix(1:((p-1)*rows), ncol=p-1, nrow=rows, byrow=TRUE)
  expected <- matrix(c(
    c(1,1,1),
    c(1,5,9),
    c(2,6,10)
  ), ncol=p, byrow=TRUE)
  
  checkEquals(expected, .simplex(data, p, indices))
}