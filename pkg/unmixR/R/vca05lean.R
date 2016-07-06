##' Simplified Vertex Component Analysis
##'
##' The original VCA algorithm (\code{\link{vca05}}) contains auxiliary
##' operations (signal to noise estimation etc.) which could be omitted while
##' still maintaining a fully functional algorithm.  This function
##' has only these essential components included.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix.
##'
##' @param p Number of endmembers.
##'
##' @return Indices of the endmembers in the original dataset.
##' 
##' @export
##' @importFrom stats prcomp runif

vca05lean <- function(data, p) {
  data <- as.matrix(data)
  Y <- t(stats::prcomp(data)[["x"]][,1:p])

  E <- matrix(0, nrow=p, ncol=p)
  E[p,1] <- 1
  I <- diag(p)

  indices <- array(0, p)

  for (i in 1:p) {
    w <- runif(p)
    x <- (I - (E %*% ginv(E))) %*% w
    f <- x / sqrt(sum(x^2))
    v <- crossprod(f, Y)

    index <- which.max(abs(v))

    if (.options ("debuglevel") >= 1L){
      print (which.max (v))
      print (which.min (v))
    }

    indices[i] <- index
    E[, i] <- Y[, index]
  }

  indices <- sort(indices)

  indices
}
