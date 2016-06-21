##' J. F. Lopez Vertex Component Analysis
##'
##' The original VCA algorithm (\code{\link{vca}}) contains auxiliary
##' operations (signal to noise estimation, etc.) which could be omitted while
##' still maintaining a fully functional algorithm. In the paper that describes
##' the Modified VCA algorithm (\code{\link{vcaMod}}) the author provides a
##' description of the original VCA with only the essential components
##' included. This is an implementation of that algorithm
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix.
##'
##' @param p Number of endmembers.
##'
##' @return Indices of the endmembers in the original dataset.
##' 
##' @references Lopez, S., Horstrand, P., Callico, G.M., Lopez, J.F. and
##' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
##' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
##' Geoscience & Remote Sensing Letters, IEEE, vol. 9 no. 3 pp. 502-506, May 2012
##' doi: 10.1109/LGRS.2011.2172771
##'
##' @export
##' @importFrom stats prcomp runif

vcaLopez <- function(data, p) {
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
