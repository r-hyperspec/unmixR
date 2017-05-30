##' LDU-Sequential N-FINDR: Slightly Modified LDU N-FINDR
##' 
##' While LDU N-FINDR examines a single pixel in each endmember position and
##' repeats over all pixels, this algorithm considers all pixels in a single
##' endmember position and then repeats over all endmember positions.
##' Intended to be called from \code{\link{nfindr}} (see there for examples).
##' 
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param indices Indices used in the simplex estimation.
##'
##' @param ... Extra unused parameters passed in from
##'   \code{\link{nfindr}}.
##'
##' @return The sorted indices of the endmembers in the original dataset.
##'   
##' @references  Dowler, Shaun W., Takashima, Reymond, and Andrews, Mark
##'   "Reducing the Complexity of the N-FINDR Algorithm for Hyperspectral
##'   Image Analysis.", IEEE Trans Image Process. 2013 22(7):2835-2848
##'   doi: 10.1109/TIP.2012.2219546
##'
##' @export
##'

nfindrSeqLDU <- function(data, p, indices, ...) {
  simplex <- .simplex(data, p, indices)
  nspectra <- nrow(data)
  pm1 <- 1:(p-1) # create a range from 1 to p minus 1
  g <- matrix(0, nrow=p, ncol=p-1)
  V <- pm1

  replace <- TRUE
  Vtest <- 0 # initialize for debug reporting
  iter <- 1 # initialize for debug reporting

  while (replace == TRUE) {

	if (.options ("debuglevel") >= 1L) {
	  cat("Iteration", iter, "\n")
	  cat("\tcurrent endmembers:", sort(indices), "\n")
	  cat("\tvolume:", Vtest, "\n")
	  }

    replace <- FALSE

    for (i in 1:p) {
      dup <- simplex # make a copy of the simplex
      # swap the i-th and p-th columns of the simplex
      dup [, c(p, i)] <- dup [, c(i, p)]

      # get the partitioned components of the simplex matrix
      A <- dup[pm1,pm1]
      b <- dup[pm1,p]
      c <- dup[p,pm1]
      d <- dup[p,p]

      g[i,] <- crossprod(c, solve(A))
      V[i] <- as.numeric(abs(d - crossprod(g[i,], b)))

      for (j in 1:nspectra) {
        y <- c(1, data[j,])
        bj <- y[1:(p-1)]
        dj <- y[p]

        Vtest <- as.numeric(abs(dj - crossprod(g[i,], bj)))

        if (Vtest > V[i]) {
          replace <- TRUE
          simplex[,i] <- y
          indices[i] <- j
        }
      }
    }

    iter <- iter + 1
    
  }
  
  indices <- sort(indices)

  indices
}
