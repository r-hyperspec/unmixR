##' Simplified Vertex Component Analysis
##'
##' The original VCA algorithm (\code{\link{vca05}}) contains auxiliary
##' operations (signal to noise estimation etc.) which could be omitted while
##' still maintaining a fully functional algorithm.  This function
##' has only these essential components included.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix. Samples in rows frequencies in columns.
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

    # if (.options ("debuglevel") >= 1L){ # CB early version
      # print (which.max (v))
      # print (which.min (v))
    # }

    indices[i] <- index
    E[, i] <- Y[, index]

    if (.options ("debuglevel") >= 1L){
   	  cat("Iteration", i, "\n")
      cat("\tcurrent endmembers:", sort(indices[1:i]), "\n")
      # To monitor the process, capture the volume
      # of the current simplex using the same process
      # as in nfindr.default, except the data set
      # grows with each iteration
      inds <- indices[1:i] # limit to non-zero indices
      red_data <- stats::prcomp(data)[["x"]][, sequence(length(inds)-1), drop=FALSE]
      simplex <- .simplex(red_data, length(inds), inds)
      vol <- abs(det(simplex))
      cat("\tvolume:", vol, "\n")
	}
  }

  indices <- sort(indices)

  indices
}
