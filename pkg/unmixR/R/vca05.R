##' Vertex Component Analysis Unmixing Algorithm
##'
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix.
##'
##' @param p Number of endmembers.
##'
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}.
##'
##' @return The indices of the endmembers in the original dataset.
##'
##' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005,
##'   doi: 10.1109/TGRS.2005.844293
##'
##' @export
##' @importFrom stats runif

vca05 <- function(data, p, SNR = estSNR(data, p)) {

  data <- t(as.matrix(data))
  N <- ncol(data)

  SNRth <- 15 + 10 * log10(p)

  # if the estimated SNR is over a certain threshold ...
  if (SNR > SNRth) {
    d <- p
    Ud <- svd(tcrossprod(data) / N)[["u"]][, sequence(d), drop = FALSE]

    x <- crossprod(Ud, data)
    u <- apply(x, 1, mean)
    dataProj <- Ud %*% x[1:d,] # project the input matrix

    repu <- .repvec.col(u, N)
    y <- x / .repvec.row(apply(t(x * repu), 1, sum), d)
  } else {
    d <- p - 1

    rowMean <- apply(data, 1, mean) # get the mean of each row
    # repeat the column of row means so that it matches the size of the data
    repMean <- .repvec.col(rowMean, N)
    zMean <- data - repMean # zero mean the data
#    Ud <- svd(tcrossprod(zMean) / N, nv=p)$u[,1:p] # Conor original
    Ud <- svd(tcrossprod(zMean) / N, nu = p)[["u"]][, sequence(d), drop = FALSE]
    
    zProj <- crossprod(Ud, zMean) # project the zero mean data

    x <- zProj#[1:d, ]
    dataProj <- Ud[, 1:d] %*% x + repMean
    c <- max(sum(x^2))^0.5
    y <- rbind(x, c)
  }

  indices <- array(0, p)
  A <- matrix(0, nrow=p, ncol=p)
  A[p,1] <- 1

  for (i in 1:p) {
    w <- stats::runif(p)
    f <- w - A %*% ginv(A) %*% w
    f <- f / sqrt(sum(f^2))

    v <- abs(crossprod(f, y))
    indices[i] <- which.max(v) # get index of max value

    # if (.options ("debuglevel") >= 1L){ # CB early version
      # print (which.max (v))
      # print (which.min (v))
    # }

    A[,i] <- y[, indices[i]]

    if (.options ("debuglevel") >= 1L){
	  cat("Iteration", i, "\n")
      cat("\tcurrent endmembers:", sort(indices[1:i]), "\n")
      # To monitor the process, capture the volume
      # of the current simplex using the same process
      # as in nfindr.default, except the data set
      # grows with each iteration
      inds <- indices[1:i] # limit to non-zero indices
      # note for vca05 only need to transpose here as data was transposed above
      red_data <- stats::prcomp(t(data))[["x"]][, sequence(length(inds)-1), drop=FALSE]
      simplex <- .simplex(red_data, length(inds), inds)
      vol <- abs(det(simplex))
      cat("\tvolume:", vol, "\n")
	}

  }

   indices <- sort (indices)
   indices

}
