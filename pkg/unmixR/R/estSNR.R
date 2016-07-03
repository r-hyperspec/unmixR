##' Estimate Signal to Noise Ratio
##'
##' Estimate the signal to noise ratio of a hyperspectral image.
##'
##' @param data The hyperspectral image whose signal to noise ratio needs to
##'   be estimated.  Samples in rows, frequencies in columns.
##'
##' @section Warning:
##'   Be careful,
##'   when this function is called by \code{\link{vca05}} the data is already
##'   transposed due to
##'   lazy evaluation.  If you want to get the same answer by calling this function
##'   directly, you'll need to transpose the data first!
##'
##' @param p The number of endmembers.
##'
##' @return The estimated signal to noise ratio in decibels.
##' 
##' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005, 
##'   doi: 10.1109/TGRS.2005.844293
##'
##' @export

estSNR <- function(data, p) {
    data <- as.matrix(data)
  E <- function(M, n) sum(c(M)^2 / n) # expectation operator

  # NOTE: we don't need to transpose here, because when this
  # is called internally by vca05, the data is already tranposed
  # and lazy evaluation applies (the SNR argument to vca05 is not
  # eval'd until it is needed, after the transpose)
  # This also means one should not call estSNR directly, unless
  # one transposes first.
  
  #data <- t(data) # BH added by analogy to the VCA functions (?)
  # If we don't transpose, the definitions below are reversed
  # compared to how we have used them other places.
  #p <- p - 1 # BH added by analogy to the VCA functions (?)

  ##CB TODO: double check all this
  
  L <- nrow(data) # no of frequencies 
  N <- ncol(data) # no of samples
  
  rowMean <- apply(data, 1, mean) # get the mean of each row
  # repeat the column of row means so that it matches the size of the data
  repMean <- .repvec.col(rowMean, N)
  zMean <- data - repMean # zero mean the data
  Ud <- svd(tcrossprod(zMean) / N, nv=p)
  Ud <- Ud[["u"]][, sequence(p), drop = FALSE]
  zProj <- crossprod(Ud, zMean) # project the zero mean data

  # Conor's original code + BH comments
  # pr <- E(data, N) # E value of raw data
  # # Next, compute E value of PCA'd data, uncentered
  # prp <- E(crossprod(Ud, zMean), N) + crossprod(rowMean)
  # # Last step based upon Eqn 13 in reference
  # SNR <- 10 * log10((prp - (p / L) * pr) / (pr - prp))
 
  # BH replacement code, which seems closer to the reference
  # Follows Eqn 13 to the letter
  pr <- E(crossprod(data), N) # E value of raw data
  # Next, compute E value of PCA'd data, uncentered
  prp <- crossprod(data, Ud) %*% crossprod(Ud, data)
  prp <- E(prp, N) + crossprod(rowMean)
  
  # .testdata used in unit tests has no or very little noise.
  # This creates some odd situations, which are trapped below.
  # Something is probably wrong before this spot.
  
  # Some reporting for troubleshooting
  
  SNRth <- 15 + 10 * log10(p)

  if ((prp - (p / L) * pr) / (pr - prp) < 0) {
  	# This would be taking the log10 of a negative number
  	message("SNR is undefined for this data, returning alternate definition")
  	return(10*log10(prp/pr)) # this is plain denoised data over data
  	}
  	
  # Otherwise compute according to Eqn 13
  SNR <- 10 * log10((prp - (p / L) * pr) / (pr - prp))
  
  # Note: .Machine[[1]] is .Machine$double.eps
  
  if (abs (SNR) < sqrt (.Machine[[1]])) # perfect reconstruction
    SNR <- .Machine[[1]] # make sure the log returns useful number

  SNR
}
