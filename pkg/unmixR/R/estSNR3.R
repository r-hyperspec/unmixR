
# This is a re-write attempt of the original estSNR in an attempt to
# clean up the code and chase out errors (original meaning start of GSOC 2016).
# Old code mostly left as comments for comparison.
# Bryan


estSNR3 <- function(data, p) {
  data <- as.matrix(data)
  
  # symbols (variable names) here try to stay close to the reference' usage
  
  #E <- function(M, n) sum(c(M)^2 / n) # expectation operator
  # The expectation operator is usually described as "mean squared signal"
  # Not sure why it was being divided by no. samples, but it doesn't make much of a difference
  E <- function(M) mean(M^2) # expectation operator
  
  # L <- nrow(data) # no of frequencies 
  # N <- ncol(data) # no of samples
  L <- ncol(data) # no of frequencies / bands 
  N <- nrow(data) # no of samples # NOT used in estSNR3
  
  # rowMean <- apply(data, 1, mean) # get the mean of each row
  # # repeat the column of row means so that it matches the size of the data
  # repMean <- .repvec.col(rowMean, N)
  # zMean <- data - repMean # zero mean the data
  zMean <- scale(data, scale = FALSE) # mean center the frequencies
  
  
  # Ud <- svd(tcrossprod(zMean) / N, nv=p)
  # Ud <- Ud[["u"]][, sequence(p), drop = FALSE]
  # zProj <- crossprod(Ud, zMean) # project the zero mean data
  Ud <- svd(zMean, nu = p, nv = p)
  Ud <- Ud[["u"]][, sequence(p), drop = FALSE]
  # It's not 100% clear, but this seems to be what is meant by
  # Ud in Eqn 13 of the reference

  # # BH replacement code, which seems closer to the reference
  # # Follows Eqn 13 to the letter
  # pr <- E(crossprod(data), N) # E value of raw data
  # # Next, compute E value of PCA'd data, uncentered
  # prp <- crossprod(data, Ud) %*% crossprod(Ud, data)
  # prp <- E(prp, N) + crossprod(rowMean)
  pr <- E(crossprod(zMean)) # E value of raw centered data
  prp <- crossprod(zMean, Ud) %*% crossprod(Ud, zMean) # see sentence under Eqn 13 (used svd not PCA here)
  prp <- E(prp) # E value of denoised centered data
  # Not sure if ww want to add back the colMeans or not,
  # but Conor's code seems to put it back at the wrong stage (?)
  # and he puts back rowMeans (originally the data was transposed)
  # Also, crossprod(colMeans) is a lot larger than colMeans.
  
  snr <- (prp - (p * pr / L)) / (pr - prp)
  SNR <- 10 * log10(snr)
  return(SNR)
}
