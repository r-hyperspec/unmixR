
# This is a re-write attempt of the original estSNR in an attempt to
# clean up the code and chase out errors (original meaning start of GSOC 2016).
# Old code mostly left as comments for comparison.
# Bryan
##' @export


estSNR3 <- function(data, p) {
  data <- as.matrix(data)
  
  E <- function(M, n) sum(c(M)^2 / n) # expectation operator
  
  L <- ncol(data) # no of frequencies / bands 
  N <- nrow(data) # no of samples # NOT used in estSNR3
  
  Ud <- svd(crossprod(data), nu = p, nv = p)
  Ud <- Ud[["u"]][, sequence(p), drop = FALSE]
  
  reducedData <- data %*% Ud
  
  pr <- E(data, N) # E value of raw data
  prp <- E(reducedData, N) # E value of reduced data
  
  snr <- (prp - (p * pr / L)) / (pr - prp)
  equalityThreshold <- 1e-4
  if(pr - prp < equalityThreshold){
      snr <- Inf
  }
  SNR <- 10 * log10(snr)
  return(SNR)
}
