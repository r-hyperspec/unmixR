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

estSNR2 <- function(data, p) {

  # data is assumed to be in the usual form: samples in rows and freq in columns
  data <- as.matrix(data)  
  N <- nrow(data) # no of samples
    
  # This approach follows Eqn 10 in the reference generally,
  # and this entry: www.scholarpedia.org/article/Signal-to-noise_ratio
  # and this SO question: stats.stackexchange.com/a/134283/26909
  
  # reduce the data using svd so we can reconstruct it from the first p PC's
  X <- svd(scale(data, scale = FALSE), nu = p, nv = p)
  
  # reconstruct the data using just p PCs, this is the signal
  
  signal <- X$u %*% diag(X$d)[1:p, 1:p] %*% t(X$v) + colMeans(data)
  
  # the noise is the difference
  
  noise <- data - signal
  
  snr <- (sum(c(signal)^2)/N) / var(c(noise))

  SNR <- 10 * log10(snr)
  
  return(SNR)
}
