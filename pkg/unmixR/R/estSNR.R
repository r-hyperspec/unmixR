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
