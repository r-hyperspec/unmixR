##' Dimensionality Reduction Contingent on SNR
##' 
##' Reduces the dimensionality of a data matrix if the signal-to-noise
##' ratio (SNR) is above a certain threshold.
##'
##' The SNR threshold is calculated as 15 + 10*log10(p).  If the input
##' matrix SNR exceeds this value, the data is reduced to dimension \code{p}.
##' If the SNR is below this value, the data is first reduced to \code{p-1} and
##' then later restored to dimension \code{p}.
##'
##' @param data Data matrix. Samples in rows frequencies in columns.
##'
##' @param p Number of endmembers.
##'
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}.
##'
##' @return Data matrix with dimensionality equal to \code{p}.
##'
##' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005,
##'   doi: 10.1109/TGRS.2005.844293
##'
##' @export
##' 

dimensionalityReduction <- function(data, p, SNR = estSNR(data, p)){

    # Mostly using the symbols in Algorithm 1 in the ref

    R <- t(as.matrix(data))    
    SNRth <- 15 + 10 * log10(p)
    N <- nrow(R) # after transposition, this is the number of frequencies
    
    # Reduce depending upon SNR
    
    if (SNR > SNRth) {
        d <- p # reducing to p
        # obtain projection matrix
        Ud <- svd(tcrossprod(R) / N, nu =  d, nv = 0)$u
        X <- crossprod(Ud, R)
        u <- rowMeans(X) # after transposition, the means of each dimension
        # project data on subspace ("projective projection")
        Y <- t(t(X) / as.vector(crossprod(X, u)))

    } else {
    	
        d <- p - 1 # reducing to p - 1
        u <- rowMeans(R)
        Ud <- svd(tcrossprod(R - u) / N, nu = d, nv = 0)$u
        X <- crossprod(Ud, R - u)
        
        # The value of cc assures that co-latitude angle between u
        # and any vector from X is between 0 and 45
        cc <- max(apply(X, 2, function(x){sqrt(sum(x^2))}))
        Y <- rbind(X, cc) # restores to dimension p
    }
    
    t(Y)
}