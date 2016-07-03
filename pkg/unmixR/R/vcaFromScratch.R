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
##' @importFrom stats prcomp rnorm
 

vcaFromScratch <- function(data, p, SNR=estSNR(data, p)){
    force(SNR)
    Y <- dimensionalityReduction(data, p, SNR)
    Y <- t(Y)
    indices <- array(0, p)
    # the matrix A stores the projection of the estimated endmembers sianatures
    A <- matrix(0, nrow = p, ncol = p)
    A[p, 1] <- 1
    for(i in 1:p){
        #getting vector f orthonormal to the space spanned by A
        w <- stats::rnorm(p, sd = 1)
        f <- (diag(p) - A %*% ginv(A)) %*% w
        f <- f / sqrt(sum(f^2))
        #projecting data onto f
        v <- crossprod(f, Y)
        #getting index of the maximal projection
        k <- which.max(abs(v))
        #ith column of A is set to estimated endmember
        A[, i] <- Y[, k]
        indices[i] <- k
    }
    
    #computation of mixing matrices
    # if(SNR > SNRth){
    #     M <- U_d %*% X[, indices]
    # }else{
    #     M <- u_d %*% X[, indices] + r_
    # }
    indices <- sort(indices)
    indices
}
