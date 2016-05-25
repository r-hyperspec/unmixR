##' Vertex Component Analysis unmixing algorithm
##'
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data to unmix.  TODO
##' @param p Number of endmembers
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}
##'
##' @return Although the other VCA algorithms return only indices, this
##'   function returns the full structure to \code{\link{vca}} as the
##'   endmembers should be taken out of a projected dataset which it generates
##'
##' @references Nascimento, J.M.P.; Bioucas Dias, J.M., "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp.898-910, April 2005,
##'   doi: 10.1109/TGRS.2005.844293
##' @export
 

vcaFromScratch <- function(data, p, SNR=estSNR(data, p)){
    SNRth <- 15 + 10 * log10(p)
    N <- nrow(data)
    if(SNR > SNRth){
        d <- p
        U_d <- svd((data %*% t(data)) / N, d, d)$u
        X <- t(U_d) %*% data
        u <- apply(X, 1, mean)
        Y <- t(t(X) / (t(X) %*% u))
    }else{
        d <- p - 1
        r_ <- apply(data, 1, mean)
        u_d <- prcomp((data - r_) %*% t(data - r_) / N)$x[, 1:d]
        X <- t(u_d) %*% (data - r_)
        c <- max(apply(X, 2, function(x){sqrt(sum(x^2))}))
        Y <- rbind(X, c)
    }
    indices <- array(0, p)
    A <- matrix(0, nrow = p, ncol = p)
    A[p, 1] <- 1
    for(i in 1:p){
        w <- rnorm(p, sd = 1)
        f <- (diag(p) - A %*% ginv(A)) %*% w
        f <- f / sqrt(sum(f^2))
        v <- t(f) %*% Y
        k <- which.max(abs(v))
        A[, i] <- Y[, k]
        indices[i] <- k
    }
    if(SNR > SNRth){
        M <- U_d %*% X[, indices]
    }else{
        M <- u_d %*% X[, indices] + r_
    }
    indices <- sort(indices)
    indices
}