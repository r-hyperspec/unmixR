##' Dimensionality reduction
##' 
##' Reduces dimensionality of data
##' 
##' @param data Data matrix.
##'
##' @param p Number of endmembers.
##'
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}.
##'
##' @return data with dimensionality equal to 15 + 10*log p
##'
##' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005,
##'   doi: 10.1109/TGRS.2005.844293
##' @export
##' 

dimensionalityReduction <- function(data, p, SNR){
    data <- t(as.matrix(data))
    
    SNRth <- 15 + 10 * log10(p)
    N <- nrow(data)
    
    #Dimensionality reduction
    if(SNR > SNRth){
        #d - number of dimensions
        d <- p
        #obtaining projection matrix
        U_d <- svd(tcrossprod(data) / N, nu =  d, nv = 0)$u
        #projecting data on subspace
        X <- crossprod(U_d, data)
        #rescaling to amlify noise
        u <- rowMeans(X)
        Y <- t(t(X) / as.vector(crossprod(X, u)))
    }else{
        #d - number of dimensions
        d <- p - 1
        
        #mean is subtracted to aplify noise
        #r_ <- apply(data, 1, mean) original
        #r_ <- rowMeans(data) the variable is not used but it will be required if we decide to return abundances
        
        #obtaining projection matrix
        #u <- stats::prcomp(tcrossprod(data - r_) / N) no need to substract mean since pca centers the data
        u <- stats::prcomp(tcrossprod(data) / N)
        
        #		u_d <- u$x[, 1:d] # this approach causes check problems BH
        u_d <- u[["rotation"]][, sequence (d), drop = FALSE]
        
        #projecting data on subspace
        #X <- crossprod(u_d, data - r_) no need to substract mean since pca centers the data
        X <- crossprod(u_d, data)
        
        #the value of c aasures that collatitude angle betwee u and any vector from X is between 0 and 45
        # BH: changed c to ca since c is a key function in R
        ca <- max(apply(X, 2, function(x){sqrt(sum(x^2))}))
        Y <- rbind(X, ca)
    }
    t(Y)
}