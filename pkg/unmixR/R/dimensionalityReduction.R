##' Dimensionality reduction
##' 
##' Reduces dimensionality of data
##' 
##' @noRd
##' 

dimensionalityReduction <- function(data, p, SNR){
    
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
        u <- apply(X, 1, mean)
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
        c <- max(apply(X, 2, function(x){sqrt(sum(x^2))}))
        Y <- rbind(X, c)
    }
    Y
}