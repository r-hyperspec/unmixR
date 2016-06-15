##' Modified Vertex Component Analysis
##'
##' Modified VCA algorithm that aims to reduced the algorithmic complexity of
##' the original.
##' Intended to be called from \code{\link{nfindr}}.
##' 
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##' @param p Number of endmembers
##' @param SNR Signal-to-Noise ratio.
##'
##' @return The indices that indicate the position of the endmembers in the
##'   original dataset
##' 
##' @references Lopez, S.; Horstrand, P.; Callico, G.M.; Lopez, J.F.;
##' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
##' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
##' Geoscience & Remote Sensing Letters, IEEE, vol. 9 no. 3 pp. 502- 506, May 2012
##' doi: 10.1109/LGRS.2011.2172771
##' @export
##' @importFrom stats prcomp

vcaLopezFromScratch <- function(data, p, SNR=estSNR(data, p)){
    data <- t(as.matrix(data))
    SNRth <- 15 + 10 * log10(p)
    N <- nrow(data)
    
    #Dimensionality reduction
    if(SNR > SNRth){
        #d - number of dimensions
        d <- p
        #obtaining projection matrix
        U_d <- svd(tcrossprod(data) / N, d, d)$u
        #projecting data on subspace
        X <- crossprod(U_d, data)
        #rescaling to amlify noise
        u <- apply(X, 1, mean)
        Y <- t(t(X) / crossprod(X, u))
    }else{
        #d - number of dimensions
        d <- p - 1
        #mean is subtracted to aplify noise
        r_ <- apply(data, 1, mean)
        #obtaining projection matrix
        #u <- stats::prcomp(tcrossprod(data - r_) / N)$x[, 1:d] # orig version by AB
        u <- stats::prcomp(tcrossprod(data - r_) / N)
        u_d <- u["x", 1:d]
        #projecting data on subspace
        X <- crossprod(u_d, data - r_)
        #the value of c aasures that collatitude angle betwee u and any vector from X is between 0 and 45
        c <- max(apply(X, 2, function(x){sqrt(sum(x^2))}))
        Y <- rbind(X, c)
    }
    
    #matrix of endmembers
    E <- matrix(0, nrow = p, ncol = p + 1)
    E[p, 1] <- 1
    #U stores the set of orthogonal vectors
    U <- matrix(0, nrow = p, ncol = p)
    #px1 vector
    w <- c(rep(1, p))
    proj_acc <- c(rep(0, p))
    indices <- c(rep(0, p))
    for (i in 1:p) {
        #U_i is initialized with the endmember computed in the last iteration
        U[, i] <- E[, i]
        
        #Gram-Schmidt orthogonalization
        if(i >= 3)
        {
            for (j in 3:i) {
            proj_ei_uj_1 <- (crossprod(E[, i], U[, j - 1])) / (crossprod(U[, j - 1])) * U[, j - 1]
            U[, i] <- U[, i] - proj_ei_uj_1
            }
        }
        #U_i is orthogonal to other i-1 vectors
        
        #projecting w onto U_i
        proj_w_ui <- (crossprod(w, U[, i])) / (crossprod(U[, i])) * U[, i]
        
        #vector f is orthogonal to to the subspace spaned by columns of E
        proj_acc <- proj_acc + proj_w_ui
        f <- w - proj_acc
        
        #projection accumulator is reset on the first iteration
        if(i == 1){
            proj_acc <- c(rep(0, p))
        }
        #projecting data onto f
        v <- crossprod(f, Y)
        #getting index of the maximal projection
        index <- which.max(abs(v))
        indices[i] <- index
        #estimated endmember is stored in E
        E[, i + 1] <- Y[, index]
    }
    indices <- sort(indices)
    indices
}