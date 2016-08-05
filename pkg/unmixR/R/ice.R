##' Iterated Constrained endmembers
##'
##' This method iteratively computes endmembers and abundances using quadratic
##' programming until it converges to a solution.
##'
##' @param data Data matrix. Samples in rows frequencies in columns.
##'
##' @param p Number of endmembers.
##'
##' @param mu Regularization parameter from 0 to 1 that penaltizes the model 
##' for large simplex volume. The smaller the value the bigger the simplex
##'
##' @param t Tolerance ratio from 0 to 1 that affects number of iterations.
##' The higher the value the more iterations.
##'
##' @return Structure with endmembers and abundances such that \eqn{abundances * endmembers = data_}
##' where data_ is an approximation of data. Endmembers are a matrix with samples in rows frequencies in columns.
##'
##' @references M. Berman, H. Kiiveri, R. Lagerstrom, A. Ernst, R. Dunne, and
##' J. F. Huntington, “Ice: A statistical approach to identifying endmembers
##' in hyperspectral images: Learning from Earth’s shapes and colors,”
##' IEEE Trans. Geosci. Remote Sens., vol. 42, no. 10, pp. 2085–2095, Oct. 2004.
##' 
##' E. M. Sigurdsson, A. Plaza and J. A. Benediktsson, "GPU Implementation of 
##' Iterative-Constrained Endmember Extraction from Remotely Sensed Hyperspectral
##' Images," in IEEE Journal of Selected Topics in Applied Earth Observations and
##' Remote Sensing, vol. 8, no. 6, pp. 2939-2949, June 2015.
##' doi: 10.1109/JSTARS.2015.2441699
##'
##' @export
##'
##' @importFrom stats rnorm
##' @importFrom limSolve lsei
##' @importFrom matrixcalc matrix.trace frobenius.norm
##'
ice <- function(data, p, mu = 0.01, t = 0.99){

    data <- t(as.matrix(data))
    #selecting several points as initial endmembers
    #curEnd <- data[, sample(dim(data)[2], p)]
    means <- rowMeans(data)
    meanMatrix <- matrix(rep(means, p), ncol = p)
    curEnd <- matrix(rnorm(nrow(data) * p), ncol = p)
    curEnd <- curEnd + meanMatrix
    
    t_i <- 0
    t_i_1 <- 0
    nIter <- 0
    while(TRUE){
        nIter <- nIter + 1
        #acquiring abundances with nonnegativity and sum-to-one constraints via quadratic programming
        abund <- apply(data, 2, function(spectrum){
            lsei(A = curEnd, B = spectrum, E = rep(1, p), F = 1, diag(1, p), rep(0, p))[["X"]]
        })
        #reqularisation parameter
        lambda <- dim(data)[1] * mu / ((p - 1)*(1 - mu))
        #acquiring current endmembers from previously calculated abundances
        curEndTransposed <- solve(abund %*% t(abund) + lambda*(diag(p) - 1/p * matrix(rep(1, p*p), nrow = p))) %*% (abund %*% t(data))
        curEnd <- t(curEndTransposed)
        
        #checking convergence conditions
        v <- matrix.trace(cov(curEnd))
        t_i_1 <- t_i
        t_i <- (1 - mu)/dim(data)[1]*frobenius.norm(data - curEnd %*% abund) + mu*v

        if(nIter > 1){
            if (t_i / t_i_1 >= t) {
                break
            }
        }
    }
    list(endmembers = t(curEnd), abundances = t(abund))
}
