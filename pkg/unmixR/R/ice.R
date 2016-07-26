##'Iterated Constrained endmembers
##'
##'@export

ice <- function(data, p, mu = 0.01, t = 0.9){

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
            limSolve::lsei(A = curEnd, B = spectrum, E = rep(1, p), F = 1, diag(1, p), rep(0, p))$X
        })
        #reqularisation parameter
        lambda <- dim(data)[1] * mu / ((p - 1)*(1 - mu))
        #acquiring current endmembers from previously calculated abundances
        curEndTransposed <- solve(abund %*% t(abund) + lambda*(diag(p) - 1/p * matrix(rep(1, p*p), nrow = p))) %*% (abund %*% t(data))
        curEnd <- t(curEndTransposed)
        
        #checking convergence conditions
        v <- matrixcalc::matrix.trace(cov(curEnd))
        t_i_1 <- t_i
        t_i <- (1 - mu)/dim(data)[1]*matrixcalc::frobenius.norm(data - curEnd %*% abund) + mu*v

        if(nIter > 1){
            if (t_i / t_i_1 >= t) {
                break
            }
        }
    }
    list(endmembers = t(curEnd), abundances = t(abund))
}
