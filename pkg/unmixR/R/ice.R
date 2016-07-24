##'Iterated Constrained endmembers
##'
##'@export

ice <- function(data, p, mu = 0.01, t = 0.9){

    data <- t(as.matrix(data))
    curEnd <- data[, sample(dim(data)[2], p)]
    
    t_i <- 0
    t_i_1 <- 0
    nIter <- 0
    while(TRUE){
        nIter <- nIter + 1
        abund <- apply(data, 2, function(spectrum){
            limSolve::lsei(A = curEnd, B = spectrum, E = rep(1, p), F = 1, diag(1, p), rep(0, p))$X
        })

        lambda <- dim(data)[1] * mu / ((p - 1)*(1 - mu))
        
        curEndTransposed <- solve(abund %*% t(abund) + lambda*(diag(p) - 1/p * matrix(rep(1, p*p), nrow = p))) %*% (abund %*% t(data))
        curEnd <- t(curEndTransposed)
        
        v <- matrixcalc::matrix.trace(cov(curEnd))
        t_i_1 <- t_i
        t_i <- (1 - mu)/dim(data)[1]*matrixcalc::frobenius.norm(data - curEnd %*% abund) + mu*v

        if(nIter > 1){
            if (t_i / t_i_1 >= t) {
                break
            }
        }
    }
    list(e = curEnd, a = abund)
}
