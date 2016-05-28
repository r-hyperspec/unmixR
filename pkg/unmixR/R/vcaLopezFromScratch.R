
##' Vertex Component Analysis unmixing algorithm
##'
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##'
##'@export

vcaLopezFromScratch <- function(data, p, SNR=estSNR(data, p)){
    data <- t(as.matrix(data))
    
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
    
    E <- matrix(0, nrow = p, ncol = p + 1)
    E[p, 1] <- 1
    U <- matrix(0, nrow = p, ncol = p)
    w <- c(rep(1, p))
    proj_acc <- c(rep(0, p))
    indices <- c(rep(0, p))
    for (i in 1:p) {
        U[, i] <- E[, i]
        if(i >= 3)
        {
            for (j in 3:i) {
            proj_ei_uj_1 <- (t(E[, i]) %*% U[, j - 1]) / (t(U[, j - 1]) %*% U[, j - 1]) * U[, j - 1]
            U[, i] <- U[, i] - proj_ei_uj_1
            }
        }
        
        proj_w_ui <- (t(w) %*% U[, i]) / (t(U[, i]) %*% U[, i]) * U[, i]
        proj_acc <- proj_acc + proj_w_ui
        f <- w - proj_acc
        if(i == 1){
            proj_acc <- c(rep(0, p))
        }
        v <- t(f) %*% Y
        index <- which.max(abs(v))
        indices[i] <- index
        E[, i + 1] <- Y[, index]
    }
    indices <- sort(indices)
    indices
}