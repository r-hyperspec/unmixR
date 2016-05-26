##' Vertex Component Analysis unmixing algorithm
##'
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##'
##'@export

vcaLopezFromScratch <- function(data, p){
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
        v <- t(f) %*% data
        index <- which.max(abs(v))
        indices[i] <- index
        E[, i + 1] <- data[, index]
    }
    indices <- sort(indices)
    indices
}