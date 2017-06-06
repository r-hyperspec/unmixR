##' Vertex Component Analysis Unmixing Algorithm
##'
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix. Samples in rows, frequencies in columns.
##'
##' @param p Number of endmembers.
##'
##'
##' @return The indices of the endmembers in the original dataset.
##'
##' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
##'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
##'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005,
##'   doi: 10.1109/TGRS.2005.844293
##'
##' @export
##'
##' @importFrom stats rnorm

vca05 <- function(data, p) {
    Y <- t(data)
    indices <- rep(NA_integer_, p)
    
    # Matrix A stores the projection of the estimated endmember signatures
    A <- matrix(0, nrow = p, ncol = p)
    A[p, 1] <- 1
    
    for(i in 1:p){
    	
        # Get vector f orthonormal to the space spanned by A
        w <- stats::rnorm(p, sd = 1)
        f <- (diag(p) - A %*% ginv(A)) %*% w
        f <- f / sqrt(sum(f^2))
        
        # Project data onto f
        v <- crossprod(f, Y)
        
        # Get index of the maximal projection
        k <- which.max(abs(v))
        
        # i-th column of A is set to estimated endmember
        A[, i] <- Y[, k]
        indices[i] <- k
        
        if (.options ("debuglevel") >= 1L){
            cat("Iteration", i, "\n")
            cat("\tcurrent endmembers:", sort(indices[1:i]), "\n")
            # To monitor the process, capture the volume
            # of the current simplex using the same process
            # as in nfindr.default, except the data set
            # grows with each iteration
            inds <- indices[1:i] # limit to non-zero indices
            red_data <- stats::prcomp(data)[["x"]][, sequence(length(inds)-1), drop=FALSE]
            simplex <- .simplex(red_data, inds)
            vol <- abs(det(simplex))
            cat("\tvolume:", vol, "\n")
        }
    }
    
    indices <- sort(indices)
    indices
}
