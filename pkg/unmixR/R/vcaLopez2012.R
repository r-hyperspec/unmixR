##' Modified Vertex Component Analysis
##'
##' Modified VCA algorithm that aims to reduced the algorithmic complexity of
##' the original.
##' Intended to be called from \code{\link{vca}}.
##' 
##' @param data Data matrix. Samples in rows frequencies in columns.
##'
##' @param p Number of endmembers.
##'
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}.
##'   
##' @note for \code{debuglevel}s 1 and 2 debug information is printed (1) and plotted (2).
##'
##' @return The indices of the endmembers in the original dataset.
##' 
##' @references Lopez, S., Horstrand, P., Callico, G.M., Lopez J.F. and
##' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
##' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
##' Geoscience & Remote Sensing Letters, IEEE, vol. 9 no. 3 pp. 502-506, May 2012
##' doi: 10.1109/LGRS.2011.2172771
##' @export
##' @importFrom stats prcomp
##' @importFrom graphics title points


vcaLopez2012 <- function(data, p, SNR = estSNR(data, p)) {
   Y <- t(data)
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
                proj_ei_uj_1 <- (crossprod(E[, i], U[, j - 1]) / crossprod(U[, j - 1])) * U[, j - 1]
                U[, i] <- U[, i] - proj_ei_uj_1
            }
        }
        #U_i is orthogonal to other i-1 vectors
        
        #projecting w onto U_i
        proj_w_ui <- (crossprod(w, U[, i]) / crossprod(U[, i])) * U[, i]
        
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

        if (.options ("debuglevel") >= 1L){
          cat("Iteration", i, "\n")
          cat("\tcurrent endmembers:", sort(indices[1:i]), "\n")
          
          ## To monitor the process, capture the volume
          ## of the current simplex using the same process
          ## as in nfindr.default, except the data set
          ## grows with each iteration
          inds <- indices[1:i] # limit to non-zero indices
          red_data <- stats::prcomp(data)[["x"]][, sequence(length(inds)-1), drop=FALSE]
          simplex <- .simplex(red_data, inds)
          vol <- abs(det(simplex))
          cat("\tvolume:", vol,  "\n")
          cat ("\tmax:", which.max (v), "\t min:", which.min (v), "\n")
        }

        if (.options ("debuglevel") >= 2L){
          plot (t (v))
          title (main = paste ("Iteration:", i))
          tmp <- c (indices [i], which.max (v), which.min (v))
          tmp <- tmp [! is.na (tmp)]
          points (tmp, v [tmp],  col = 2 : 4, pch = c (19, 20, 20))
        }
    }
    indices <- sort(indices)
    indices
}
