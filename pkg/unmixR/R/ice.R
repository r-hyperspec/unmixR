##' Iterated Constrained endmembers
##'
##' This method iteratively computes endmembers and abundances using quadratic
##' programming until it converges to a solution.
##'
##' @param data Data matrix. Samples in rows, frequencies in columns.
##'
##' @param p Integer.  Desired number of endmembers.
##'
##' @param mu Regularization parameter on [0 \ldots 1] that penalizes the model 
##' for large simplex volume. The smaller the value the bigger the simplex.
##' Default value is the recommendation from the reference.
##'
##' @param tol Tolerance value from [0 \ldots 1] that affects number of iterations.
##' The higher the value the more iterations.  Default value is the recommendation
##' from the reference.
##'
##' @param reduce Character.  Chosen from \code{c("MNF", "PCA", "no")}.  Defaults to \code{"MNF"},
##' which is the recommended approach. However, using \code{"MNF"} is only possible if 
##' \code{nrow(data) > ncol(data)}.  When this is not the case, \code{"PCA"} is the next best
##' option. Set to \code{"no"} if you pre-process your data, or if you don't want data reduction.
##' This last option will be impractical for large data sets.  If you pre-process your data,
##' check your results with and without centering.  The code herein does not center.
##'
##' @return A list of class \code{ice} with elements:
##'   \itemize{
##'     \item \strong{endmembers}: The \code{p} endmembers.
##'     \item \strong{weights}: Weights matrix.
##'     \item \strong{reduce}: How and if the data was reduced.
##'   }
##'  \eqn{weights * endmembers} gives an approximation of the original data matrix
##'  (the approximation depends on how/whether the data was reduced, and the nummber
##'   of endmembers requested). \code{\link{matchEM}} will match the returned 
##'  endmembers against the raw data, and returns the indices of the best match.
##'
##' @references Berman et al. ICE: A New Method for the Multivariate Curve Resolution
##' of Hyperspectral Images, Journal of Chemometrics, vol. 29 pgs 101-116 (2009).
##'
##' @export
##'
##' @importFrom stats rnorm prcomp
##' @importFrom limSolve lsei
##' @importFrom matrixcalc matrix.trace frobenius.norm
##' @importFrom spacetime mnf
##' 
##' @examples
##' tst <- ice(laser, p = 3)
##' 
ice <- function(data, p, mu = 0.01, tol = 0.9999, reduce = "MNF"){

	data <- as.matrix(data) # brings in hyperSpec objects seamlessly
	reduced <- "no"
	
	# Default is to reduce the data using mnf, however
    # mnf requires nrow(x) >= ncol(x) for the matrix calc to work.
    # Save the entire transformed/reduced data structure, as we need it for reconstruction
    
    if (reduce == "MNF") {
	   if (.options ("debuglevel") >= 1L) cat("Using minimum noise fraction\n")
	   if (!nrow(data) > ncol(data)) stop("Cannot use MNF unless nrow(data) > ncol(data)")
	   red <- spacetime::mnf(data) # mnf does not center
	   # Literature is a bit unclear as to the order of components.
	   # Some say order is highest SNR to lowest, other the opposite.
	   # This gives much better looking endmembers if the last components are used.
	   #keep <- seq(p) # keep the first p components
	   keep <- (ncol(data)-p+1):ncol(data) # keep the last p components
       data <- red[["x"]][, keep, drop = FALSE]
       reduced <- "MNF"
	}
	
    if (reduce == "PCA") {
      if (.options ("debuglevel") >= 1L) cat("Reducing via PCA\n")
      red <- stats::prcomp(data, center = FALSE) # do not center, it leads to poor results
      data <- red[["x"]][, seq(p), drop = FALSE]
      reduced <- "PCA"
	}
	
    data <- t(data)
	
    # Select several points as initial endmembers
    means <- rowMeans(data)
    meanMatrix <- matrix(rep(means, p), ncol = p)
    curEnd <- matrix(rnorm(nrow(data) * p), ncol = p)
    curEnd <- curEnd + meanMatrix
    
    t_i <- 0
    t_i_1 <- 0
    nIter <- 0
    while(TRUE){
        nIter <- nIter + 1
        # Acquiring abundances with nonnegativity and sum-to-one constraints via quadratic programming
        abund <- apply(data, 2, function(spectrum){
			lsei(A = curEnd, B = spectrum, E = rep(1, p), F = 1, G = diag(1, p), H = rep(0, p))[["X"]]
        })
        
        # Regularisation parameter
        # BH asks: should this be dim(data)[2]?  data is transposed, no of samples is ncol(data)
        # See eqn 14 in Berman2009
        # Also see a few lines below
        lambda <- dim(data)[1] * mu / ((p - 1)*(1 - mu))
        
        # Acquiring current endmembers from previously calculated abundances
        curEndTransposed <- solve(abund %*% t(abund) + lambda*(diag(p) - 1/p * matrix(rep(1, p*p), nrow = p))) %*% (abund %*% t(data))
        curEnd <- t(curEndTransposed)
        
        # Checking convergence conditions
        v <- matrix.trace(cov(curEnd))
        t_i_1 <- t_i
        t_i <- (1 - mu)/dim(data)[1]*frobenius.norm(data - curEnd %*% abund) + mu*v

        if(nIter > 1){
            if (t_i / t_i_1 >= tol) {
                break
            }
        }
    } # end of while
    
    curEnd <- t(curEnd)

    # If data was reduced, reconstruct the original data dimensions but of reduced rank
    if ((reduce == "MNF") | (reduce == "PCA")) {
      curEnd <- curEnd %*% t(red[["rotation"]][,seq(p)]) # data was not previously centered
	}
	
    ans <- list(endmembers = curEnd, weights = t(abund), reduce = reduced)
    class(ans) <- "ice"
    return(ans)
}

.test(ice) <- function() {
  context ("ice")
  
  # Misc. front end tests
  
  expect_true(require (hyperSpec))
  
  test_that ("Exceptions for invalid values of p", {
  	# Are we checking input?
    expect_error (ice(.triangle$x, p="---"))
    expect_error (ice(.triangle$x, p = 0))  
  })

  # Check correct answers
  # This is a pretty crude test!  hist(mean(M - as.matrix(laser))) shows the truth
  test_that ("ice on laser w/o reduction returns original data matrix within reason", {
    chk <- ice(laser, p = 3, reduce = "no")
    M <- chk$weights %*% chk$endmembers
    expect_lt (mean(M - as.matrix(laser)), 1e-6)
  })

}
