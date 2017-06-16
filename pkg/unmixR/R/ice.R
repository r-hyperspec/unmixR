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
##' Default value is the recommendation from XXX.
##'
##' @param tol Tolerance value from [0 \ldots 1] that affects number of iterations.
##' The higher the value the more iterations.  Default value is the recommendation from XXX.
##'
##' @param reduce Logical.  Should the data be dimensionally reduced? If
##' \code{nrow(data) > ncol(data)} the minimum noise fraction will be computed,
##' otherwise PCA will be carried out.  Defaults to \code{TRUE}. Set to \code{FALSE}
##' if you pre-process your data, or if you don't want data reduction.  This last
##' option will be slow for large data sets.
##'
##' @return A list of class \code{ice} with elements:
##'   \itemize{
##'     \item \strong{endmembers}: The \code{p} endmembers.
##'     \item \strong{abundance}: Abundance matrix.
##'     \item \strong{reduce}: How and if the data was reduced.
##'   }
##'  \eqn{abundances * endmembers} gives an approximation of the original data matrix
##'  if the data was reduced.
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
ice <- function(data, p, mu = 0.01, tol = 0.9999, reduce = TRUE){

	data <- as.matrix(data) # brings in hyperSpec objects seamlessly
	reduced <- "no"
	
	# Default is to reduce the data.
    # mnf requires nrow(x) >= ncol(x) for the matrix calc to work, do so if possible,
    # otherwise use PCA.
    # Save the entire transformed/reduced data structure, as we need it for reconstruction
    
    if (reduce) {
    	
	    if (nrow(data) > ncol(data)) { # Can use mnf in this case, it is preferred
	    	if (.options ("debuglevel") >= 1L) cat("Using minimum noise transform\n")
	    	red <- spacetime::mnf(data)
	    	# Literature is a bit unclear as to the order of components.
	    	# Some say order is highest SNR to lowest, other the opposite.
	    	# This gives much better looking endmembers if the last components are used.
	    	#keep <- seq(p) # keep the first p components
	    	keep <- (ncol(data)-p+1):ncol(data) # keep the last p components
	    	data <- red[["x"]][, keep, drop = FALSE]
	    	reduced <- "MNF"
		}
	
	    if (!nrow(data) > ncol(data)) { # Must use PCA in this case
	      if (.options ("debuglevel") >= 1L) cat("Reducing via PCA\n")
	      red <- stats::prcomp(data)
	      data <- red[["x"]][, seq(p), drop = FALSE]
	      reduced <- "PCA"
    	}
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
            lsei(A = curEnd, B = spectrum, E = rep(1, p), F = 1, diag(1, p), rep(0, p))[["X"]]
        })
        
        # Regularisation parameter
        # BH: should this be dim(data)[2]?  data is transposed, no of samples is ncol(data)
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
    }
    
    # Reconstruct the original data dimensions but of reduced rank
    curEnd <- t(curEnd)
    if (reduce) curEnd <- curEnd %*% t(red$rotation[,seq(p)]) +
                          red$center # center = FALSE = 0 in mnf by the way

    ans <- list(endmembers = curEnd, abundances = t(abund), reduce = reduced)
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
    chk <- ice(laser, p = 3, reduce = FALSE)
    M <- chk$abundances %*% chk$endmembers
    expect_lt (mean(M - as.matrix(laser)), 1e-10)
  })

}
