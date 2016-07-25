##' LDU N-FINDR: 2013 Improved N-FINDR Algorithm Based on LU Decompositions
##' 
##' This approach calculates M LU decompositions, one with each column
##' permuted to the last position, and reuses those decompositions on each
##' pixel until a permanent replacement requires the calculation of a new set
##' of decompositions.
##' Intended to be called from \code{\link{nfindr}}.
##' 
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param indices Indices used in the simplex estimation.
##'
##' @param debuglevel If \code{>= 1L}, print simplices with their 
##' corresponding volume.
##' 
##' @param ... Extra unused parameters passed in from
##'   \code{\link{nfindr}}.
##'
##' @return The indices of the endmembers in the original dataset.
##'   
##' @references  Dowler, Shaun W., Takashima, Reymond, and Andrews, Mark
##'   "Reducing the complexity of the N-FINDR algorithm for hyperspectral
##'   image analysis.", IEEE Trans Image Process. 2013 22(7):2835-2848
##'   doi: 10.1109/TIP.2012.2219546
##'
##' @export
##'

nfindrLDU <- function (data, p, indices, ..., debuglevel = unmixR.options("debuglevel")) {
  
  if (debuglevel >= 2L) print (indices)
  
  simplex <- .simplex (data, p, indices)
  nspectra <- nrow (data)
  pm1 <- sequence (p - 1) # create a range from 1 to p minus 1

  # local function
  update <- function(simplex, p) {
    g <- matrix (0, nrow = p, ncol = p - 1)
    V <- pm1

    for (i in sequence (p)) { 
      dup <- simplex
      
      # swap the i-th and p-th columns of the simplex
      dup [, c (p, i)] <- dup [, c (i, p)]

      # get the partitioned components of the simplex matrix
      A <- dup [pm1, pm1]
      b <- dup [pm1, p]
      c <- dup [p,   pm1]
      d <- dup [p,   p]

      g [i,] <- crossprod(c, solve(A))
      V [i] <- as.numeric(abs (d - crossprod (g [i,], b)))
    }

    list(simplex=simplex, V=V, g=g)
  } # end of local function

  vars <- update (simplex, p)
  simplex <- vars$simplex
  g <- vars$g
  V <- vars$V

  replace <- TRUE
  Vtest <- 0 # initialize for debug reporting
  iter <- 1 # initialize for debug reporting
	  
  while (replace == TRUE) {

	if (.options ("debuglevel") >= 1L) {
	  cat("Iteration", iter, "\n")
	  cat("\tcurrent endmembers:", sort(indices), "\n")
	  cat("\tvolume:", Vtest, "\n")
	  }

    replace <- FALSE

    for (j in sequence (nspectra)) {
      y <- c (1, data [j,])
      bj <- y [ sequence (p - 1)]
      dj <- y [p]

      for (i in sequence (p)) {
        Vtest <- as.numeric (abs (dj - crossprod (g [i,], bj)))

        if (Vtest > V[i] && !(j %in% indices)) {
          replace <- TRUE
          simplex [,i] <- y
          indices [i] <- j
          if (debuglevel >= 2L) print (indices)
          
          vars <- update (simplex, p)
          simplex <- vars$simplex
          g <- vars$g
          V <- vars$V
        } # end of if (Vtest)
      } # end of for (i in 1:p)
    } # end of for (j in 1:nspectra)

    iter <- iter + 1
    
  } # end of while

  indices <- sort (indices)

  indices
}

.test (nfindrLDU) <- function (){
  context ("nfindrLDU")

  ## nfindrLDU fails when simplex is on straight line
  test_that ("nfindrLDU with simplex corners being straight line", {
  expect_equal (nfindrLDU (data = .testdata$x [, 1 : 2], p = 3, indices = 1 : 3),
                .correct)    
  })  

}
