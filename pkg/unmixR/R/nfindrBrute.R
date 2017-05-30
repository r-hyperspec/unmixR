##' Brute Force N-FINDR
##'
##' This method exhaustively checks every possible simplex that could be
##' formed from \code{p} points and returns the indices that generate the simplex
##' with the largest volume. This should indicate the endmembers based on the
##' theory of N-FINDR. It should only be used for testing purposes as it
##' is extremely slow for non-trivial datasets.
##' Intended to be called from \code{\link{nfindr}} (see there for examples).
##'
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param ... Extra unused parameters passed in from 
##' \code{\link{nfindr}}.
##'
##' @return The sorted indices of the endmembers in the original dataset.
##'
##' @export
##' @importFrom utils combn tail

nfindrBrute <- function(data, p, ...) {
  # generate all possible unique combinations of p indices
  combos <- combn(nrow(data), p, simplify=TRUE)
  n <- ncol(combos)

  # generate the volumes of all the simplexes using the indices
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]
    simplex <- cbind (rep(1, p), data [idx,])
    abs (det (simplex))
  })

  if (.options ("debuglevel") >= 1L) {
    volumes <- volumes / factorial (p - 1)   
    DF <- data.frame(ind = t(combos), volume = volumes)
  	DF <- DF [order(DF$volume),]

  	if (.options ("debuglevel") == 1L) {
  		message("\nTop endmember combinations & their volumes:\n")
    	print (tail (DF), row.names = FALSE)
  	}
  	
   	if (.options ("debuglevel") > 1L) {
  		message("\nAll endmember combinations & their volumes:\n")
    	print (DF, row.names = FALSE)
  	}
 	
  }
  
  ## return the indices that formed the largest simplex
  col <- which.max (volumes)
  
  indices <- combos [, col]
  indices
}


.test (nfindrBrute) <- function (){
  context ("nfindrBrute")
  expect_true (require (hyperSpec))
  
  test_that("correct output for triangle", {
    expect_equal(nfindr (.testdata$x, "Brute", p = 3)$indices, .correct)
  })

  test_that("correct output for laser data", {
    expect_equal(nfindr (laser, "Brute", p = 2)$indices, .correct.laser)
  })
  
}
