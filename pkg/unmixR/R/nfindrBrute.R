##' Brute Force N-FINDR
##'
##' This method exhaustively checks every possible simplex that could be
##' formed from \code{p} points and returns the indices that generate the simplex
##' with the largest volume. This should indicate the endmembers based on the
##' theory of N-FINDR. It should only be used for testing purposes as it
##' is extremely slow for non-trivial datasets.
##' Intended to be called from \code{\link{nfindr}}.
##'
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param ... Extra unused parameters passed in from 
##' \code{\link{nfindr}}.
##'
##' @param debuglevel If \code{>= 1L}, print top simplices with their 
##' corresponding volume.
##' 
##' @return The indices of the endmembers in the original dataset or 
##' a data.frame holding indices and corresponding volume if \code{volume = TRUE}.
##'
##' @export
##' @importFrom utils combn tail

nfindrBrute <- function(data, p, ..., debuglevel = .options ("debuglevel")) {
  # generate all possible unique combinations of p indices
  combos <- combn(nrow(data), p, simplify=TRUE)
  n <- ncol(combos)

  # generate the volumes of all the simplexes using the indices
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]

    ## calling simplex_volume takes approx 2.5x as long, so leave it this way
    simplex <- cbind (rep(1, p), data [idx,])
    abs (det (simplex))
  })

  if (debuglevel >= 1L) {
    volumes <- volumes / factorial (p - 1)
    
    DF <- as.data.frame(cbind(t (combos), volumes))
  	DF <- DF [order(DF$volumes),]
  
  	message("Top endmember combinations & their volumes:\n")
  }
  
  if (debuglevel >= 1L) 
  if (debuglevel == 1L)
    message (tail (DF), row.names = FALSE)
  else if (debuglevel > 1L)
    message (DF, row.names = FALSE)
  
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
    expect_equal(nfindr (.testdata$x, "Brute", p = 3, debuglevel = 1)$indices, .correct)
  })

  test_that("correct output for laser data", {
    expect_equal(nfindr (laser, "Brute", p = 2)$indices, .correct.laser)
  })
  
}
