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
  # BH: just for the record, in other places where we calc the volume
  # we use rbind and t(data) but the answer is the same eithe way
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]
    simplex <- cbind (rep(1, p), data [idx,])
    abs (det (simplex))
  })

  if (.options ("debuglevel") >= 1L) {
    volumes <- volumes / factorial (p - 1)   
    DF <- data.frame(volRank = NA_integer_, ind = t(combos), volume = volumes)
  	DF <- DF [order(DF[["volume"]], decreasing = TRUE),]
	DF$volRank <- 1:nrow(DF)
	
  	if (.options ("debuglevel") == 1L) {
  		cat("\nTop 25 endmember combinations & their volumes:\n")
    	print (DF[1:25,], row.names = FALSE)
  	}
  	
   	if (.options ("debuglevel") > 1L) {
  		cat("\nTop 1000 endmember combinations & their volumes:\n")
    	print (DF[1:1000,], row.names = FALSE)
  	}
 	
  }
  
  ## return the indices that formed the largest simplex
  col <- which.max (volumes)
  
  indices <- combos [, col]
  indices
}
