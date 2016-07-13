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
##' @param get.volumes Boolean indicating whether a data.frame with the actual 
##' volumes should be returned.
##' 
##' @param debuglevel If \code{>= 1L}, print all simplices with their 
##' corresponding volume.
##' 
##' @return The indices of the endmembers in the original dataset or 
##' a data.frame holding indices and corresponding volume if \code{volume = TRUE}.
##'
##' @export
##' @importFrom utils combn

nfindrBrute <- function(data, p, ..., get.volumes = FALSE, debuglevel = .options ("debuglevel")) {
  # generate all possible unique combinations of p indices
  combos <- combn(nrow(data), p, simplify=TRUE)
  n <- ncol(combos)

  # generate the volumes of all the simplexes using the indices
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]

    # simplex <- rbind(rep(1, p), data[idx,])
    simplex <- rbind(rep(1, p), t(data[idx,])) # Bryan's fix
    
    abs(det(simplex))
  })

  if (debuglevel >= 1L || get.volumes) {
  	rownames (combos) <- paste0 ("Index", seq_len(ncol (combos)))
    DF <- as.data.frame(cbind(t (combos), volumes))
    print (colnames (DF))
  	DF <- DF [order(DF$volumes),]
  }
  
  if (debuglevel >= 1L) 
    cat("Endmember combinations & their volumes:\n")
  if (debuglevel == 1L)
    print (tail (DF), row.names = FALSE)
  else if (debuglevel > 1L)
    print (DF, row.names = FALSE)
  
  

  if (get.volumes) {
    DF
  } else {
    ## return the indices that formed the largest simplex
    col <- which.max (volumes)
    
    indices <- combos [, col]
    indices
  }
}
