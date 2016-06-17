##' Brute Force N-FINDR
##'
##' This method exhaustively checks every possible simplex that could be
##' formed from \code{p} points and returns the indices that generated the simplex
##' with the largest volume. This should indicate the endmembers based on the
##' theory of N-FINDR. It should only be used for testing purposes as an
##' exhaustive verification of the results from other N-FINDR methods as it
##' is extremely slow for non-trivial datasets.
##' Intended to be called from \code{\link{nfindr}}.
##'
##' @param data Data matrix to unmix
##' @param p Number of endmembers
##' @param ... Extra unused parameters that get passed in from
##'   \code{\link{nfindr}}
##' @return The indices that indicate the position of the endmembers in the
##'   original dataset
##' @export
##' @importFrom utils combn

nfindrBrute <- function(data, p, ...) {
  # generate all possible unique combinations of p indices
  combos <- combn(nrow(data), p, simplify=TRUE)
  n <- ncol(combos)

  # generate the volumes of all the simplexes using the indices
  volumes <- sapply(1:n, function(i) {
    idx <- combos[,i]

    #    simplex <- rbind(rep(1, p), data[idx,])
    simplex <- rbind(rep(1, p), t(data[idx,])) # Bryan's fix
    
    abs(det(simplex))
  })

  # return the indices that formed the largest simplex
  col <- which.max(volumes)

  indices <- sort(combos[, col])
  indices
}
