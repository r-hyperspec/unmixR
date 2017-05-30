##' Michael E. Winter's 1999 N-FINDR Unmixing Algorithm
##' 
##' This technique is based on the fact that, in N spectral dimensions, the
##' N-volume contained by a simplex formed of the purest pixels is larger
##' than any other volume formed from any other combination of pixels.
##' Intended to be called from \code{\link{nfindr}} (see there for examples).
##' 
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param indices Indices used in the simplex estimation.
##'
##' @param iters Max number of iterations, defaults to 3*p.
##'
##' @return The sorted indices of the endmembers in the original dataset.
##'
##' @references Michael E. Winter "N-FINDR: an Algorithm for Fast Autonomous
##'   Spectral End-Member Determination in Hyperspectral Data", Proc.
##'   SPIE 3753, Imaging Spectrometry V, 266 (October 27, 1999), 
##'   doi:10.1117/12.366289
##'
##' @include unmixR-package.R
##' @export
##'
nfindr99 <- function(data, p, indices, iters=3*p) {

  simplex <- .simplex(data, indices)
  nspectra <- nrow(data)

  # calculate the initial volume using the random endmembers
  volume <- abs(det(simplex))
  volume.prev <- -1
  volume.now <- volume

  if (.options ("debuglevel") >= 1L) {
    cat("Initial guess at endmembers:", indices, "\n")
    cat("\tinitial volume:", volume, "\n")
    }

  # keep replacing endmembers until there is never an increase in volume
  # or the max iterations are reached (indicates pure endmembers not found)
  iter <- 1
  while (volume.now > volume.prev && iter <= iters) {

	if (.options ("debuglevel") >= 1L) {
	  cat("Iteration", iter, "\n")
	  cat("\tcurrent endmembers:", sort(indices), "\n")
	  cat("\tvolume:", volume.now, "\n")
	  }

    for (k in 1:p) {
      for (i in 1:nspectra) {
        # store current sample as it may need to be placed back into the
        # simplex after the following replacement
        sample <- simplex[2:p,k] # first row of 1's ignored see Winter1999 Eqn (3)

        # replace the k-th endmember with the i-th reduced spectrum
        # and recalculate the volume
        simplex[2:p,k] <- data[i,]
        testVolume <- abs(det(simplex))

        # if the replacement increased the volume then keep the replacement
        # and the note the spectrum's index
        if (testVolume > volume) {
          volume <- testVolume
          indices[k] <- i
        }
        # otherwise revert the replacement
        else {
          simplex[2:p,k] <- sample
        }
      } # end of (i in 1:nspectra) loop
    } # end of (k in 1:p) loop
    
    iter <- iter+1
    volume.prev <- volume.now
    volume.now <- volume
  } # end of 'while'
  
  indices <- sort(indices)
  indices
}
