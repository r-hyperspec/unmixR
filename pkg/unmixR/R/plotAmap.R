##' Plot an Abundance Map Based on Endmembers
##' 
##' Convenience function to take a \code{hyperSpec} object, and the results
##' of unmixing that object, and create an abundance map using the
##' specified endmembers.
##' 
##' @param hS An object of class \code{hyperSpec}.
##'
##' @param uM An object of class \code{nfindr}, \code{vca}, or \code{ice}
##'   from the unmixing analysis of \code{hS}.
##'
##' @param EMs Integer.  The endmembers to include in the map.
##'
##' @param ret Character.  One of \code{c("Amap", "EM")}.  In the first case, return
##'        the abundance map data.  In the second case, return the endmembers.  In
##'        both cases a \code{hyperSpec} object is returned invisibly.
##'
##' @param plot Logical.  Shall the abundance map be plotted?
##'
##' @param ... Additional parameters to be passed to levelplot/hyperSpec::plotmap.
##' 
##' @return A list of class \code{vca} which contains:
##'   \itemize{
##'     \item \strong{data}: Either the original data or just the endmembers if
##'                          \code{EMonly = TRUE}.
##'     \item \strong{indices}: The indices the endmembers in the suppled \code{data}.
##'     \item \strong{seed}: The value of \code{seed} if \code{debuglevel >= 1}. 
##'   }
##' 
##' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
##' to determine abundances of endmembers in each sample.
##'
##' @export
##' @include unmixR-package.R
##'
##' @examples
##' unmix <- vca(chondro, p = 3, method = "Lopez2012")
##' plotAmap(chondro, unmix, EMs = 1:3)
##' 

plotAmap <- function(hS, uM, EMs = 1, ret = "Amap", plot = TRUE, ...) {


  if (length(EMs) > length(uM$indices)) {
    msg <- "More endmembers requested than exist.\n\tCheck length(EMs) & the value of p used in unmixing." 
    stop(msg) 	
  }
  
  if ((class(uM) == "vca") | (class(uM) == "nfindr")) {
  	
    no.em <- length(uM$indices)
    no.spc <- nrow(hS[])
	EM <- hS # make a copy to hold endmembers

	# Remove all the spectra except leave enough rows for the endmembers
	EM <- EM[-c((no.em + 1):no.spc)]
	EM[[]] <- endmembers(uM) # extract and insert endmembers

	# Build abundance map data
	abun <- .predict(uM)
	map <- abun[,EMs] %*% uM$data[uM$indices[EMs],, drop = FALSE]
	MAP <- hS # make a copy to hold abundance map
	MAP[[]] <- map
	if (plot) p <- hyperSpec::plotmap(MAP, ...); print(p)	
  }

  if (class(uM) == "ice") {
  	stop("mapping ice objects not yet implemented")
  }

  if (ret == "Amap") ans <- MAP
  if (ret == "EM") ans <- EM
  invisible(ans)
}
