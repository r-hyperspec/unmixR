##' Abundance Map and Endmembers
##' 
##' Convenience function to take a \code{hyperSpec} object, and the results
##' of unmixing that object, and create an abundance map using the
##' specified endmembers.  The map data and the ALL endmembers are returned.
##' 
##' @param hS An object of class \code{hyperSpec}.
##'
##' @param uM An object of class \code{nfindr}, \code{vca}, or \code{ice}
##'   from the unmixing analysis of \code{hS}.
##'
##' @param EMs Integer.  The endmembers to include in the map.
##'
##' @param plotMap Logical.  Shall the abundance map be plotted?
##'
##' @param plotEM Logical.  Shall the endmembers be plotted?
##' You can add \code{stacked = TRUE} for example.
##'
##' @param ... Additional parameters to be passed to levelplot/hyperSpec::plotmap.
##'
##' @section Details:
##' If both \code{plotMap and plotEM = TRUE}, both will be plotted and the endmember
##' plot will overwrite the abundance map in interactive use.
##' 
##' @return A list with two \code{hyperSpec} objects:
##'   \itemize{
##'     \item \strong{Endmembers}: ALL of the endmembers that were present in \code{uM}.
##'     \item \strong{Map}: The map constructed from the endmembers specified by \code{EMs}.
##'   }
##' 
##' @export
##' @include unmixR-package.R
##'
##' @examples
##' unmix <- vca(chondro, p = 3, method = "Lopez2012")
##' aem1 <- AEM(chondro, unmix, EMs = 1:3) # plot the map
##' aem2 <- AEM(chondro, unmix, EMs = 1:3, stacked = TRUE,
##'   plotMap = FALSE, plotEM = TRUE) # plot the endmembers
##' 


AEM <- function(hS, uM, EMs = 1, plotMap = TRUE, plotEM = FALSE, ...) {

  # As written, I think this will fail if uM was returned using EMonly = TRUE
  
  
  if ((class(uM) == "vca") | (class(uM) == "nfindr")) {

	  if (length(EMs) > length(uM$indices)) {
	    msg <- "More endmembers requested than exist.\n\tCheck length(EMs) & the value of p used for unmixing." 
	    stop(msg) 	
	  }
	  	
	  EM <- new("hyperSpec", spc = endmembers(uM), wavelength = wl(hS),
			label = list(spc = hS@label$spc, wavelength = hS@label$.wavelength))
		
	  weights <- .predict(uM)
	  map <- weights[,EMs] %*% uM$data[uM$indices[EMs],, drop = FALSE]
  }

  if (class(uM) == "ice") {

    if (length(EMs) > nrow(uM[["endmembers"]])) {
      msg <- "More endmembers requested than exist.\n\tCheck length(EMs) & the value of p used for unmixing." 
      stop(msg) 	
    }

	EM <- new("hyperSpec", spc = uM[["endmembers"]], wavelength = wl(hS),
		label = list(spc = hS@label$spc, wavelength = hS@label$.wavelength))

	# Build abundance map data
	map <- uM[["weights"]][,EMs] %*% uM[["endmembers"]][EMs,, drop = FALSE]
  }

  # Make a copy to hold abundance map
  # Need all the extra data and labels for the map
  MAP <- hS
  MAP[[]] <- map

  if (plotMap) {
  	if (!"x" %in% colnames(MAP)) stop("hyperSpec object does not have an x column, cannot make a map")
  	if (!"y" %in% colnames(MAP)) stop("hyperSpec object does not have an y column, cannot make a map")
  	p <- hyperSpec::plotmap(MAP, ...)
  	print(p)
  }
  
  if (plotEM) {hyperSpec::plotspc(EM[EMs,,], ...)}

  ans <- list(Endmembers = EM, Map = MAP)
  invisible(ans)
}
