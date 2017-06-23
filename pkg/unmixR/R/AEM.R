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


  if (length(EMs) > length(uM$indices)) {
    msg <- "More endmembers requested than exist.\n\tCheck length(EMs) & the value of p used for unmixing." 
    stop(msg) 	
  }
  
  if ((class(uM) == "vca") | (class(uM) == "nfindr")) {
  	
    no.em <- length(uM$indices)
    no.spc <- nrow(hS[])
	EM <- hS # make a copy to hold endmembers

	# Remove all the spectra except leave enough rows for the endmembers
	EM <- EM[-c((no.em + 1):no.spc)]
	EM[[]] <- endmembers(uM) # extract and insert endmembers
	# Add extra info marking the endmembers to facilitate plot conditioning
	EM$em <- as.factor(LETTERS[1:no.em])
	
	# Build abundance map data
	abun <- .predict(uM)
	map <- abun[,EMs] %*% uM$data[uM$indices[EMs],, drop = FALSE]
	MAP <- hS # make a copy to hold abundance map
	MAP[[]] <- map
  }

  if (class(uM) == "ice") {
  	stop("mapping ice objects not yet implemented")
  }

  if (plotMap) {p <- hyperSpec::plotmap(MAP, ...); print(p)}
#  if (plotEM) {p <- hyperSpec::plotspc(EM[EMs,,], ...); print(p)}
  if (plotEM) {hyperSpec::plotspc(EM[EMs,,], ...)}

  ans <- list(Endmembers = EM, Map = MAP)
  invisible(ans)
}
