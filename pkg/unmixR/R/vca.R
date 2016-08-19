##' General Interface to Vertex Component Analysis Spectral Unmixing
##' Implementations
##' 
##' This algorithm is based on the geometry of convex sets. It exploits the
##' fact that endmembers occupy the vertices of a simplex.
##' 
##' @param data Data matrix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row.
##'
##' @param formula Formula object.
##'
##' @param frame Data frame containing the hyperspectral data.
##'
##' @param p Number of endmembers.
##'
##' @param SNR The Signal-to-Noise ratio of the data. By default it will be
##'   estimated using \code{\link{estSNR}}.
##'
##' @param method The VCA algorithm to use. Options:
##'   \itemize{
##'     \item 05 (\code{\link{vca05}})
##'     \item Lopez2012 (\code{\link{vcaLopez2012}})
##'   }
##'   Default: 05.
##'
##' @param seed vca05 generates a random vector. Set
##'   the random number generator seed with this argument.
##'
##' @param EMonly Boolean that indicates whether the \code{data} parameter
##'   should be stored in the resulting structure.
##'
##' @param ... Additional parameters for the methods (currently unused).
##' 
##' @return A list which contains:
##'   \itemize{
##'     \item \strong{data}: the original data.
##'     \item \strong{indices}: the indices of the calculated endmembers.
##'   }
##' 
##'
##' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
##' to determine abundances of endmembers in each sample.
##'
##' @rdname vca
##' @export
##' @include unmixR-package.R
##'
##' @examples
##' data(demo_data)
##' demo <- vca(demo_data, 2, method = "05")
##' em <- endmembers(demo)
##' em <- rbind(demo_data[c(7,9),], em)
##' em[3:4,] <- em[3:4,] + 0.5 # a small offset for the found em's
##' matplot(t(em), type = "l",
##'    col = c("black", "blue", "black", "blue"), lty = c(1, 1, 2, 2),
##'    xlab = "frequency", ylab = "intensity",
##'    main = "mvca of demo_data")
##' leg.txt <- c("Endmember 2", "Endmember 3", "Endmember 2 (found)", "Endmember 3 (found)")
##' legend("topright", leg.txt, col = c("black", "blue", "black", "blue"),
##' lty = c(1, 1, 2, 2), cex = 0.75)

vca <- function(...) {
  UseMethod("vca")
}
