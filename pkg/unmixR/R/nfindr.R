##' General Interface to N-FINDR Spectral Unmixing Implementations
##'
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##'
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain one spectrum per row. This data will
##'   be dimensionally reduced using PCA without scaling. If you want to reduce the data using
##'   some other method then reduce it first to \code{p - 1} and the PCA step
##'   will automatically be skipped.
##'
##' @param formula Formula object.
##'
##' @param frame Data frame containing the hyperspectral data.
##'
##' @param p Number of endmembers (will be coerced to integer).
##'
##' @param method The N-FINDR algorithm to use. Options:
##'   \itemize{
##'     \item 99 (\code{\link{nfindr99}})
##'     \item LDU (\code{\link{nfindrLDU}})
##'     \item SeqLDU (\code{\link{nfindrSeqLDU}})
##'     \item Brute (\code{\link{nfindrBrute}})
##'   }
##'   Default: LDU as it generally performs the best.
##'
##' @param indices Rows in the dataset that will be used to
##'   form the initial simplex. Default: Randomly selected indices.
##'   If you need reproducibility, specify the indices.
##'   Irrelevant if \code{method = "Brute"} is chosen since all
##'   possibilities will be computed.
##'
##' @param ... Additional parameters for the methods (currently unused).
##'
##' @param EMonly Boolean that indicates whether the original \code{data}
##'   should be returned in the resulting structure.  If \code{TRUE} the data
##'   element of the returned list will hold only the endmembers.
##' 
##' @return A list of class \code{nfindr} which contains:
##'   \itemize{
##'     \item \strong{data}: Either the original data or just the endmembers if
##'                          \code{EMonly = TRUE}.
##'     \item \strong{indices}: The indices the endmembers in the suppled \code{data}.
##'   }
##'
##' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
##' to determine abundances of endmembers in each sample.
##'
##' @examples
##' data(demo_data)
##' demo <- nfindr(demo_data, 2, method = "99")
##' em <- endmembers(demo)
##' em <- rbind(demo_data[c(3,7),], em)
##' em[3:4,] <- em[3:4,] + 0.5 # a small offset for the found em's
##' matplot(t(em), type = "l",
##'    col = c("black", "red", "black", "red"), lty = c(1, 1, 2, 2),
##'    xlab = "frequency", ylab = "intensity",
##'    main = "N-FINDR 99 of demo_data")
##' leg.txt <- c("Endmember 1", "Endmember 2", "Endmember 1 (found)", "Endmember 2 (found)")
##' legend("topright", leg.txt, col = c("black", "red", "black", "red"),
##' lty = c(1, 1, 2, 2), cex = 0.75)
##'
##' @rdname nfindr
##' @export
##' @include unmixR-package.R

nfindr <- function (...) {
  UseMethod("nfindr")
}
