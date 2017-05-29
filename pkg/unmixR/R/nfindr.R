##' General Interface to N-FINDR Spectral Unmixing Implementations
##'
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##'
##' @param data Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row. This data will
##'   be dimensionally reduced using PCA. If you want to reduce the data using
##'   some other method then reduce it first and set drop to \code{TRUE}.
##'
##' @param formula Formula object.
##'
##' @param frame Data frame containing the hyperspectral data.
##'
##' @param p Number of endmembers.
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
##' @param indices Locations of the rows in the dataset that will be used to
##'   form the initial simplex. Default: Randomly selected indices.
##'
##' @param ... Additional parameters for the methods (currently unused).
##'
##' @param EMonly Boolean that indicates whether the \code{data} parameter
##'   should be stored in the resulting structure. This should only be set to
##'   \code{TRUE} when \code{data} was passed in already reduced.
##' 
##' @return A list which contains:
##'   \itemize{
##'     \item \strong{data}: the original data or reduced data if drop is
##'                          set to \code{TRUE}.
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers. If drop is set to
##'                             \code{TRUE} then indices will be 1 to p.
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

.test(nfindr) <- function() {
  context ("N-FINDR")
  
  ## test triangle data
  triangle <- .testdata$x
  correct.triangle <- .correct
  
  expect_true(require (hyperSpec))
  
  # test: nfindr produces error for invalid values of p

  test_that ("Exceptions", {
    # invalid p
    expect_error (nfindr(triangle, p="---"))
    expect_error (nfindr(triangle, p = 0))  
    
    # test: nfindr produces error for invalid method
    expect_error (nfindr(triangle, p, method="invalid"))
  })

  ## test that at least the implementations provided by unmixR are available
  implementations <- get.implementations("nfindr")
  test_that ("Implementations available", {
    expect_true (all (c ("99", "Brute", "LDU", "SeqLDU") %in% implementations))
  })

  # test: nfindr default produces the correct answer
  test_that ("correct endmembers by default method for laser", {
    expect_equal (nfindr(laser, p = 2)$indices, .correct.laser)
  })

  # test: all N-FINDR methods produce the same output
  test_that ("All implementations return correct results for laser data", {
    for (i in implementations)
      expect_equal(nfindr (laser, method = i, p = 2)$indices, .correct.laser)
  })
  
  test_that ("correct endmembers triangle data, default method", {
    expect_equal (nfindr(triangle, p = 3)$indices, correct.triangle)
  })
  
  test_that ("All implementations return correct results for triangle data", {
    for (i in implementations) {
      
      if (i == "LDU")
        skip ("LDU skipped: known issue #38")
      
      expect_equal(nfindr (triangle, method = i, p = 3)$indices, correct.triangle,
                   info = i)
    }
  })

  # test: check the formula interface
  # -> nfindr.formula has its own test
  
  # test: check other (hyperSpec) objects
  test_that ("hyperSpec object", {
    output <- nfindr (laser, 2)
    expect_equal (output$data, laser@data$spc)
    expect_equal (output$indices, .correct.laser)
  })
}
