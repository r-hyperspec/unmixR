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
##' @param drop Boolean that indicates whether the \code{data} parameter
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
##' @rdname nfindr
##' @export
##' @include unmixR-package.R
##' @importFrom svUnit checkException checkTrue checkEquals

nfindr <- function (...) {
  UseMethod("nfindr")
}

.test(nfindr) <- function() {
  context ("N-FINDR")
  
  data <- as.matrix(laser)
  p <- 2
  indices <- c(1, 2)

  # test: nfindr produces error for invalid values of p

  test_that ("Exceptions", {
    # invalid p
    expect_error (nfindr(data, p="---"))
    expect_error (nfindr(data, p=0))  
    
    # test: nfindr produces error for invalid method
    expect_error (nfindr(data, p, method="invalid"))
  })

  ## test that at least the implementations provided by unmixR are available
  implementations <- get.implementations("nfindr")
  test_that ("Implementations available", {
    expect_true (all (c ("99", "Brute", "LDU", "SeqLDU") %in% implementations))
  })

  # test: nfindr default produces the correct answer
  correct <- c (4, 79)
  test_that ("correct endmembers laser data, default method", {
    output <- nfindr(data, p)$indices
    expect_equal (output, correct)
  })

  # test: all N-FINDR methods produce the same output
  test_that ("All implementations return correct results for laser data", {
    outputs <- sapply(implementations, 
                      function(i) {
                        nfindr(data = data, p = p, method = i)$indices
                      })
    expect_true(all (outputs == correct))
  })
  
  ## test triangle data
  triangle <- .testdata$x
  triangle.correct <- .correct

  test_that ("correct endmembers triangle data, default method", {
    output <- nfindr(triangle, p = 3)$indices
    expect_equal (output, triangle.correct)
  })
  
  test_that ("All implementations return correct results for triangle data", {
    outputs <- sapply(implementations, 
                      function(i) {
                        nfindr(data = triangle, p = 3, method = i)$indices
                      })
    expect_true(all (outputs == triangle.correct))
  })

  # test: check the formula interface
  test_that ("Formula Interface", {
    expect_equal(nfindr (~ ., as.data.frame (data), p)$indices, correct)  
  })
  
  # test: check other (hyperSpec) objects
  test_that ("hyperSpec object", {
    output <- nfindr (laser, 2)
    expect_equal (output$data, laser)  
    expect_equal (output$indices, correct)
  })
}
