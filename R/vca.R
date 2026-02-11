#' General Interface to Vertex Component Analysis Spectral Unmixing
#' Implementations
#'
#' This algorithm is based on the geometry of convex sets. It exploits the
#' fact that endmembers occupy the vertices of a simplex.
#'
#' @param data Data matrix. It will be converted to a matrix using
#'   as.matrix. The matrix should contain a spectrum per row. If dimension
#'   of the data is higher than \code{p}, \code{\link{vca_dr}} is applied
#'   to reduce the data to \code{p} dimensions before running projections.
#'
#' @param p Number of endmembers.
#'
#' @param method The VCA algorithm to use. Options:
#'   \itemize{
#'     \item nascimento (\code{\link{vca_nascimento}})
#'     \item lopez (\code{\link{vca_lopez}})
#'   }
#'   Default: \code{nascimento}.
#' 

#' @return A list which contains:
#'   \itemize{
#'     \item \strong{indices}: sorted indices of the calculated endmembers.
#'     \item \strong{projection_vectors}: projection vectors in iteration
#'       order, included only when \code{debuglevel >= 1}.
#'     \item \strong{unsorted_indices}: unsorted list of indices,
#'       i.e., in the same order as iteration, included only
#'       when \code{debuglevel >= 1}.
#'   }
#' 
#'
#' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
#' to determine abundances of endmembers in each sample.
#'
#' @rdname vca
#' @export
#' @include unmixR-package.R
#'
#' @examples
#' data(demo_data)
#' demo_red <- vca_dr(demo_data, p = 2)
#' demo <- vca(demo_red, p = 2, method = "nascimento")
#' em <- endmembers(demo, demo_data)
#' em <- rbind(demo_data[c(7, 9), ], em)
#' em[3:4,] <- em[3:4,] + 0.5 # a small offset for the found em's
#' matplot(t(em), type = "l",
#'    col = c("black", "blue", "black", "blue"), lty = c(1, 1, 2, 2),
#'    xlab = "frequency", ylab = "intensity",
#'    main = "mvca of demo_data")
#' leg.txt <- c("Endmember 2", "Endmember 3", "Endmember 2 (found)", "Endmember 3 (found)")
#' legend("topright", leg.txt, col = c("black", "blue", "black", "blue"),
#' lty = c(1, 1, 2, 2), cex = 0.75)
vca <- function(data, p, method = c("nascimento", "lopez"), ...) {

  # check if the method passed in is valid
  method <- match.arg (method)

  # transform the input into a matrix
  data <- as.matrix (data)

  # check for p being with the valid range, >= 2
  if (!is.numeric (p) || p < 2 || p > ncol (data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }

  vcaFunc <- get(paste("vca", method, sep="_"), mode = "function")
  res <- vcaFunc(data)

  if (.options("debuglevel") >= 1L) {
    res[["unsorted_indices"]] <- res$indices
    res$indices <- sort(res$indices)
  }
  class(res) = "vca"
  return(res)

}


.test(vca) <- function() {
  context ("vca")

  # Note: .testdata$x matches all columns of .testdata, which are x.L1, x.L2, x.L3.

  reduced <- vca_dr(.testdata$x, p = 3)

  test_that ("vca produces error for invalid values of p", {
    expect_error (vca (.testdata$x, p = "---"))
    expect_error (vca (.testdata$x, p = 0))
    expect_error (vca (.testdata$x, p = 1))
    expect_error (vca (.testdata$x, p = 4))
  })

  test_that("vca produces error for invalid method", {
    expect_error(vca(reduced, p=3, method="invalid"))
  })

  test_that("vca requires input with exactly p columns", {
    expect_error(vca(.testdata$x, p = 2), "Use `vca_dr\\(\\)` first")
  })


  # test correct calculations for the available methods
  implementations <- get.implementations("vca")
  test_that("Implementations available", {
    expect_equal(sort(implementations), c("lopez", "nascimento"))
  })

  test_that("correct results for all available methods: triangle data", {
    # FIXME: Fix the tests below ASAP
    skip("Skip tests to implement GHA infrastructure. Fix the tests ASAP")

    for (i in implementations) {
      expect_equal (vca (.testdata$x, p = 3, method = i)$indices, .correct)

      indices <- vca (.testdata$x, p = 2, method = i)$indices
      expect_true (all (indices %in% .correct), info = i)

      if (i == "Lopez2012") skip ("temporarily disabled: known issue #36")
      expect_false (any (duplicated (indices)), info = i)
    }
  })

  test_that ("no duplicates with Lopez2012 for test data", {
    skip ("known issue: #36")

    indices <- replicate (10, vca (.testdata$x, p = 2, method = "Lopez2012")$indices)
    expect_true (all (indices %in% .correct))
    expect_true (all (indices [1, ] != indices [2, ]), info = "Lopez2012 duplicate indices: testdata, p = 2")
  }
  )

  test_that("correct results for all available methods: laser data", {
    skip ("temporarily disabled")
    for (i in implementations) {
      expect_equal (vca (laser$spc, p = 2, method = i)$indices, .correct.laser)
    }
  })


  ## all 3 components should be recovered, vca output is sorted.
  test_that("vca output is sorted", {
    # FIXME: Fix the tests below ASAP
    skip("Skip tests to implement GHA infrastructure. Fix the tests ASAP")

    indices <- vca (.testdata$x, p = 3)$indices
    expect_equal(indices, sort (indices))
  })

  # test: hyperSpec object
  test_that("vca on hyperSpec object", {
      expect_equal (vca (laser, p = 2, seed = 12345)$indices,
                    vca (laser$spc, p = 2, seed = 12345)$indices)
  })
}
