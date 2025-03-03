##' General Interface to N-FINDR Spectral Unmixing Implementations
##'
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##'
##' @param x Data to unmix (spectra in rows). It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row. It is recommended
##'   to reduce the dimensionality of the data to `p-1` before using this
##'   function. This can be done using PCA or other dimensionality reduction
##'   techniques. Withouth dimensionality reduction, the results might be
##'   inefficient and computation intensive.
##'
##' @param p Number of endmembers.
##' 
##' @param init Initialization strategy. 
##'   \itemize{
##'     \item vector of `p` integers - manually selected initial points, can be output of
##'       previous another endmember extraction method, e.g. VCA
##'     \item random - randomly selected points
##'     \item projections - selecting the two extreme points of the
##'       projections of the data onto random vectors
##'     \item coordinates - selecting the two extreme points of the
##'       projections of the data onto the coordinate axes
##'   }
##'   Default: "projections" is used.
##'
##' @param iter The iteration strategy. Options: "points", "endmembers",
##'   "both". By default, "points" are used.
##' 
##' @param estimator Volume change estimator
##'   \itemize{
##'     \item volume - straight forward volume calculation without any
##'       optimization
##'     \item height - Use the fact that the simplex volume is proportional to 
##'       the product of `height` and `base volume`.
##'     \item Cramer - Using Cramer's rule
##'     \item LDU - Using LDU matrix decomposition
##'     \item cofactor - Using the cofactor expansion for calculating `det(E)` 
##'   }
##'   Default: Cramer's rules is used since it has best performance.
##'
##' @param iter_max Maximum number of iterations to make.
##'
##' @param ... Additional parameters for the methods (currently unused).
##'
##' @return A list which contains:
##'   \itemize{
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers.
##'     \item \strong{iterations_count}: if debug level higher than 0, number of
##'                                      loop iterations.
##'     \item \strong{replacements_count}: if debug level higher than 0, number of
##'                                      actual replacements during iterations.
##'     \item \strong{replacements}: if debug level higher than 1, the vectors of
##'                                  indices at all replacement steps. If fact,
##'                                  is used to see how the simplex was growing.  
##'   }
##'
##' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
##' to determine abundances of endmembers in each sample.
##'
##' @examples
##' data(demo_data)
##' demo <- nfindr(demo_data, 2)
##' em <- demo_data[demo$indices,]
##' em <- rbind(demo_data[c(3,7),], em)
##' em[3:4,] <- em[3:4,] + 0.5 # a small offset for the found em's
##' matplot(t(em), type = "l",
##'    col = c("black", "red", "black", "red"), lty = c(1, 1, 2, 2),
##'    xlab = "frequency", ylab = "intensity",
##'    main = "N-FINDR of demo_data")
##' leg.txt <- c("Endmember 1", "Endmember 2", "Endmember 1 (found)", "Endmember 2 (found)")
##' legend("topright", leg.txt, col = c("black", "red", "black", "red"),
##' lty = c(1, 1, 2, 2), cex = 0.75)
##'
##' @rdname nfindr
##' @export
##' @include unmixR-package.R

nfindr <- function (x, ...) {
  UseMethod("nfindr")
}


.test(nfindr) <- function() {
  context("N-FINDR")
  
  expect_true(require (hyperSpec))
  
  ## Prepare data for trivial tests ----
  # For visualization:
  # > plot(data, pch=16)
  # > points(data[indices,], pch=17, col="red")
  # > points(data[best_indices,], pch=17, col="green")
  # This dataset provides a case when all three inner-loop
  # options give the same result, but upate of points is done
  # in different order.
  # NOTE: The order of points matters
  set.seed(923)
  vertices <- rbind(c(-5, 0), c(0, 4), c(10, 0))
  initial_points <- rbind(c(0, 0), c(-1, 0), c(0, 1))
  data <- rbind(
    initial_points,
    vertices,
    .get_simplex_points(vertices)
  )
  indices <- 1:3
  best_indices <- 4:6
  p <- length(indices)
  estimators <- get.implementations ("nfindr")
  estimators <- estimators[estimators != "Brute"]
  print(estimators)
  #c("volume", "height", "Cramer", "cofactor", "LDU")
  # formals(nfindr)$estimator

  ## Test exceptions ----
  test_that("Exceptions", {
    # invalid p
    expect_error(nfindr(data, p = "---"))
    expect_error(nfindr(data, p = 0))
    
    # test: nfindr produces error for invalid iteration or volume change estimator
    expect_error(nfindr(data, p, estimator = "invalid"))
    expect_error(nfindr(data, p, iter = "invalid"))
  })
  
  ## Endmembers in the inner-most loop -------
  for (estimator in estimators) {
    if (estimator == "LDU")
      next
    
    test_that(
      paste0("Trivial case: ", estimator, " - endmembers in the inner-most loop"),
      {
        unmixR.options(debuglevel = 0L)
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(names(result), c("indices"))
        
        unmixR.options(debuglevel = 1L)
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count"))
        
        unmixR.options(debuglevel = 2L)
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        expect_equal(
          result$replacements,
          rbind(
            c(1, 2, 3),
            c(1, 4, 3),
            c(1, 4, 5),
            c(6, 4, 5)
          ),
          check.attributes = FALSE
        )
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count", "replacements"))
      }
    )
  }
  
  ## Points in the inner-most loop -------
  for (estimator in estimators) {
    test_that(
      paste0("Trivial case: ", estimator, " - points in the inner-most loop"),
      {
        unmixR.options(debuglevel = 0L)
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(names(result), c("indices"))
        
        unmixR.options(debuglevel = 1L)
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count"))
        
        unmixR.options(debuglevel = 2L)
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator)
        expect_equal(
          result$replacements,
          rbind(
            c(1, 2, 3),
            c(6, 2, 3),
            c(6, 5, 3),
            c(6, 5, 4)
          ),
          check.attributes = FALSE
        )
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count", "replacements"))
      }
    )
  }
  
  ## Both in inner-most loop -------
  for (estimator in estimators) {
    if (estimator == "LDU")
      next
    
    test_that(
      paste0("Trivial case: ", estimator, " - both in the inner-most loop"),
      {

        unmixR.options(debuglevel = 0L)
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(names(result), c("indices"))
        
        unmixR.options(debuglevel = 1L)
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        expect_equal(result$iterations_count, 4)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count"))
        
        unmixR.options(debuglevel = 2L)
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        expect_equal(
          result$replacements,
          rbind(
            c(1, 2, 3),
            c(6, 2, 3),
            c(6, 2, 5),
            c(6, 4, 5)
          ),
          check.attributes = FALSE
        )
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count", "replacements"))
      }
    )
  }
  
  ## Prepare data for non-trivial tests ----
  set.seed(518)
  data <- .get_simplex_points(
    rbind(
      c(0,0,1),
      c(0,1,0),
      c(1,0,0),
      c(-1,0,0)
    ),
    max_coefficient = rep(0.6,4),
    n_points = 2000
  )
  p <- ncol(data)+1
  indices <- sample(which(apply(data, 1, norm, type="2") < 0.3), p)
  
  ## Test non-trivial case ----
  unmixR.options(debuglevel = 2L)
  iter <- c("endmembers", "points", "both")
  for(estimator in estimators) {
    for (iterator in iter) {
      test_that(
        paste("Non-trivial case:", estimator, iterator),
        # The iteration steps and the final solution must be the same as we use
        # straightforward volume calculation
        expect_equal(
          nfindr(data, p, indices, iter=iterator, estimator = estimator),
          nfindr(data, p, indices, iter=iterator, estimator = "volume")
        )
      )
    }
  }

  ## Test the formula interface ----
  # -> nfindr.formula has its own test
  
  ## Test other (hyperSpec) objects ----
  test_that ("hyperSpec object", {
    pca <- prcomp(laser$spc)
    output <- nfindr (pca$x[,1,drop=FALSE], 2)
    expect_equal (output$indices, .correct.laser)
  })
}
