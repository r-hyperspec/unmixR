##' General Interface to N-FINDR Spectral Unmixing Implementations
##'
##' All the N-FINDR techniques are based on the fact that, in N spectral
##' dimensions, the N-volume contained by a simplex formed of the purest
##' pixels is larger than any other volume formed from any other combination
##' of pixels.
##'
##' @param x Data to unmix. It will be converted to a matrix using
##'   as.matrix. The matrix should contain a spectrum per row. If the dimension
##'   of the data is larger than `p-1` then it will be dimensionally reduced
##'   using PCA. If you want to reduce the data using some other method then
##'   reduce it first and provide the data of `p-1` dimension.
##'
##' @param formula Formula object.
##'
##' @param p Number of endmembers.
##' 
##' @param indices Locations of the rows in the dataset that will be used to
##'   form the initial simplex. Default: Randomly selected indices.
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
##' @param debug.level Debug level. Controls the level of detalization of the
##'   output. By default, is 0. If it is 1, then number of replacements also will
##'   be returned. 2 - also will add list of vertices at each iteration.
##'
##' @param ... Additional parameters for the methods (currently unused).
##'
##' @param keep_data Boolean that indicates whether the actual data used for 
##'   the calculation (i.e. after dimension reduction) should be stored in the
##'   resulting structure. 
##' 
##' @return A list which contains:
##'   \itemize{
##'     \item \strong{data}: the original data or reduced data if \code{keep_data}
##'                          is set to \code{TRUE}.
##'     \item \strong{indices}: the indices of the spectra which increased
##'                             the simplex volume the most. These are the
##'                             indices of the endmembers.
##'     \item \strong{endmembers}: the vectors of endmembers in reduced data space.
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
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(result$endmembers[order(result$indices), ], data[best_indices, ])
        expect_equal(names(result), c("indices", "endmembers"))
        
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))
        
        result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator, debug.level = 2)
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
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count", "replacements"))
      }
    )
  }
  
  ## Points in the inner-most loop -------
  for (estimator in estimators) {
    test_that(
      paste0("Trivial case: ", estimator, " - points in the inner-most loop"),
      {
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(result$endmembers[order(result$indices), ], data[best_indices, ])
        expect_equal(names(result), c("indices", "endmembers"))
        
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))
        
        result <- nfindr(data, p, indices, iter = "points", estimator = estimator, debug.level = 2)
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
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count", "replacements"))
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
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(
          result$endmembers[order(result$indices), ],
          data[best_indices, ]
        )
        expect_equal(names(result), c("indices", "endmembers"))
        
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 4)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))
        
        result <- nfindr(data, p, indices, iter = "both", estimator = estimator, debug.level = 2)
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
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count", "replacements"))
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
  iter <- c("endmembers", "points", "both")
  for(estimator in estimators) {
    for (iterator in iter) {
      test_that(
        paste("Non-trivial case:", estimator, iterator),
        # The iteration steps and the final solution must be the same as we use
        # straightforward volume calculation
        expect_equal(
          nfindr(data, p, indices, iter=iterator, estimator = estimator, debug.level = 2),
          nfindr(data, p, indices, iter=iterator, estimator = "volume", debug.level = 2)
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
