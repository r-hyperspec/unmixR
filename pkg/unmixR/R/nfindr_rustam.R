##' Generalized N-FINDR unmixing algorithm
##'
##' TODO: Add a description
##'
##' @param data Data matrix to unmix.
##'
##' @param p Number of endmembers.
##'
##' @param indices Indices used in the simplex estimation.
##'
##' @param iter Iterate estimations over
##'
##' @return The indices of the endmembers in the original dataset.
##'
##' @references TODO: Add references
##'
##' @rdname nfindr
##' @export
##'
nfindr_rustam <- function(data, p, indices,
                          iter = c("points", "endmembers", "both"),
                          estimator = c("volume", "height", "Cramer", "cofactor", "LDU"),
                          iter_max = 10,
                          debug.level = 0) {

  ## Parse and validate function arguments -----
  iter <- tolower(match.arg(iter))
  estimator <- tolower(match.arg(estimator))

  # get the selected nfindr method
  nfindr_func <- get0(
    paste(".nfindr", estimator, iter, sep = "_"),
    mode = "function"
  )
  if (is.null(nfindr_func)) {
    stop("Invalid options iter and/or estimator parameters")
  }

  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }

  # for "both" type iteration increase the number of iteration
  # to approximately similar amount that "points" estimator would have
  if (iter == "both") {
    iter_max <- iter_max * p
  }

  # transform the input into a matrix
  data <- as.matrix(data)
  m <- nrow(data)
  n <- ncol(data)

  ## Reduce dimension, if required ----
  # TODO: Add MNF
  # TODO: No dimension reduction
  # reduce the dimension of the data using PCA
  # do nothing if the data was passed in already reduced
  if (n != p - 1) {
    data <- stats::prcomp(data)[["x"]][, sequence(p - 1), drop = FALSE]
  }

  ## Pre-filter initial endmembers -------
  # TODO: Add different initialization strategies
  if (is.null(indices)) {
    indices <- sample(m, p)
  }

  ## Do N-FINDR -----
  nfindr_func(data, indices, iter_max = iter_max, debug.level = debug.level)
}

# Testing ----------

.test(nfindr_rustam) <- function() {
  context("N-FINDR")
  
  ## Test exceptions ----
  test_that("Exceptions", {
    # invalid p
    expect_error(nfindr(triangle, p = "---"))
    expect_error(nfindr(triangle, p = 0))

    # test: nfindr produces error for invalid method
    expect_error(nfindr(triangle, p, method = "invalid"))
  })

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
  estimators <- c("volume", "height", "Cramer", "cofactor", "LDU") # formals(nfindr)$estimator

  ## Endmembers in the inner-most loop -------
  for (estimator in estimators) {
    if (estimator == "LDU")
      next
    
    test_that(
      paste0("Trivial case: ", estimator, " - endmembers in the inner-most loop"),
      {
        result <- nfindr_rustam(data, p, indices, iter = "endmembers", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(result$endmembers[order(result$indices), ], data[best_indices, ])
        expect_equal(names(result), c("indices", "endmembers"))

        result <- nfindr_rustam(data, p, indices, iter = "endmembers", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))

        result <- nfindr_rustam(data, p, indices, iter = "endmembers", estimator = estimator, debug.level = 2)
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
        result <- nfindr_rustam(data, p, indices, iter = "points", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(result$endmembers[order(result$indices), ], data[best_indices, ])
        expect_equal(names(result), c("indices", "endmembers"))

        result <- nfindr_rustam(data, p, indices, iter = "points", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))

        result <- nfindr_rustam(data, p, indices, iter = "points", estimator = estimator, debug.level = 2)
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
        result <- nfindr_rustam(data, p, indices, iter = "both", estimator = estimator)
        expect_equal(sort(result$indices), best_indices)
        expect_equal(
          result$endmembers[order(result$indices), ],
          data[best_indices, ]
        )
        expect_equal(names(result), c("indices", "endmembers"))

        result <- nfindr_rustam(data, p, indices, iter = "both", estimator = estimator, debug.level = 1)
        expect_equal(result$iterations_count, 4)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "endmembers", "iterations_count", "replacements_count"))

        result <- nfindr_rustam(data, p, indices, iter = "both", estimator = estimator, debug.level = 2)
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
  iter <- c("endmembers", "points", "both")
  for(estimator in estimators) {
    for (iterator in iter) {
      test_that(
        paste("Non-trivial case:", estimator, iterator),
        expect_equal(
          nfindr_rustam(data, p, indices, iter=iterator, estimator = estimator, debug.level = 2),
          nfindr_rustam(data, p, indices, iter=iterator, estimator = "volume", debug.level = 2)
        )
      )
    }
  }
}
