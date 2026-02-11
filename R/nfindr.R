#' General Interface to N-FINDR Spectral Unmixing Implementations
#'
#' All the N-FINDR techniques are based on the fact that, in N spectral
#' dimensions, the N-volume contained by a simplex formed of the purest
#' pixels is larger than any other volume formed from any other combination
#' of pixels.
#'
#' @param x Data to unmix (spectra in rows). It will be converted to a matrix using
#'   as.matrix. The matrix should contain a spectrum per row. It is recommended
#'   to reduce the dimensionality of the data to `p-1` before using this
#'   function. This can be done using PCA or other dimensionality reduction
#'   techniques. Withouth dimensionality reduction, the results might be
#'   inefficient and computation intensive.
#'
#' @param p Number of endmembers.
#'
#' @param init Initialization strategy.
#'   \itemize{
#'     \item vector of `p` integers - manually selected initial points, can be output of
#'       previous another endmember extraction method, e.g. VCA
#'     \item list of numeric vectors - multiple initializations, each element should be
#'       a vector of `p` integers representing indices
#'     \item function - a callable function that takes (data, p) as arguments and returns
#'       either numeric indices or a list with 'indices' element (e.g. output of other methods)
#'     \item random - randomly selected points
#'     \item projections - selecting the two extreme points of the
#'       projections of the data onto random vectors
#'     \item coordinates_sequence - selecting extreme points along coordinate axes
#'       in a deterministic sequential manner
#'     \item coordinates_random - selecting extreme points along coordinate axes
#'       with randomization
#'   }
#'   Default: "projections" is used.
#'
#' @param iter The iteration strategy. Options: "points", "endmembers",
#'   "both". By default, "points" are used.
#'
#' @param estimator Volume change estimator
#'   \itemize{
#'     \item volume - straight forward volume calculation without any
#'       optimization
#'     \item height - Use the fact that the simplex volume is proportional to
#'       the product of `height` and `base volume`.
#'     \item Cramer - Using Cramer's rule
#'     \item LDU - Using LDU matrix decomposition
#'     \item cofactor - Using the cofactor expansion for calculating `det(E)`
#'   }
#'   Default: Cramer's rules is used since it has best performance.
#'
#' @param iter_max Maximum number of iterations to make.
#'
#' @param n_init Number of initializations to try. The final result will be
#'   the best output of all initializations. Ignored if specific initial
#'   endmember indices provided. Default: 1.
#'
#' @param ... Additional parameters for the methods (currently unused).
#'
#' @return A list which contains:
#'   \itemize{
#'     \item \strong{indices}: the indices of the spectra which increased
#'                             the simplex volume the most. These are the
#'                             indices of the endmembers.
#'     \item \strong{iterations_count}: if debug level higher than 0, number of
#'                                      loop iterations.
#'     \item \strong{replacements_count}: if debug level higher than 0, number of
#'                                      actual replacements during iterations.
#'     \item \strong{replacements}: if debug level higher than 1, the vectors of
#'                                  indices at all replacement steps. If fact,
#'                                  is used to see how the simplex was growing.
#'   }
#'   The returned object has classes \code{c("nfindr", "pure_endmembers")} and
#'   can be passed directly to \code{\link{abundances}}.
#'
#' @seealso \code{\link{endmembers}} to extract the endmembers; \code{\link{abundances}}
#' to determine abundances of endmembers in each sample
#'
#' @examples
#' data("demo_data")
#'
#' # Reduce data dimensionality with PCA
#' pca <- prcomp(demo_data)
#' x <- pca$x[,1:2]
#'
#' # Perform N-FINDR in reduced space
#' nf <- nfindr(x, p = 3)
#'
#' # Get endmembers both in reduced and original space
#' ems <- endmembers(nf, demo_data)
#' ems_pca <- endmembers(nf, x)
#
#' # Calculate abundances
#' ab <- abundances(nf, demo_data, method = "nnls")
#'
#' # Plot endmembers
#' matplot(t(ems), type = "l")
#'
#' @rdname nfindr
#' @export
#' @include unmixR-package.R
nfindr <- function(
  x,
  p,
  init = c("projections", "random", "coordinates_sequence", "coordinates_random"),
  iter = c("points", "endmembers", "both"),
  estimator = c("Cramer", "volume", "height", "cofactor", "LDU"),
  iter_max = 10,
  n_init = 1,
  ...
  ) {

  m <- nrow(x)
  n <- ncol(x)
  
  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }

  ## Normalize string arguments -----
  iter <- tolower(match.arg(iter))
  estimator <- tolower(match.arg(estimator))
  if (is.character(init)) {
    init <- tolower(match.arg(init))
  }

  # Check dimensions and number of endmembers ------
  if (n != p - 1) {
    warning(
      "Applying N-FINDR without dimension reduction might be inefficient. ",
      "Consider reducing the dimensionality of the data first, e.g. with PCA."
    )
    if (estimator != "height") {
      warning("Note, `estimator` parameter is forced to 'height'.")
    }
    estimator = "height"
  }

  ## Parse init --------
  # Convert init into list where each element of the list is a set of initial indices
  if (is.numeric(init) && (length(init) == p)) {
    # Single numeric vector of indices
    init <- list(init)
    if ( (.options("debuglevel") > 0L) && (n_init != 1L) ) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (is.list(init)) {
    # List of numeric vectors (multiple initializations)
    if ( (.options("debuglevel") > 0L) && (n_init != length(init)) ) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (is.function(init)) {
    # Call the function n_init times
    init <- lapply(1:n_init, function(i) {
      result <- init(x, p)
      # Handle both direct indices and list with indices element
      if (is.list(result) && ("indices" %in% names(result)) ) {
        result$indices
      } else {
        result
      }
    })
  } else if (init == "random") {
    init <- lapply(1:n_init, function(i) sample(m, p))
  } else if (init == "projections") {
    init <- lapply(1:n_init, function(i) random_projections(x, p)$indices)
  } else if (init == "coordinates_sequence") {
    init <- lapply(1:n_init, function(i) extreme_coordinates(x, p, random = FALSE)$indices)
  } else if (init == "coordinates_random") {
    init <- lapply(1:n_init, function(i) extreme_coordinates(x, p, random = TRUE)$indices)
  } else {
    stop("Unexpected `init` value. Must be a string, numeric vector, list of numeric vectors, or function.")
  }

  stopifnot(
    is.list(init) && all(sapply(init, is.numeric)) && all(sapply(init, length) == p)
  )

  ## Check number of outer-most iterations --------
  # for "both" type iteration increase the number of iteration
  # to approximately similar amount that "points" estimator would have
  if (iter == "both") {
    iter_max <- iter_max * p
  }

  ## Check the selected nfindr method --------
  nfindr_func <- get0(
    paste(".nfindr", estimator, iter, sep = "_"),
    mode = "function"
  )
  if (is.null(nfindr_func)) {
    stop("Invalid options iter and/or estimator parameters")
  }

  # transform the input into a matrix
  data <- as.matrix(x)

  ## Do N-FINDR -----
  results_list <- lapply(
    init,
    function(indices) {
      nfindr_func(data, indices, iter_max = iter_max)
    }
  )
  
  # Combine the results
  result <- list()
  
  # Remove duplicate results to avoid unnecessary volume calculations
  # sort the indices to normalize the order between runs
  unique_indices <- unique(t(
    sapply(results_list, function(r) sort(r$indices))
  ))
  
  if (nrow(unique_indices) == 1){
    # if there is only one unique result, return it
    result$indices <- unique_indices[1,]
  } else {
    # if there are multiple unique results, select the one with the largest volume
    volumes <- apply(unique_indices, 1, function(indices) simplex_volume(data, indices, factorial=FALSE))
    result$indices <- unique_indices[which.max(volumes),]
  }

  # Add counts for debugging
  if (.options("debuglevel") > 0L) {
    result[["iterations_count"]] <- sapply(results_list, function(r) r$iterations_count)
    result[["replacements_count"]] <- sapply(results_list, function(r) r$replacements_count)
  }

  # Add replacements for debugging
  if (.options("debuglevel") > 1L) {
    result[["replacements"]] <- lapply(results_list, function(r) r$replacements)
  }

  class(result) <- c("nfindr", "pure_endmembers")
  
  return(result)
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
  # options give the same result, but replacement of points is done
  # in different order.
  # NOTE: The order of points matters
  set.seed(923)
  vertices <- rbind(c(-5, 0), c(0, 4), c(10, 0))
  initial_points <- rbind(c(0, 0), c(-1, 0), c(0, 1))
  data <- rbind(initial_points, vertices, .get_simplex_points(vertices))
  indices <- 1:3
  best_indices <- 4:6
  p <- length(indices)
  estimators <- eval(formals(nfindr)$estimator)
  estimators <- estimators[order(tolower(estimators))]
  expect_equal(estimators, c("cofactor", "Cramer", "height", "LDU", "volume"))

  test_that("nfindr output has pure_endmembers class", {
    result <- nfindr(data, p, init = indices)
    expect_s3_class(result, "nfindr")
    expect_s3_class(result, "pure_endmembers")
  })

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
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        })
        expect_equal(sort(result$indices), best_indices)
        expect_equal(names(result), c("indices"))
        
        unmixR.options(debuglevel = 1L)
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        })
        expect_equal(result$iterations_count, 2)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count"))
        
        unmixR.options(debuglevel = 2L)
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "endmembers", estimator = estimator)
        })
        expect_equal(
          result$replacements[[1]],
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
          result$replacements[[1]],
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
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        })
        expect_equal(sort(result$indices), best_indices)
        expect_equal(names(result), c("indices"))
        
        unmixR.options(debuglevel = 1L)
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        })
        expect_equal(result$iterations_count, 4)
        expect_equal(result$replacements_count, 3)
        expect_equal(names(result), c("indices", "iterations_count", "replacements_count"))
        
        unmixR.options(debuglevel = 2L)
        suppressWarnings({
          result <- nfindr(data, p, indices, iter = "both", estimator = estimator)
        })
        expect_equal(
          result$replacements[[1]],
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
    n_points = 200
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
        {
          if ((estimator == "height") && (iterator %in% c("endmembers", "both"))) {
            expect_warning(
              result <- nfindr(data, p, indices, iter=iterator, estimator = estimator),
              "This combination of iterator and volume change estimator is not optimized"
            )
          } else {
            result <- nfindr(data, p, indices, iter=iterator, estimator = estimator)
          }
          
          expect_equal(
            result,
            nfindr(data, p, indices, iter=iterator, estimator = "volume")
          )
        }
      )
    }
  }

  ## Test n_init ----
  unmixR.options(debuglevel = 2L)
  for (n in c(1, 2, 5)) {
    test_that(paste("n_init =", n), {
      set.seed(123)
      result <- nfindr(data, p, init="random", n_init = n)
      expect_equal(length(result$indices), p)
      expect_equal(length(result$iterations_count), n)
      expect_equal(length(result$replacements_count), n)
      expect_equal(length(result$replacements), n)
    })
  }

  ## Test init ----
  unmixR.options(debuglevel = 2L)

  # Test string-based initialization methods
  test_that("String-based initialization methods", {
    set.seed(123)
    
    # Test projections initialization
    result_proj <- nfindr(data, p, init="projections")
    expect_equal(length(result_proj$indices), p)
    expect_true(all(result_proj$indices %in% 1:nrow(data)))
    
    # Test coordinates_sequence initialization
    result_coords_seq <- nfindr(data, p, init="coordinates_sequence")
    expect_equal(
      result_coords_seq$replacements[[1]][1,],
      extreme_coordinates(data, p, random=FALSE)$indices
    )
    
    # Test coordinates_random initialization
    set.seed(1234)
    result_coords_rand <- nfindr(data, p, init="coordinates_random")
    expect_equal(
      result_coords_rand$replacements[[1]][1,],
      {set.seed(1234); extreme_coordinates(data, p, random=TRUE)$indices}
    )
    
    # Test random initialization
    set.seed(1234)
    result_random <- nfindr(data, p, init="random")
    expect_equal(
      result_random$replacements[[1]][1,],
      {set.seed(1234); sample(nrow(data), p)}
    )
  })
  
  # Test numeric vector initialization
  test_that("Numeric vector initialization", {
    manual_indices <- c(1, 5, 10, 15)
    result <- nfindr(data, p, init=manual_indices)
    expect_equal(result$replacements[[1]][1,], manual_indices)
  })
  
  # Test list initialization
  test_that("List initialization", {
    init_list <- list(
      c(1, 5, 10, 15),
      c(2, 8, 12, 18),
      c(3, 7, 11, 16)
    )
    expect_warning(
      result <- nfindr(data, p, init=init_list),
      "n_init.*is ignored"
    )
    expect_equal(length(result$replacements), length(init_list))
    expect_true(
      all(
        sapply(1:length(init_list), function(i) result$replacements[[i]][1,] == init_list[[i]])
      )
    )
  })
  
  # Test function initialization
  test_that("Function initialization", {
    # Test function that returns numeric indices
    custom_init_numeric <- function(data, p) {
      return(sample(1:nrow(data), p))
    }
    
    set.seed(456)
    result_numeric <- nfindr(data, p, init=custom_init_numeric)
    expect_equal(length(result_numeric$indices), p)
    expect_true(all(result_numeric$indices %in% 1:nrow(data)))
    
    # Test function that returns list with indices element
    custom_init_list <- function(data, p) {
      indices <- sample(1:nrow(data), p)
      return(list(indices = indices))
    }
    
    set.seed(789)
    result_list <- nfindr(data, p, init=custom_init_list)
    expect_equal(length(result_list$indices), p)
    expect_true(all(result_list$indices %in% 1:nrow(data)))
  })
  
  # Test error conditions for initialization
  test_that("Initialization error conditions", {
    # Test invalid list elements
    invalid_list <- list(c(1, 2), c(3, 4, 5))  # Different lengths
    expect_error(nfindr(data, p, init=invalid_list, n_init=2))
    
    # Test function that returns invalid output
    invalid_function <- function(data, p) {
      return("invalid")
    }
    expect_error(nfindr(data, p, init=invalid_function))
    
    # Test invalid string
    expect_error(nfindr(data, p, init="invalid_method"))
  })
  
  # Test that n_init warnings are properly issued
  test_that("n_init warnings", {
    # Test with numeric vector
    expect_warning(
      nfindr(data, p, init=c(1, 5, 10, 15), n_init=3),
      "n_init.*is ignored"
    )
    
    # Test with list
    expect_warning(
      nfindr(data, p, init=list(c(1, 5, 10, 15)), n_init=3),
      "n_init.*is ignored"
    )
  })
  
  # Test reproducibility with seed
  test_that("Reproducibility with seed", {
    set.seed(999)
    result1 <- nfindr(data, p, init="random")
    
    set.seed(999)
    result2 <- nfindr(data, p, init="random")
    
    expect_equal(result1$indices, result2$indices)
  })
  

  ## Test other (hyperSpec) objects ----
  test_that ("hyperSpec object", {
    pca <- prcomp(laser$spc)
    output <- nfindr (pca$x[,1,drop=FALSE], 2)
    expect_equal (output$indices, .correct.laser)
  })
}
