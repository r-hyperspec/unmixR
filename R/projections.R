#' Find Endmember Candidates Using Extreme Coordinates
#'
#' Selects endmember candidates by identifying extreme points along coordinate axes.
#' This function implements a deterministic approach to endmember initialization by finding
#' the minimum and maximum values along each coordinate axis. If insufficient unique extreme
#' points are found, it supplements with random projections.
#'
#' @param data A numeric matrix where each row represents a data point and each column
#'   represents a coordinate/dimension.
#' @param p Number of endmembers to select.
#' @param random A logical value indicating the selection strategy. If \code{TRUE}, selects
#'   randomly \code{p} points from all dimensions simultaneously. If \code{FALSE} (default), 
#'   iteratively selects extreme points dimension by dimension.
#'
#' @return A list containing:
#'   \item{indices}{An integer vector of length \code{p} containing the row indices of
#'     the selected endmember candidates from the input data matrix.}
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' data("demo_data")
#' 
#' # Select 3 endmembers using deterministic approach
#' result1 <- extreme_coordinates(demo_data, p=3, random=FALSE)
#' print(result1$indices)
#' 
#' # Select 3 endmembers using random approach
#' result2 <- extreme_coordinates(demo_data, p=3, random=TRUE)
#' print(result2$indices)
#'
#' @seealso \code{\link{random_projections}}
#'
#' @export
extreme_coordinates <- function(data, p, random=FALSE) {
  # Check for p being with the valid range, >= 1
  if (!is.numeric(p) || p < 1) {
    stop("p must be a positive integer >= 1")
  }

  if (random) {
    indices <- apply(data, 2, FUN=function(x) c(which.min(x), which.max(x)))
    indices <- unique(as.vector(indices))
    indices <- sample(indices, min(p, length(indices)), replace = FALSE)
  } else {
    indices <- c()
    i <- 1
    while((length(indices)<p) && (i<=ncol(data))) {
      new_indices <- c(which.min(data[,i]), which.max(data[,i]))
      indices <- unique(c(indices, new_indices))
      i <- i+1
    }
  }
  
  if (length(indices) < p) {
    warning(paste0(
      "NA values in output. Not enough extreme coordinates were found.
      Probably dimension of the data is too small, or p is too high."
    ))
  }
  
  return(list("indices"=indices[1:p]))
}

#' Find Endmember Candidates Using Random Projections
#'
#' Selects endmember candidates by projecting the data onto randomly generated
#' vectors and identifying extreme points in the projected space.
#'
#' @param data A numeric matrix where each row represents a data point and each column
#'   represents a coordinate/dimension.
#' @param p Number of endmembers to select.
#' @param iter_max Maximum number of projections to try, to avoid edge cases where
#'   all (or many) projections result in same extreme points.
#'
#' @return A list containing:
#'   \item{indices}{An integer vector of length \code{p} containing the row indices of
#'     the selected endmember candidates from the input data matrix.}
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' data("demo_data")
#' 
#' # Select 4 endmembers using random projections
#' result <- random_projections(demo_data, p=4)
#' print(result$indices)
#'
#' @seealso \code{\link{extreme_coordinates}}
#'
#' @export
random_projections <- function(data, p, iter_max = 2*p) {
  # Check for p being with the valid range, >= 1
  if (!is.numeric(p) || p < 1) {
    stop("p must be a positive integer >= 1")
  }
  
  indices <- c()
  m <- ncol(data)
  i <- 1
  while ( (length(indices)<p) && (i <= iter_max) ) {
    w <- stats::rnorm(m, sd = 1)
    projections <- as.vector(data %*% w)
    new_indices <- c(which.max(projections), which.min(projections))
    indices <- unique(c(indices, new_indices))
    i <- i + 1
  }
  
  if (length(indices) < p) {
    warning(paste0(
      "NA values in output. Not enough extreme points were found.
      Probably dimension of the data is too small, or p is too high."
    ))
  }
  
  return(list("indices"=indices[1:p]))
}

# Add tests for extreme_coordinates function
.test(extreme_coordinates) <- function() {
  context("extreme_coordinates")
  
  # Create test data
  set.seed(123)
  test_data <- matrix(c(
     1,  1,  1,
     1,  0,  1,
    -3,  0,  1,
     4,  0, -4,
     1, -5,  5,
     1,  0,  1
  ), nrow = 6, ncol = 3, byrow = TRUE)
  
  test_that("extreme_coordinates produces error for invalid values of p", {
    expect_error(extreme_coordinates(test_data, p = "invalid"))
    expect_error(extreme_coordinates(test_data, p = 0))
    expect_error(extreme_coordinates(test_data, p = -1))
  })
  
  test_that("extreme_coordinates returns correct structure", {
    result <- extreme_coordinates(test_data, p = 3)
    expect_is(result, "list")
    expect_true("indices" %in% names(result))
    expect_equal(length(result$indices), 3)
    expect_true(all(result$indices %in% 1:nrow(test_data)))
    expect_equal(length(unique(result$indices)), length(result$indices))
  })
  
  test_that("extreme_coordinates deterministic mode works correctly", {
    result <- extreme_coordinates(test_data, p = 3, random = FALSE)
    # Should find extreme points along each coordinate axis
    expect_equal(result$indices, c(3,4,5))
  })
  
  test_that("extreme_coordinates random mode works correctly", {
    all_results <- c()
    for(i in 1:10) {
      result <- extreme_coordinates(test_data, p = 3, random = TRUE)
      expect_equal(length(result$indices), 3)
      expect_true(all(result$indices %in% c(1,3,4,5)))
      expect_equal(length(unique(result$indices)), length(result$indices))
      all_results <- c(all_results, result$indices)
    }
    
    # Check that all extreme indices appeared
    expect_equal(sort(unique(all_results)), c(1,3,4,5))
  })
  
  test_that("extreme_coordinates handles edge cases", {
    # Test with p = 1 (minimum valid value)
    result <- extreme_coordinates(test_data, p = 1)
    expect_equal(result$indices, c(3))
    
    # Test with single column data
    single_col_data <- matrix(c(1, 5, 3, 2, 4), nrow = 5, ncol = 1)
    result <- extreme_coordinates(single_col_data, p = 2)
    expect_equal(result$indices, c(1,2))
    # Should include indices of min and max values
    expect_warning(
      result <- extreme_coordinates(single_col_data, p = 3),
      "NA values in output. Not enough extreme coordinates were found."
    )
    expect_equal(result$indices, c(1,2,NA))

    # Create data where extreme coordinates won't be enough
    small_data <- matrix(c(
      1,   2,  5,
      0, 1.5,  4,
      0,   1,  3,
      0,   0,  2,
      0,  -1,  1,
     -1,  -2,  0
    ), nrow = 6, ncol = 3, byrow = TRUE)
    
    # Expect warning when requesting more endmembers than available extreme points
    expect_warning(
      result <- extreme_coordinates(small_data, p = 4, random = FALSE),
      "NA values in output. Not enough extreme coordinates were found."
    )
    expect_equal(result$indices, c(6,1,NA,NA))
  })
}

# Add tests for random_projections function
.test(random_projections) <- function() {
  context("random_projections")

  # Create test data
  set.seed(456)
  test_data <- matrix(rnorm(50), nrow = 10, ncol = 5)

  test_that("random_projections produces error for invalid values of p", {
    expect_error(random_projections(test_data, p = "invalid"))
    expect_error(random_projections(test_data, p = 0))
    expect_error(random_projections(test_data, p = -1))
  })

  test_that("random_projections returns correct structure", {
    result <- random_projections(test_data, p = 4)
    expect_is(result, "list")
    expect_true("indices" %in% names(result))
    expect_equal(length(result$indices), 4)
    expect_true(all(result$indices %in% 1:nrow(test_data)))
    expect_equal(length(unique(result$indices)), 4)
  })

  test_that("random_projections handles edge cases", {
    # Test with p = 1 (minimum valid value)
    result <- random_projections(test_data, p = 1)
    expect_equal(length(result$indices), 1)

    # Test with single row data (should still work)
    single_row_data <- matrix(c(1, 2, 3, 4, 5), nrow = 1, ncol = 5)
    expect_warning(
      result <- random_projections(single_row_data, p = 2),
      "NA values in output. Not enough extreme points were found."
    )
    expect_equal(result$indices, c(1, NA))
  })

  test_that("random_projections algorithm terminates correctly", {
    # Test that the algorithm doesn't run indefinitely
    # Even with challenging data, it should terminate
    challenging_data <- matrix(rep(1, 20), nrow = 4, ncol = 5)
    expect_warning(
      result <- random_projections(challenging_data, p = 3),
      "NA values in output. Not enough extreme points were found."
    )
    expect_equal(result$indices, c(1,NA,NA))
  })
}
