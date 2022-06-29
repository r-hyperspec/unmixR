##' Generate a simplex volume matrix
##'
##' Simple helper function for generating a simplex volume matrix E
##' (i.e. volume of simplex = det(E)/p-1!) of the following structure:
##' |   1   1   ... 1 |
##' | e_1 e_2 ... e_p |
##' Where e_i is an i-th vertex point of the simplex.
##'
##' @param data Matrix whose rows will be included in the simplex. This
##'   matrix should be reduced using using PCA or some other process
##'   so that it has p-1 columns before calling this function.
##'
##' @param indices Locations of the rows in the dataset to use as simplex vertecies
##'
##' @return A simplex volume matrix E, a p x p matrix whose first row contains
##' only 1s
##'
##' @include unmixR-package.R
##' @rdname simplex
.simplex_E <- function(data, indices = 1:nrow(data)) {
  p <- length(indices)
  if (ncol(data) != p - 1L) {
    stop("length (indices) does not correspond to dimensionality of data")
  }

  data <- data[indices, , drop = FALSE]
  rbind(rep(1, p), t(data))
}

.test(.simplex_E) <- function() {
  context("simplex_E")

  test_that("simplex exceptions", {
    # p < n
    expect_error(.simplex_E(.testdata$x, 1:2))
    # p == n
    expect_error(.simplex_E(.testdata$x, 1:3))
    # p > n+1
    expect_error(.simplex_E(.testdata$x, 1:5))
  })

  p <- 3
  rows <- 5
  indices <- c(1, 3, 5)

  data <- matrix(seq_len((p - 1) * rows),
    ncol = p - 1, nrow = rows, byrow = TRUE
  )

  expected <- matrix(c(
    1, 1, 1,
    1, 5, 9,
    2, 6, 10
  ), ncol = p, byrow = TRUE)

  test_that("correct simplex", {
    expect_equal(expected, .simplex_E(data, indices))
    expect_equal(expected, .simplex_E(data[indices,]))
  })
}

#' Volume of simples
#'
#' @param data matrix with coordinates in rows
#' @param indices  indices of the  \code{ncol(data) + 1} vertex points.
#' Defaults to all rows.
#' @param factorial logical indicating whether proper volumes
#' (i.e. not only the determinant, but actually taking into account the
#' proper 1 / n! prefactor)
#'
#' @return volume of the simplex
#' @export
#'
#' @examples
#' data <- prcomp(laser)$x[, 1:2]
#' plot(data, pch = 19, col = matlab.dark.palette(nrow(data)))
#' lines(data[c(1, 84, 50, 1), ])
#'
#' simplex_volume(data, indices = c(1, 84, 50))
simplex_volume <- function(data, indices = seq_len(nrow(data)), factorial = TRUE) {
  E <- .simplex_E(data = data, indices = indices)
  V <- abs(det(E))

  if (factorial) {
    V <- V / factorial(length(indices) - 1)
  }

  V
}

.test(simplex_volume) <- function() {
  context("simplex_volume")

  triangle_2d <- rbind(
    c(0,0),
    c(1,0),
    c(0,1)
  )
  shifted_triangle_2d <- triangle_2d + matrix(c(1,3), nrow=3, ncol=2, byrow = TRUE)
  area_2d <- 0.5
  test_that("correct volumes for triangle data", {
    expect_equal(simplex_volume(triangle_2d, factorial = TRUE), area_2d)
    # Provide indices
    expect_equal(simplex_volume(triangle_2d, indices = 1:3, factorial = TRUE), area_2d)
    # Without factorial
    expect_equal(simplex_volume(triangle_2d, factorial = FALSE), 2*area_2d)
    # Moving the triangle should not change the area
    expect_equal(simplex_volume(shifted_triangle_2d, factorial = TRUE), area_2d)
  })

  zero_volume_triangle <- matrix(1, nrow = 3, ncol= 2)
  test_that("zero-volume triangle", {
    expect_equal(simplex_volume(zero_volume_triangle), 0)
    expect_equal(simplex_volume(zero_volume_triangle), 0)
  })
}
