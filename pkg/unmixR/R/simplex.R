##' Simplex Generator
##' 
##' Simple helper function for generating initial simplex for N-FINDR
##' methods using indices that point to rows in a data matrix
##' 
##' @param data Matrix whose rows will be included in the simplex. This
##'   matrix should be reduced using using PCA or some other process
##'   so that it has p-1 columns before calling this function.
##'
##' @param p Number of endmembers that should be found using the simplex.
##'   Defines the size of the simplex (will be p x p)
##'
##' @param indices Locations of the rows in the dataset to use in the simplex
##'
##' @return The simplex, a p x p matrix whose first row contains only 1s
##' 
##' @include unmixR-package.R

## @rdname simplex

# The following is Bryan's modification so that the function does what
# it is claimed to do, return a p x p matrix
# This was not really necessary, but does not harm; I was forgetting that
# nfinder.default was doing the PCA before coming here.

## TODO: @CB check why p is needed at all: it should be clear from both length
## (indices) and ncol (data)
## TODO: @CB check wheter data columns 1 : (p - 1) is needed

.simplex <- function(data, p = length (indices), indices) {
  if (length (indices) != p) {
  	stop("p indices needed to form simplex")
  }

  if (ncol (data) != length (indices) - 1L)
    stop ("length (indices) does not correspond to dimensionality of data")
  
  data <- data [indices, , drop = FALSE]
  rbind (rep (1, p), t (data))
}

.test(.simplex) <- function() {
  
  context ("simplex")
  
  test_that("simplex exceptions", {
    expect_error (.simplex (data, 2, indices))
    expect_error (.simplex (data [, rep (1, 3)], 3, indices))
  })
  
  p <- 3
  rows <- 5
  indices <- c(1, 3, 5)
  
  data <- matrix (seq_len ((p - 1) * rows), 
                  ncol = p - 1, nrow = rows, byrow = TRUE)
  
  expected <- matrix(c (1, 1,  1,
                        1, 5,  9, 
                        2, 6, 10), ncol = p, byrow = TRUE)
 
  test_that("correct simplex", {
    expect_equal (expected, .simplex (data, p, indices))
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
simplex.volume <- function (data, indices = seq_len (nrow (data)), factorial = TRUE){
  simplex <- .simplex (data = data, indices = indices, p = length (indices))
  V = abs (det (simplex)) 
  
  if (factorial)
    V <- V / factorial (length (indices) - 1)
  
  V
}

.test (simplex.volume) <- function (){
  
  context ("simplex.volume")
  
  data <- svd (.testdata$x, nv = 0)
  
  ## uncentered svd of .testdata$xhas column 1 constant
  data <- data$u [c (1, 4, 10), -1] 
  
  ## data now has the axes aligned for direct calculation of triangle area
  area <- apply (data, 2, range)
  area <- apply (area, 2, diff)
  area <- 1/2 * prod (area)
  
  expect_that("correct volumes for triangle data", {
    expect_equal (simplex.volume (data, indices = 1 : 3, factorial = TRUE), area)
    expect_equal (simplex.volume (data, indices = 1 : 3, factorial = FALSE), area * 2)
    expect_equal (simplex.volume (data, factorial = TRUE), area)

    data <- svd (.testdata$x, nv = 0)$u[, -1] # see above
    expect_equal (simplex.volume (data, indices = c (1, 4, 10), factorial = TRUE), area)
  })
}
