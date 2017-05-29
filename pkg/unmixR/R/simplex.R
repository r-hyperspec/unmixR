##' Simplex Generator
##' 
##' Simple helper function for generating initial simplex for N-FINDR
##' methods using indices that point to rows in a data matrix
##' 
##' @param data Matrix whose rows will be included in the simplex. This
##'   matrix should already be reduced using using PCA or some other process
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
##' @noRd

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
  rbind (rep (1, p), t (data)) # See Winter1999 Eqn (3) - needed for volume computation
}

.test(.simplex) <- function() {
  
  context ("simplex")
  
  test_that("simplex exceptions", {
    expect_error (.simplex (.testdata$x, 2, indices = 1 : 3))
    expect_error (.simplex (.testdata$x [, rep (1, 3)], 3))
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
