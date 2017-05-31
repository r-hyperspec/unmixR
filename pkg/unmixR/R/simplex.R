##' Simplex Generator
##' 
##' Simple helper function for generating initial simplex for N-FINDR
##' methods using indices that point to rows in a data matrix
##' 
##' @param data Matrix whose rows will be included in the simplex. This
##'   matrix should already be reduced using using PCA or some other process
##'   so that it has p-1 columns before calling this function.
##'
##' @param indices Locations of the rows in the dataset to use in the simplex
##'
##' @return The simplex, a p x p matrix whose first row contains only 1s
##' 
##' @include unmixR-package.R
##' @noRd

.simplex <- function(data, indices) {

  if (ncol (data) != length (indices) - 1L)
    stop ("length(indices) -1 != ncol(data)")
  
  data <- data [indices, , drop = FALSE]
  rbind (rep (1, length(indices)), t (data)) # See Winter1999 Eqn (3) - needed for volume computation
}

.test(.simplex) <- function() {
  
  context ("simplex")
  
  test_that("simplex exceptions", {
    expect_error (.simplex (.testdata$x, indices = 1 : 3))
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
    expect_equal (expected, .simplex (data, indices))
  }) 
  
}
