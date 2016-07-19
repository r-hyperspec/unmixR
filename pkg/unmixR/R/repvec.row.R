##' Repeat a Vector Row-Wise
##' 
##' Takes in a vector and repeats it column-wise to make a matrix with
##' \code{n} rows
##' 
##' @param v The vector that should be repeated
##' @param n The number of rows in the resulting matrix
##' @return A matrix with \code{n} rows where each row is \code{v}
##' 
##' @include unmixR-package.R
##' @rdname repvec

.repvec.row <- function(v, n) t(as.matrix(v))[rep(1, n), ]

.test(.repvec.row) <- function() {
  
  context ("row-wise repeat vector")
  
  v <- c(1, 2, 3)
  n <- 3
  
  expected <- matrix(c(
    c(1,2,3),
    c(1,2,3),
    c(1,2,3)
  ), ncol=3, byrow=TRUE)
  
  test_that ("repeat rows", {
    expect_equal(.repvec.row (v, n), expected)
  })
}
