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
