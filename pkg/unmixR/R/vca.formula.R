##' @name vca
##' @rdname vca
##' @export
##' @include unmixR-package.R
##' @include vca.default.R
##' @importFrom stats terms

vca.formula <- function(formula, frame, p, method = "05lean", seed=NULL, ...) {

  mt <- stats::terms(formula, data = frame)

  ## response term is not allowed
  if (attr(mt, "response") > 0L) stop("VCA models cannot have response")

  ## drop intercept silently
  attr(mt, "intercept") <- 0L

  data <- model.matrix (mt, frame)

  vca(data, p, method, seed, ...)
}

.test (vca.formula) <- function (){
  context ("vca.formula")
  
  test_that("error on response term in formula", {
    expect_error(vca (x ~ ., data.frame (x = matrix (1:4, 2)), p = 2))
  })

  test_that ("same results with formula interface", {
    expect_equal(vca (~ x, .testdata, p = 3)$indices,
                 vca (.testdata$x, p = 3)$indices)
  })  

  test_that ("check conversion of classes", {
    if (require ("hyperSpec")) {
      expect_equal (vca(~ spc, laser, p = 2)$indices, c (4, 81))
    }
  })

}
