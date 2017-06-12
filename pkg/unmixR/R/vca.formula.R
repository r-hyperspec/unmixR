##' @name vca
##' @rdname vca
##' @export
##' @include unmixR-package.R
##' @include vca.default.R
##' @importFrom stats terms model.matrix

vca.formula <- function(formula, frame, p, method = "05", seed = NULL, ...) {

  mt <- terms(formula, data = frame)

  ## response term is not allowed
  if (attr(mt, "response") > 0L) stop("VCA models cannot have response")

  ## drop intercept silently
  attr(mt, "intercept") <- 0L

  data <- model.matrix (mt, frame)

  vca(data, p, method, seed, ...)
}

.test (vca.formula) <- function (){
  context ("vca.formula")
  expect_true (require (hyperSpec))
  
  test_that("error on response term in formula", {
    expect_error(vca (x ~ ., data.frame (x = matrix (1:4, 2)), p = 2))
  })

  test_that ("same results with formula interface", {
    expect_equal(vca (~ x, .triangle, p = 3)$indices,
                 vca (.triangle$x, p = 3)$indices)
  })  

  test_that ("check conversion of classes", {
      expect_equal (vca (~ spc, laser, p = 2, seed = 12345)$indices, 
                    vca (~ spc, laser$., p = 2, seed = 12345)$indices)
  })

  test_that ("Formula Interface does not produce offset term", {
    expect_false("(Intercept)" %in% colnames (vca (~ x, .triangle, p = 3)$data))
  })
  
}
