##' @name nfindr
##' @rdname nfindr
##' @export
##' @include nfindr.default.R
##' @importFrom stats model.matrix terms
##'
nfindr.formula <- function(formula, frame, ...) {
  mt <- terms(formula, data = frame)
  
  ## response term is not allowed
  if (attr(mt, "response") > 0L) stop("N-FINDR models cannot have response")
  
  ## drop intercept silently
  attr(mt, "intercept") <- 0L
  
  data <- model.matrix (mt, frame)

  nfindr (data, ...)
}

.test (nfindr.formula) <- function () {
  context ("nfindr.formula")
  
  expect_true (require(hyperSpec))
  
  test_that("error on response term in formula", {
    expect_error(nfindr (x ~ ., .testdata, p = 2))
  })
  
  test_that ("same results with formula interface", {
    expect_equal(nfindr (~ x, .testdata, p = 3)$indices,
                 nfindr (.testdata$x, p = 3)$indices)
  })  
  
  test_that ("check conversion of classes", {
    expect_equal (nfindr (~ spc, laser, p = 2)$indices, .correct.laser)
  })
  
  test_that ("Formula Interface", {
    expect_equal(nfindr (~ x, .testdata, p = 3)$indices, correct)  
    expect_equal(nfindr (~ ., as.data.frame (.testdata$x), p = 3)$indices, correct)  
  })
  
  test_that ("Formula Interface does not produce offset term", {
    expect_false("(Intercept)" %in% colnames (nfindr (~ x, .testdata, p = 3)$data))
  })
}
