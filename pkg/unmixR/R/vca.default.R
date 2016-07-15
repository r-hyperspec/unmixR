##' @name vca
##' @rdname vca
##' @include vca.R
##' @export

vca.default <- function(data, p, method = c("05lean", "mvca", "05"), seed = NULL, ...) {

  # check if the method passed in is valid
  method <- match.arg (method)

  # transform the input into a matrix
  data <- as.matrix (data)

  # check for p being with the valid range, >= 2
  if (!is.numeric (p) || p < 2 || p > ncol (data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }

  # set the random number generator seed if supplied
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (method == "mvca") vcaFunc <- mvca
  if (method != "mvca") vcaFunc <- get(paste("vca", method, sep=""), mode = "function")
  val <- vcaFunc(data, p, ...)

  res <- list(data = data, indices = as.integer(val))
  class(res) = "vca"
  return(res)

}


.test(vca.default) <- function() {
  context ("vca")
  
  # Note: .testdata$x matches all columns of .testdata, which are x.L1, x.L2, x.L3
  
  test_that ("vca produces error for invalid values of p", {
    expect_error (vca (.testdata$x, p = "---"))
    expect_error (vca (.testdata$x, p = 0))
    expect_error (vca (.testdata$x, p = 1))
    expect_error (vca (.testdata$x, p = 4))
  })

  test_that ("vca produces error for invalid method", {
    expect_error (vca (.testdata$x, p, method="invalid"))
  })
  
  ## test that at least the implementations provided by unmixR are available
  # this fails at the moment (correctly!) because we need to rename mvca again!
  implementations <- get.implementations("vca")
  test_that ("Implementations available", {
    expect_true (all (c ("05", "05lean", "Modified") %in% implementations))
  })
  

  # test correct calculations for the available methods
  implementations <- get.implementations("vca")
  
  test_that("correct results for all available methods: triangle data", {
    for (i in implementations) {
      expect_equal (vca (.testdata$x, p = 3, method = i)$indices, .correct)
    }
  })

  test_that("correct results for all available methods: laser data", {
    for (i in implementations) {
      expect_equal (vca (laser$spc, p = 2, method = i)$indices, .correct.laser)
    }
  })
  
  ## all 3 components should be recovered, vca output is sorted.
  test_that("vca output is sorted", {
    indices <- vca (.testdata$x, p = 3)$indices
    expect_equal(indices, sort (indices))
  })

  # test: if hyperSpec is available, test on hyperSpec object
  # tests also the correct application of as.matrix.
  test_that("check conversion of classes", {
    expect_equal (vca (laser, p = 2)$indices, .correct.laser)
  })
}
