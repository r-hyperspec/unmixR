##' @name nfindr
##' @rdname nfindr
##' @include nfindr.R
##' @export
##' @importFrom stats prcomp

nfindr.default <- function(data, p,
                           method = "99", indices = sample(nrow(data), p), ...,
                           EMonly = FALSE) {

  ## get the selected nfindr method
  nfindrFunc <- get0 (paste0 ("nfindr", method), mode = "function")

  # check if the method passed in was found
  if (is.null (nfindrFunc)) {
    stop ('Invalid option for method parameter (', method ,') try: ', 
          paste (get.implementations ("nfindr"), collapse = ", "))
  }

  # check for p being with the valid range, >= 2
  p <- as.integer(p)
  if (p < 2) stop("p must be a positive integer >= 2")

  # check that p and length(indices) are the same
  if (p != length(indices)) stop("length(indices) must equal p")

  # ensure we are dealing with a matrix
  data <- as.matrix (data)

  # keep original data for possible return
  orig <- data

  # reduce the dimensionality of the data using PCA
  # do nothing if the data was passed in already reduced
  if (ncol(data) != p - 1) {
    data <- stats::prcomp(data)[["x"]][, sequence(p-1), drop=FALSE]
  }

  # call the function to get the indices of the endmembers
  # at this point data has dimensions n samples x (p-1) abstract wavelengths
  indices <- nfindrFunc(data, p, indices, ...)
  
  # sort the indices to normalise the order between runs
  indices <- sort (indices) # redundant: these are also sorted in the individual implementations

  res <- list(data = if (!EMonly) orig else orig[indices,],
              indices = indices)
  class(res) <- "nfindr"
  return(res)
}


# Note: nfindr.formula has its own tests
# There is also a test in nfinderLDU

.test(nfindr.default) <- function() {
  context ("N-FINDR")
  
  # Misc. front end tests
  
  expect_true(require (hyperSpec))
  
  test_that ("Exceptions for invalid values of p", {
    # invalid p
    expect_error (nfindr(.triangle$x, p="---"))
    expect_error (nfindr(.triangle$x, p = 0))  
  })

  test_that ("Exceptions for invalid method", {
    expect_error (nfindr(.triangle$x, p, method="invalid"))
  })

  implementations <- get.implementations("nfindr")
  
  test_that ("All implementations available", {
    expect_true (all (c ("99", "Brute", "LDU", "SeqLDU") %in% implementations))
  })

  # Check correct answers for the available methods

  test_that ("Correct endmembers by nfindr default method for laser", {
    expect_equal (nfindr(laser, p = 2)$indices, .correct.laser)
  })

  test_that ("Correct endmembers by nfindr default method for triangle data", {
    expect_equal (nfindr(.triangle$x, p = 3)$indices, .correct.triangle)
  })
  
  test_that ("All nfindr implementations return correct (same) results for laser data", {
    for (i in implementations)
      expect_equal(nfindr (laser, method = i, p = 2)$indices, .correct.laser)
  })

  test_that("Correct results for all available nfindr methods: demo_data", {
    data(demo_data, envir = environment())
    for (i in implementations) {
      expect_equal (nfindr (demo_data, p = 2, method = i)$indices, .correct.demo_data)
    }
  })
  
  test_that ("All nfindr implementations return correct (same) results for triangle data", {
    for (i in implementations) {
      
      if (i == "LDU")
        skip ("LDU skipped: known issue #38")
      
      expect_equal(nfindr (.triangle$x, method = i, p = 3)$indices, .correct.triangle,
                   info = i)
    }
  })
  
  # Misc. back end tests
 
  test_that ("Return values for laser match the hyperSpec object", {
    output <- nfindr (laser, 2)
    expect_equal (output$data, laser@data$spc)
    expect_equal (output$indices, .correct.laser)
  })
}
