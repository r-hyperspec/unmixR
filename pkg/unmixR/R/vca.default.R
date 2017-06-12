##' @name vca
##' @rdname vca
##' @include vca.R
##' @export

vca.default <- function(data, p, method = "05",
                        seed = NULL, SNR = estSNR(data, p), ...,
                        EMonly = FALSE) {

  ## get the selected vca method
  vcaFunc <- get0(paste0 ("vca", method), mode = "function")

  # check if the method passed in was found
  if (is.null (vcaFunc)) {
    stop ('Invalid option for method parameter (', method ,') try: ', 
          paste (get.implementations ("vca"), collapse = ", "))
  }

  # check for p being with the valid range, >= 2
  if (!is.numeric (p) || p < 2 || p > ncol (data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }

  # ensure we are dealing with a matrix
  data <- as.matrix (data)
  

  # set the random number generator seed if supplied
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  force(SNR)
  reducedData <- dimensionalityReduction(data, p, SNR)
  
  indices <- vcaFunc(reducedData, p, ...)

	# BH: why do we have as.integer here?
	
  if (.options ("debuglevel") >= 1L){
      res <- list(data = if (!EMonly) data else data[as.integer(indices),],
                  indices = if (!EMonly) as.integer(indices) else 1:p,
                  seed = seed)
  } else {
      res <- list(data = if (!EMonly) data else data[as.integer(indices),],
                  indices = as.integer(indices))
  }
  class(res) = "vca"
  return(res)

}


.test(vca.default) <- function() {
  context ("vca")
  
  # Note: .triangle$x matches all columns of .triangle, which are x.L1, x.L2, x.L3
  
  # Misc. front end tests
  
  test_that ("vca produces error for invalid values of p", {
    expect_error (vca (.triangle$x, p = "---"))
    expect_error (vca (.triangle$x, p = 0))
    expect_error (vca (.triangle$x, p = 1))
    expect_error (vca (.triangle$x, p = 4))
  })

  test_that ("vca produces error for invalid method", {
    expect_error (vca (.triangle$x, p, method="invalid"))
  })
  
  implementations <- get.implementations("vca")

  test_that("Built-in vca implementations are in fact available", {
      expect_true (all (c("05", "Lopez2012") %in% implementations))
  })
  
  test_that("Correct results for all available vca methods: triangle data", {
    for (i in implementations) {
      expect_equal (vca (.triangle$x, p = 3, method = i)$indices, .correct.triangle)
      
      indices <- vca (.triangle$x, p = 2, method = i)$indices
      expect_true (all (indices %in% .correct.triangle), info = i)
      
      if (i == "Lopez2012") skip ("Temporarily disabled: known issue #36")
      expect_false (any (duplicated (indices)), info = i)
    }
  })

  test_that("Correct results for all available vca methods: laser data", {
    skip ("Disabled: returns 4, 84 rather than 4, 79, see issue #20")
    for (i in implementations) {
      expect_equal (vca (laser$spc, p = 2, method = i)$indices, .correct.laser)
    }
  })

  test_that("Correct results for all available vca methods: demo_data", {
    data(demo_data, envir = environment())
    for (i in implementations) {
      expect_equal (vca (demo_data, p = 2, method = i)$indices, c(3,7))
    }
  })

  # Misc. back end tests
  
  test_that ("No duplicates with Lopez2012 for test data", {
  	
    skip ("Known issue: #36")
    
    indices <- replicate (10, vca (.triangle$x, p = 2, method = "Lopez2012")$indices)
    expect_true (all (indices %in% .correct))            
    expect_true (all (indices [1, ] != indices [2, ]), info = "Lopez2012 duplicate indices: testdata, p = 2")
  })
  

  test_that("vca output is sorted", {
    indices <- vca (.triangle$x, p = 3)$indices
    expect_equal(indices, sort (indices))
  })

  # If hyperSpec is available, test on hyperSpec object; also tests the correct application of as.matrix.
  
  test_that("Check conversion of classes", {
      expect_equal (vca (~ spc, laser, p = 2, seed = 12345)$indices, 
                    vca (~ spc, laser$., p = 2, seed = 12345)$indices)
  })
}
