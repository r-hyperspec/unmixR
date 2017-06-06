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
  implementations <- get.implementations("vca")
  

  # test correct calculations for the available methods
  implementations <- get.implementations("vca")
  
  test_that("correct results for all available methods: triangle data", {
    for (i in implementations) {
      expect_equal (vca (.testdata$x, p = 3, method = i)$indices, .correct)
      
      indices <- vca (.testdata$x, p = 2, method = i)$indices
      expect_true (all (indices %in% .correct), info = i)
      
      if (i == "Lopez2012") skip ("temporarily disabled: known issue #36")
      expect_false (any (duplicated (indices)), info = i)
    }
  })

  test_that ("no duplicates with Lopez2012 for test data", {
    skip ("known issue: #36")
    
    indices <- replicate (10, vca (.testdata$x, p = 2, method = "Lopez2012")$indices)
    expect_true (all (indices %in% .correct))            
    expect_true (all (indices [1, ] != indices [2, ]), info = "Lopez2012 duplicate indices: testdata, p = 2")
  }
  )
  
  test_that("correct results for all available methods: laser data", {
    skip ("temporarily disabled")
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
      expect_equal (vca (~ spc, laser, p = 2, seed = 12345)$indices, 
                    vca (~ spc, laser$., p = 2, seed = 12345)$indices)
  })
}
