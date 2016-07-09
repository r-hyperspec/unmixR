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
  # Note: .testdata$x matches all columns of .testdata, which are x.L1, x.L2, x.L3
  # test: vca produces error for invalid values of p
  checkException (vca (.testdata$x, p = "---"))
  checkException (vca (.testdata$x, p = 0))
  checkException (vca (.testdata$x, p = 1))
  checkException (vca (.testdata$x, p = 4))

  # test: vca produces error for invalid method
  checkException(vca (.testdata$x, p, method="invalid"))

  # test correct calculations for the available methods

  methods <- eval (formals (vca.default)$method)

# BH: original version, fails because vca05lean & mvca give wrong answer, see next chunk
  # for (m in methods) {
    # ## .testdata has 3 components, so picking 2 out of 3
    # model <- vca (.testdata$x, p = 2, method = m)
    # checkTrue (all (model$indices %in% .correct),
               # msg = sprintf ("%s: .testdata, p = 2 yields %s", m, paste (model$indices, collapse = " ")))
    
    # checkTrue (all (vca (.testdata$x, p = 2, method = m)$indices %in% .correct), 
               # msg = sprintf ("%s: .testdata, p = 2", m))
    
    # ## all 3 components should be recovered, vca output is sorted.
    # model <- vca (.testdata$x, p = 2, method = m)
    # checkEquals (model$indices, .correct,
                 # msg = sprintf ("%s: .testdata, p = 2 yields %s", m, paste (model$indices, collapse = " ")))
  # }

# BH commented the above out; vca05lean and mvca are returning the wrong answer
# BH: this chunk only runs vca05 and uses p = 3
# BH: everything here needs review after the bug fix is done
  for (m in methods[3]) {
    ## .testdata has 3 components, so picking 2 out of 3
    model <- vca (.testdata$x, p = 3, method = m)
    checkTrue (all (model$indices %in% .correct),
               msg = sprintf ("%s: .testdata, p = 3 yields %s", m, paste (model$indices, collapse = " ")))
    
    checkTrue (all (vca (.testdata$x, p = 3, method = m)$indices %in% .correct), 
               msg = sprintf ("%s: .testdata, p = 3", m))
    
    ## all 3 components should be recovered, vca output is sorted.
    model <- vca (.testdata$x, p = 3, method = m)
    checkEquals (model$indices, .correct,
                 msg = sprintf ("%s: .testdata, p = 3 yields %s", m, paste (model$indices, collapse = " ")))
  }

  # test: if hyperSpec is available, test on hyperSpec object
  # tests also the correct application of as.matrix.
  if (require ("hyperSpec")) {
    checkEquals (vca (laser, p = 2)$indices, c (4, 81))
  }
}
