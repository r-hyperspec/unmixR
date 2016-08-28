##' Run the Unit Tests
##'
##' Run the unit tests for the package and output with the given testthat reporter.
##' 
##' @param reporter name of a testthat reporter. Defaults to \code{\link[testthat]{SummaryReporter}}.
##' 
##' @return Invisibly returns a data frame with the test results
##'
##' @author Claudia Beleites
##'
##' @seealso  \link[svUnit]{svUnit}
##' @keywords programming utilities
##' @export
##' @include unmixR-package.R
##' @importFrom testthat SummaryReporter ListReporter MultiReporter get_reporter
##' 

unmixR.unittest <- function () {

    if (!requireNamespace("testthat", quietly=TRUE)) {
    warning("testthat required to run the unit tests.")
    return(NA)
  }
  
  tests <- eapply(env = getNamespace ("unmixR"), FUN = get.test, all.names=TRUE)
  tests <- tests [! sapply (tests, is.null)]
  
  reporter <- SummaryReporter$new()
  lister <- ListReporter$new()
  reporter <- MultiReporter$new(reporters = list(reporter, lister))

  with_reporter(reporter = reporter, start_end_reporter = TRUE, {
    for (t in seq_along(tests)){
      lister$start_file(names (tests [t]))
      tests [[t]] ()
    }
    get_reporter()$.end_context()
  })


 invisible(lister$get_results())
}

##' test data for unit tests
##' @noRd
{
.C <- expand.grid ( 0 : 3, 0 : 3)
.C [, 3] <- 3 - rowSums (.C)
.C <- as.matrix (.C [.C [, 3] >= 0,])
dimnames(.C) <- list(samples = 1:10, wavelengths = paste("L", 1:3, sep = "")) # BH
.testdata <- data.frame (sample = paste("s", 1:10, sep = ""), x = I (.C))
rm (.C)
.correct <- c (1, 4, 10)

.correct.laser <- c (4, 79)
}

##' get test that is attached to object as "test" attribute
##' @noRd
get.test <- function (object)
  attr (object, "test")

