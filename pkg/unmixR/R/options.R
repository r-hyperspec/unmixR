##' unmixR package options
##'
##' \pkg{unmixR} uses \pkg{settings} for option management.
##'
##' The following options are defined for \pkg{unmixR}:
##'
##' \describe{
##'   \item{debuglevel}{Indicates how much debuging output is to be produced.  Default: 0.
##'   A value of 1 reports on the overall progress of identifying the endmembers.
##'   Values > 1 give additional details about the internal processing.}
##'
##'  \item{implementation.search}{(default: \code{"package:unmixR"}) 
##'  Environments in which to search for unmixing algorithm implementations.
##'  Provides a means of integrating implementations from other packages with a 
##'  compatible return value. The global environment can be added as \code{".GlobalEnv"}}
##' }
##'
##'
##' @param ... either \code{key = value} pairs to set options or the names of
##'   the options to retrieve. If no parameters are passed, a list of all
##'   options is returned.
##'
##' @return Either a list of current options or the value of a requested option.
##'
##' @examples
##' unmixR.options () # show all options
##' unmixR.options ("debuglevel") # show just this one option
##' unmixR.options (debuglevel = 0L) # set an option
##"
##' @importFrom settings options_manager stop_if_reserved
##' @export
##' @rdname options

unmixR.options <- function (...) {
  settings::stop_if_reserved (...)
  .options (...)
}

.options <- settings::options_manager (
  debuglevel = 0L,
  implementation.search = "package:unmixR"
  )

.test (unmixR.options) <- function (){

  context ("options")
  
  ## check list of defined options against (manually kept) list of documented
  ## options.
  test_that("manual check of option list",{
    expect_equal(sort (names (.options ())),
                 c("debuglevel", "implementation.search"))
  })
}

