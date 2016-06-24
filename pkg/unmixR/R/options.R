##' unmixR's package options
##'
##' \pkg{unmixR} uses \pkg{settings} for option management.
##'
##' The following package-specific options are defined:
##'
##' \tabular{lll}{
##' debuglevel \tab >= 0L \tab indicates how much debuging output is to be produced.\\
##' implementation.search \tab "package:unmixR" \tab indicate environments where unmixing implementations can be found
##' }
##'
##' @details 
##' \code{implementation.search}: register packages providing additional implementations by appending \code{"package:packagename"}. 
##' Implementations. The global environment can be added as \code{".GlobalEnv"}.
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

