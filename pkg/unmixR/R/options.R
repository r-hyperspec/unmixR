##' unmixR Package Options
##'
##' \pkg{unmixR} uses \pkg{settings} for option management.
##'
##' The following package-specific options are defined:
##'
##' \describe{
##'   \item{debuglevel}{Indicates how much debuging output is to be produced.
##' A value of 1 reports on the overall progress of identifying the endmembers.
##' Values > 1 give additional details about the internal processing.}
##' }
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
  debuglevel = 0L
  )

.test (unmixR.options) <- function (){

  ## check list of defined options against (manually kept) list of documented
  ## options.
  svUnit::checkEquals (c("debuglevel"), names (.options ()))

}

