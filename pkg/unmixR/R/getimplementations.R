#' Find implementations 
#' 
#' Find all currently available implementations for methods like \code{"\link{nfindr}"}
#' or \code{"\link{vca}"}.  Also permits implementations in other packages to be
#' integrated into \pkg{unmixR}.
#'
#' @param method The method (e.g. \code{"\link{nfindr}"} or \code{"\link{vca}"}).
#'
#' @param search.paths Vector with packages and/or environments that should be searched
#' for implementations.  Default: implementations built into \pkg{unmixR} (set by
#' \code{unmixR.options}).  To integrate implementations from other packages which have a 
#' compatible interface, give packages in the form \code{"package:unmixR"}. 
#'
#' @return Vector with available method names
#'
#' @export
#'
#' @examples
#' get.implementations ("nfindr")
#'
get.implementations  <- function (method, search.paths = unmixR.options("implementation.search")) {
  search.paths <- lapply (search.paths, as.environment)
  
  implementations <- sapply (search.paths, ls, pattern = sprintf ("^%s[^.].*", method))
  
  implementations <- gsub (method, "", implementations)
  
  implementations <- implementations [nzchar (implementations)] 
  
  return(implementations)
}
