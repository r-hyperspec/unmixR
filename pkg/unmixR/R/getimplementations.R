#' Find implementatios 
#' 
#' Fnd all currently available implementations for methdos like \code{"\link{nfindr}"} or \code{"\link{vca}"}).
#'
#' @param method the method (e.g. \code{"\link{nfindr}"} or \code{"\link{vca}"})
#' @param search.paths vector with packages and/or environment that should be searched for implementations.
#'  Give packages in the form \code{"package:unmixR"}. 
#'
#' @return vector with available method names
#' @export
#'
#' @examples
#' get.implementations ("nfindr")
get.implementations  <- function (method, search.paths = unmixR.options("implementation.search")){
  search.paths <- lapply (search.paths, as.environment)
  
  implementations <- sapply (search.paths, ls, pattern = method)
  
  implementations <- gsub (method, "", implementations)
  implementations <- implementations [nzchar (implementations)] 
  
  implementations
}
