#' Find Implementations
#'
#' Find all currently available implementations for methods like
#' \code{"\link{nfindr}"} or \code{"\link{vca}"}.
#'
#' @param method The method (e.g. \code{"\link{nfindr}"} or \code{"\link{vca}"}).
#' @param search.paths A vector with packages and/or environments that should
#'   be searched for implementations. Give packages in the form
#'   \code{"package:unmixR"}.
#'
#' @return A vector with available method names.
#' @export
#'
#' @examples
#' get.implementations("nfindr")
#' get.implementations("vca")
get.implementations <- function(method, search.paths = unmixR.options("implementation.search")) {
  search.paths <- lapply(search.paths, as.environment)

  # Get all methods starting with `method`
  implementations <- sapply(search.paths, ls, pattern = sprintf("^%s[^.].*", method))
  
  # Remove method name (with and without "_") from prefix
  implementations <- gsub(paste0(method, "_"), "", implementations)
  implementations <- gsub(method, "", implementations)

  implementations <- implementations[nzchar(implementations)]

  implementations <- unique(implementations)
  if (identical(method, "vca")) {
    implementations <- implementations[!implementations %in% c("dr", "snr")]
  }

  implementations
}
