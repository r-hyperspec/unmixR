##' @name nfindr
##' @rdname nfindr
##' @export
##' @include nfindr.default.R
##' @importFrom stats model.matrix terms
##'
nfindr.formula <- function(formula, frame, ...) {
  mt <- terms(formula, data = frame)
  
  ## response term is not allowed
  if (attr(mt, "response") > 0L) stop("N-FINDR models cannot have response")
  
  ## drop intercept silently
  attr(mt, "intercept") <- 0L
  
  data <- model.matrix (mt, frame)

  nfindr (data, ...)
}

