##' @name nfindr
##' @rdname nfindr
##' @export
##' @include nfindr.default.R
##' @importFrom stats model.matrix
##'
nfindr.formula <- function(formula, frame, p,
                           method="LDU", indices = sample (nrow (data), p), ...,
                           drop=FALSE) {

  data <- stats::model.matrix (formula, frame)

  nfindr (data, p, method, indices, ..., drop=drop)
}
