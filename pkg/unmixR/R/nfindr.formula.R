##' @name nfindr
##' @rdname nfindr
##' @export
##' @include nfindr.default.R

nfindr.formula <- function(formula, frame, p,
                           method="LDU", indices = sample (nrow (data), p), ...,
                           drop=FALSE) {

  data <- model.matrix (formula, frame)

  nfindr (data, p, method, indices, ..., drop=drop)
}
