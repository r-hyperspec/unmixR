##' @name nfindr
##' @rdname nfindr
##' @method nfindr formula
# @export nfindr formula

nfindr.formula <- function(formula, frame, p,
                           method="LDU", indices=sample(nrow(data), p), ...,
                           drop=FALSE) {
  data <- model.matrix(formula, frame)
  nfindr(data, p, method, indices, ..., drop=drop)
}
