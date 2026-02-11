#' Calculate abundances
#'
#' These functions calculate the abundances of endmembers in the given data
#' using either non-negative least squares (NNLS) or barycentric coordinates.
#' Barycentric coordinates require the intrinsic simplex space, i.e. endmembers
#' must be a (N+1) x N matrix. If you used PCA, consider providing data in PCA
#' scores space.
#' NNLS is more robust and can be applied in the original space. However, the values
#' are not guaranteed to sum to 1. Use the \code{normalize} argument to normalize
#' the abundances.
#'
#' For pure-pixel extractors (e.g. \code{\link{nfindr}}, \code{\link{vca}},
#' \code{\link{atgp}}), the model object can be passed directly. For methods
#' that return endmember matrices directly (e.g. \code{\link{ice}}), pass that
#' matrix.
#'
#' @param endmembers Either a matrix where each row is an endmember, or a
#'   pure-pixel model object (class \code{pure_endmembers}) such as output from
#'   \code{\link{nfindr}}, \code{\link{vca}}, or \code{\link{atgp}}.
#' @param data A matrix where each row is a spectrum.
#' @param method A character string specifying the method to use for abundance calculation. 
#'   Options are "nnls" (default) for non-negative least squares and "bary" for barycentric coordinates.
#' @param normalize A logical value indicating whether to normalize the abundances so that they sum to 1.
#'
#' @return A matrix where each row is the abundance percentages of the endmembers for the corresponding spectrum.
#'
#' @examples
#' data("demo_data")
#' 
#' # Reduce data dimensionality with PCA
#' pca <- prcomp(demo_data)
#' x <- pca$x[,1:2]
#' 
#' # Perform N-FINDR in reduced space
#' nf <- nfindr(x, p = 3)
#' 
#' # Calculate abundances using barycentric coordinates
#' # it works only in the reduced space
#' ab_bary <- abundances(nf, x, method = "bary")
#' 
#' # Calculate abundances using NNLS
#' # it works in both spaces but it is better to be applied in the original space
#' ab_nnls <- abundances(nf, demo_data, method = "nnls", normalize = TRUE)
#'
#' # One can use the specific endmember matrix directly:
#' # `abundances(nf, x)` is the same as `abundances(endmembers(nf, x), x)`
#' ems <- endmembers(nf, demo_data)
#' ab_from_matrix <- abundances(ems, demo_data)
#'
#' # Alternatively, one can use the specific function directly
#' nnls(ems, demo_data) # not normalized
#' bary(endmembers(nf, x), x)
#'
#' @name abundances
#' @include unmixR-package.R
NULL


#' @rdname abundances
#' @export
#' @importFrom nnls nnls
nnls <- function(endmembers, data) {
  endmembers <- as.matrix(endmembers)
  data <- as.matrix(data)
  
  # Check for valid dimensions
  if (ncol(endmembers) != ncol(data)) {
	stop("Data and endmembers must have the same number of columns")
  }
  
  # Solve the non-negative least squares problem for each row in data
  res <- t(apply(data, 1, function(x) {nnls::nnls(t(endmembers), x)[["x"]]}))
  
  return(res)
}

#' @rdname abundances
#' @export
bary <- function(endmembers, data) {
  data <- as.matrix(data)
  endmembers <- as.matrix(endmembers)
  
  # Check for valid dimensions
  if (ncol(data) != ncol(endmembers)) {
	  stop("Data and endmembers must have the same number of columns")
  }
  if (nrow(endmembers) != ncol(endmembers) + 1) {
    stop(
      "Barycentric coordinates require the intrisic simplex space, ",
      "i.e. endmembers must be a (N+1) x N matrix. If you used PCA, ",
      "consider providing data in PCA scores space."
    )
  }
  
  # Center the data and endmembers
  data_c <- scale(data, center = endmembers[1,], scale = FALSE)
  endmembers_c <- scale(endmembers[-1,], center = endmembers[1,], scale = FALSE)

  # Calculate the barycentric coordinates
  coords <- data_c %*% solve(endmembers_c)
  coords <- cbind(1-rowSums(coords), coords)
  
  return(coords)
}

#' @rdname abundances
#' @export
abundances <- function(endmembers, data, method=c("nnls", "bary"), normalize=FALSE) {
	method <- match.arg(method)

  if (inherits(endmembers, "pure_endmembers")) {
    get_endmembers <- get("endmembers", mode = "function")
    endmembers <- get_endmembers(endmembers, data)
  }
  
  if (method == "nnls") {
	  res <- nnls(endmembers, data)
  } else if (method == "bary") {
	  res <- bary(endmembers, data)
  }
  
  if (normalize) {
	  res <- res / rowSums(res)
  }
  
  return(res)
}

# Add tests
.test(abundances) <- function() {
  context("abundances")
  
  endmembers <- rbind(
    c(1, 0),
    c(0, 1),
    c(1, 1)
  )
  data <- rbind(
    c(0.5, 0.5),
    c(0.2, 0.8),
    c(0.1, 0.9),
    c(0.7, 0.3)
  )

  test_that("nnls", {  
    result <- nnls(endmembers, data)
    
    expect_equal(ncol(result), nrow(endmembers))
    expect_equal(nrow(result), nrow(data))
    expect_true(all(result >= 0))
  })

  test_that("bary", {   
    result <- bary(endmembers, data)
    
    expect_equal(ncol(result), nrow(endmembers))
    expect_equal(nrow(result), nrow(data))
    expect_true(all(rowSums(result) == 1))
    expect_equal(result, geometry::cart2bary(endmembers, data))
  })

  test_that("abundances", {
    ab_nnls <- abundances(endmembers, data, method = "nnls")
    expect_equal(ab_nnls, nnls(endmembers, data))
    
    ab_nnls_norm <- abundances(endmembers, data, method = "nnls", normalize = TRUE)
    expect_equal(ab_nnls_norm, ab_nnls / rowSums(ab_nnls))

    ab_bary <- abundances(endmembers, data, method = "bary")
    expect_equal(ab_bary, bary(endmembers, data))
  })

  test_that("abundances accepts pure_endmembers objects", {
    pure_model <- list(indices = c(1, 2, 3))
    class(pure_model) <- "pure_endmembers"

    ab_from_model <- abundances(pure_model, data, method = "nnls")
    ab_from_matrix <- abundances(data[pure_model$indices, , drop = FALSE], data, method = "nnls")
    expect_equal(ab_from_model, ab_from_matrix)

    bary_data <- endmembers
    ab_bary_from_model <- abundances(pure_model, bary_data, method = "bary")
    ab_bary_from_matrix <- abundances(bary_data[pure_model$indices, , drop = FALSE], bary_data, method = "bary")
    expect_equal(ab_bary_from_model, ab_bary_from_matrix)
  })

  test_that("abundances pure_endmembers shorthand works for pure-pixel algorithms", {
    pure_data <- .testdata$x[,1:2]

    nf <- nfindr(pure_data, p = 3)
    expect_equal(abundances(nf, pure_data), abundances(endmembers(nf, pure_data), pure_data))

    vc <- vca(pure_data, p = 2)
    expect_equal(abundances(vc, pure_data), abundances(endmembers(vc, pure_data), pure_data))

    ag <- atgp(pure_data, p = 2)
    expect_equal(abundances(ag, pure_data), abundances(endmembers(ag, pure_data), pure_data))
  })

  test_that("pure_endmembers object must have indices", {
    bad_model <- structure(list(), class = "pure_endmembers")
    expect_error(abundances(bad_model, data), "indices")
  })


  test_that("invalid method", {   
    expect_error(abundances(endmembers, data, method = "invalid"))
  })

  test_that("dimension mismatch", {
    expect_error(nnls(endmembers[,1,drop=FALSE], data))   
    expect_error(bary(endmembers[,1,drop=FALSE], data))
    expect_error(bary(endmembers[1:2,], data))
  })
}
