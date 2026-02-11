#' Vertex Component Analysis Unmixing Algorithm
#'
#' This algorithm is based on the geometry of convex sets. It exploits the
#' fact that endmembers occupy the vertices of a simplex.
#' Intended to be called from \code{\link{vca}}.
#'
#' @param data Data matrix. Samples in rows, frequencies in columns.
#'   This function expects data to be already dimension-reduced (for example
#'   with \code{\link{vca_dr}}). The number of endmembers is inferred as
#'   \code{ncol(data)}.
#'
#' @return A list which contains:
#'   \itemize{
#'     \item \strong{indices}: indices of endmembers in extraction order.
#'     \item \strong{projection_vectors}: projection vectors (row-wise) used
#'       in each iteration, included only when \code{debuglevel >= 1}.
#'   }
#'   Both elements are returned in the same order as the endmembers are
#'   extracted.
#'
#' @references Nascimento, J.M.P. and Bioucas Dias, J.M. "Vertex component
#'   analysis: a fast algorithm to unmix hyperspectral data," Geoscience and
#'   Remote Sensing, vol. 43, no. 4, pp. 898-910, April 2005,
#'   doi: 10.1109/TGRS.2005.844293
#'
#' @export

vca_nascimento <- function(data) {
  Y <- t(as.matrix(data))
  p <- nrow(Y)

  indices <- integer(p)
  # the matrix A stores the projection of the estimated endmember signatures
  A <- matrix(0, nrow = p, ncol = p)
  A[p, 1] <- 1

  if (.options("debuglevel") >= 1L) {
    projection_vectors <- matrix(NA_real_, nrow = p, ncol = p)
  }

  for (i in 1:p) {
    # getting vector f orthonormal to the space spanned by A
    w <- stats::rnorm(p, sd = 1)
    f <- (diag(p) - A %*% MASS::ginv(A)) %*% w
    f <- f / sqrt(sum(f^2))
    # projecting data onto f
    v <- crossprod(f, Y)
    # getting index of the maximal projection
    k <- which.max(abs(v))

    # i-th column of A is set to estimated endmember
    A[, i] <- Y[, k]
    indices[i] <- k

    if (.options("debuglevel") >= 1L) {
      projection_vectors[i, ] <- as.vector(f)
    }
  }

  res <- list(indices = as.integer(indices))
  if (.options("debuglevel") >= 1L) {
    res[["projection_vectors"]] <- projection_vectors
  }
  res
}

.test(vca_nascimento) <- function() {
  context("vca_nascimento")

  old_debuglevel <- unmixR.options("debuglevel")
  on.exit(unmixR.options(debuglevel = old_debuglevel), add = TRUE)

  test_that("vca_nascimento returns valid indices", {
    unmixR.options(debuglevel = 0L)
    set.seed(123)
    res <- vca_nascimento(.testdata$x)

    expect_true(is.list(res))
    expect_equal(names(res), "indices")
    expect_equal(sort(res$indices), .correct)
  })

  test_that("vca_nascimento is reproducible for a fixed seed", {
    unmixR.options(debuglevel = 0L)
    set.seed(42)
    indices_1 <- vca_nascimento(.testdata$x)$indices
    set.seed(42)
    indices_2 <- vca_nascimento(.testdata$x)$indices

    expect_identical(indices_1, indices_2)
  })

  test_that("vca_nascimento includes projection vectors in debug mode", {
    unmixR.options(debuglevel = 1L)
    set.seed(7)
    res <- vca_nascimento(.testdata$x)

    expect_true(all(c("indices", "projection_vectors") %in% names(res)))
    expect_equal(dim(res$projection_vectors), c(ncol(.testdata$x), ncol(.testdata$x)))
    expect_true(all(is.finite(res$projection_vectors)))
  })
}
