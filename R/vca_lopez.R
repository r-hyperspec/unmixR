#' Modified Vertex Component Analysis
#'
#' Modified VCA algorithm that aims to reduce the algorithmic complexity of
#' the original.
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
#' @references Lopez, S., Horstrand, P., Callico, G.M., Lopez J.F. and
#' Sarmiento, R., "A Low-Computational-Complexity Algorithm for
#' Hyperspectral Endmember Extraction: Modified Vertex Component Analysis,"
#' Geoscience & Remote Sensing Letters, IEEE, vol. 9 no. 3 pp. 502-506, May 2012
#' doi: 10.1109/LGRS.2011.2172771
#' @export

vca_lopez <- function(data) {
  Y <- t(as.matrix(data))
  p <- nrow(Y)

  indices <- integer(p)
  # matrix of endmembers
  E <- matrix(0, nrow = p, ncol = p + 1)
  E[p, 1] <- 1
  # U stores the set of orthogonal vectors
  U <- matrix(0, nrow = p, ncol = p)
  # p x 1 vector
  w <- c(rep(1, p))
  proj_acc <- c(rep(0, p))

  if (.options("debuglevel") >= 1L) {
    projection_vectors <- matrix(NA_real_, nrow = p, ncol = p)
  }

  for (i in 1:p) {
    # U_i is initialized with the endmember computed in the last iteration
    U[, i] <- E[, i]

    # Gram-Schmidt orthogonalization
    if (i >= 3) {
      for (j in 3:i) {
        coef <- as.numeric(crossprod(E[, i], U[, j - 1]) / crossprod(U[, j - 1]))
        proj_ei_uj_1 <- coef * U[, j - 1]
        U[, i] <- U[, i] - proj_ei_uj_1
      }
    }
    # U_i is orthogonal to other i - 1 vectors

    # projecting w onto U_i
    coef <- as.numeric(crossprod(w, U[, i]) / crossprod(U[, i]))
    proj_w_ui <- coef * U[, i]

    # vector f is orthogonal to the subspace spanned by columns of E
    proj_acc <- proj_acc + proj_w_ui
    f <- w - proj_acc

    # projection accumulator is reset on the first iteration
    if (i == 1) {
      proj_acc <- c(rep(0, p))
    }
    # projecting data onto f
    v <- crossprod(f, Y)
    # getting index of the maximal projection
    index <- which.max(abs(v))
    indices[i] <- index
    # estimated endmember is stored in E
    E[, i + 1] <- Y[, index]

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

.test(vca_lopez) <- function() {
  context("vca_lopez")

  old_debuglevel <- unmixR.options("debuglevel")
  on.exit(unmixR.options(debuglevel = old_debuglevel), add = TRUE)

  test_that("vca_lopez returns valid indices", {
    unmixR.options(debuglevel = 0L)
    res <- vca_lopez(.testdata$x)

    expect_true(is.list(res))
    expect_equal(names(res), "indices")
    expect_equal(sort(res$indices), .correct)
  })

  test_that("vca_lopez includes projection vectors in debug mode", {
    unmixR.options(debuglevel = 1L)
    res <- vca_lopez(.testdata$x)

    expect_true(all(c("indices", "projection_vectors") %in% names(res)))
    expect_equal(dim(res$projection_vectors), c(ncol(.testdata$x), ncol(.testdata$x)))
    expect_true(all(is.finite(res$projection_vectors)))
  })
}
