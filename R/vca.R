#' General Interface to Vertex Component Analysis Spectral Unmixing
#' Implementations
#'
#' This algorithm is based on the geometry of convex sets. It exploits the
#' fact that endmembers occupy the vertices of a simplex.
#'
#' @param data Data matrix. It will be converted to a matrix using
#'   as.matrix. The matrix should contain a spectrum per row. If dimension
#'   of the data is higher than \code{p}, \code{\link{vca_dr}} is applied
#'   to reduce the data to \code{p} dimensions before running projections.
#'
#' @param p Number of endmembers.
#'
#' @param method The VCA algorithm to use. Options:
#'   \itemize{
#'     \item nascimento (\code{\link{vca_nascimento}})
#'     \item lopez (\code{\link{vca_lopez}})
#'   }
#'   Default: \code{nascimento}.
#'
#' @return A list which contains:
#'   \itemize{
#'     \item \strong{indices}: sorted indices of the calculated endmembers.
#'     \item \strong{projection_vectors}: projection vectors in iteration
#'       order, included only when \code{debuglevel >= 1}.
#'     \item \strong{unsorted_indices}: unsorted list of indices,
#'       i.e., in the same order as iteration, included only
#'       when \code{debuglevel >= 1}.
#'   }
#'
#'
#' @seealso \code{\link{endmembers}} to extract the spectra; \code{\link{predict}}
#' to determine abundances of endmembers in each sample.
#'
#' @rdname vca
#' @export
#' @include unmixR-package.R
#'
#' @examples
#' data(demo_data)
#' vca_demo <- vca(demo_data, p = 2)
#' em <- endmembers(vca_demo, demo_data)
#' matplot(t(em), type = "l")
vca <- function(data, p, method = c("nascimento", "lopez")) {
  # check if the method passed in is valid
  method <- match.arg(method)

  # transform the input into a matrix
  data <- as.matrix(data)

  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2 || p > ncol(data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }

  vcaFunc <- get(paste("vca", method, sep = "_"), mode = "function")
  if (ncol(data) == p) {
    res <- vcaFunc(data)
  } else {
    res <- vcaFunc(vca_dr(data, p))
  }

  if (.options("debuglevel") >= 1L) {
    res[["unsorted_indices"]] <- res$indices
  }
  res$indices <- sort(res$indices)

  class(res) <- "vca"
  return(res)
}

.test(vca) <- function() {
  context("vca")

  old_debuglevel <- unmixR.options("debuglevel")
  on.exit(unmixR.options(debuglevel = old_debuglevel), add = TRUE)

  test_that("vca validates p and method", {
    expect_error(vca(.testdata$x, p = "---"))
    expect_error(vca(.testdata$x, p = 0))
    expect_error(vca(.testdata$x, p = 1))
    expect_error(vca(.testdata$x, p = 4))
    expect_error(vca(.testdata$x, p = 3, method = "invalid"))
  })

  test_that("vca exposes expected implementations", {
    implementations <- get.implementations("vca")
    expect_equal(sort(implementations), c("lopez", "nascimento"))
  })

  test_that("correct results for all available methods: triangle data (debuglevel = 0)", {
    unmixR.options(debuglevel = 0L)

    for (method in get.implementations("vca")) {
      set.seed(17)
      res <- vca(.testdata$x, p = 3, method = method)

      expect_s3_class(res, "vca")
      expect_true(names(res) == "indices", info = method)
      expect_equal(res$indices, .correct, info = method)
    }
  })

  test_that("vca adds debug fields and sorts indices when debuglevel >= 1", {
    unmixR.options(debuglevel = 1L)

    set.seed(17)
    res <- vca(.testdata$x, p = 3)

    expect_true(all(c("indices", "unsorted_indices", "projection_vectors") %in% names(res)))
    expect_equal(res$indices, .correct)
    expect_equal(dim(res$projection_vectors), c(ncol(.testdata$x), ncol(.testdata$x)))
  })

  test_that("vca on higher dimensional data reduces dimensions before running projections", {
    set.seed(17)

    # Prepare a mixture with true endmembers
    vertices <- sample(laser$spc, size = 3)
    inner_points <- .get_simplex_points(vertices, max_coefficient = c(0.8, 0.8, 0.8))
    X <- rbind(vertices, inner_points)
    # Shuffle rows to make sure the order is not informative
    i <- sample(nrow(X))
    o <- order(i)
    X <- X[i, ]
    vertex_indices <- sort(o[1:3])

    res <- vca(X, p = 3)
    ems <- endmembers(res, X)
    expect_equal(res$indices, vertex_indices)
  })

  test_that("no duplicates with Lopez2012 for test data", {
    indices <- replicate(10, vca(.testdata$x, p = 2, method = "lopez")$indices)
    expect_true(all(indices %in% .correct))
    expect_true(all(indices[1, ] != indices[2, ]), info = "vca lopez duplicate indices: testdata, p = 2")
  })

  ## all 3 components should be recovered, vca output is sorted.
  test_that("vca output is sorted", {
    indices <- vca(.testdata$x, p = 3)$indices
    expect_equal(indices, sort(indices))
  })

  # test: hyperSpec object
  test_that("vca on hyperSpec object", {
    set.seed(17)
    hspc <- vca(laser, p = 2)$indices
    set.seed(17)
    spc <- vca(laser$spc, p = 2)$indices

    expect_equal(hspc, spc)
  })
}
