#' ATGP (Automatic Target Generation Procedure) algorithm.
#'
#' @param data Data matrix. It will be converted to a matrix using
#'   as.matrix. The matrix should contain a spectrum per row.
#'
#' @param p Number of endmembers.
#'
#' @references Based on Python implementation in [pysptools](https://github.com/ctherien/pysptools/blob/fbcd3ecaa7ab27f0158b28b4327537c3e75db160/pysptools/eea/eea.py#L52)
#'
#' @return A list which contains:
#'   \itemize{
#'     \item \strong{indices}: the indices of the calculated endmembers.
#'     \item \strong{projection_vectors}: the projection vectors of the calculated endmembers.
#'       Included only when debug level is 1 or higher.
#'   }
#'
#' @name atgp
#' @rdname atgp
#' @export
#' @include unmixR-package.R
atgp <- function(data, p) {
  data <- as.matrix(data)

  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2 || p > ncol(data)) {
    stop("p must be a positive integer >= 2 and <= ncol (data)")
  }
  
  # Get index of t0
  lengths <- rowSums(data^2)
  indices <- which.max(lengths)
  
  projection_vectors <- NULL
  while (length(indices) < p) {
    # Get the next projection vector
    P <- MASS::Null(t(data[indices,,drop=FALSE]))
    
    # Save it for debugging
    if (.options("debuglevel") >= 1L) {
        if (is.null(projection_vectors)) {
            projection_vectors <- P
        } else {
            projection_vectors <- cbind(projection_vectors, P)
        }
    }

    projections <- data %*% P
    lengths <- rowSums(projections*projections)
    indices <- c(indices, which.max(lengths))
  }
  
  res <- list("indices" = as.vector(indices))
  if (.options("debuglevel") >= 1L) {
    res[["projection_vectors"]] <- t(projection_vectors)
  }

  return(res)
}

# Add tests
.test(atgp) <- function() {
  context("ATGP")
  
  test_that("ATGP produces error for invalid values of p", {
    expect_error(atgp(.testdata$x, p = "---"))
    expect_error(atgp(.testdata$x, p = 0))
    expect_error(atgp(.testdata$x, p = 1))
    expect_error(atgp(.testdata$x, p = 4))
  })

  test_that("ATGP produces correct results", {
    expect_equal(atgp(.testdata$x, p = 3)$indices, .correct)
  })

  test_that("ATGP works in higher dimensions", {
    expect_equal(atgp(.testdata$x, p = 2)$indices, .correct[1:2])
  })
}