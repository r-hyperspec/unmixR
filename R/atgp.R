#' ATGP (Automatic Target Generation Procedure) algorithm.
#'
#' @param data Data matrix. It will be converted to a matrix using
#'   as.matrix. The matrix should contain a spectrum per row.
#'
#' @param p Number of endmembers.
#'
#' @references Based on Python implementation in [pysptools](https://github.com/ctherien/pysptools/blob/fbcd3ecaa7ab27f0158b28b4327537c3e75db160/pysptools/eea/eea.py#L52).
#' Also matches [matlabHyperspectralToolbox](https://github.com/isaacgerg/matlabHyperspectralToolbox/blob/master/hyperspectralToolbox/hyperAtgp.m)
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
    # Same as `P <- MASS::Null(t(data[indices,,drop=FALSE]))`
    # But MASS::Null is very slow, it is faster to use orthogonal complement:
    # $P = I - U(UU^T)^{-1}U^T$
    # P is symmetric matrix, so we use Ut directly to reduce transpositions
    Ut <- data[indices, ,drop=FALSE]
    P <- diag(ncol(data)) - t(Ut) %*% solve(tcrossprod(Ut), Ut)
    projections <- data %*% P
    
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
  
  # Reference implementation from matlabHyperspectralToolbox
  .ref_hyperAtgp <- function(M, q, Maug = NULL) {
    # Inputs:
    #   M    - Matrix of hyperspectral data (p x N)
    #   q    - Number of endmembers
    #   Maug - Optional initial endmembers (p x k)
    # Outputs:
    #   List containing:
    #     U        - Extracted endmember matrix (p x q)
    #     indices  - Indices in M of selected endmembers
    
    p <- nrow(M)
    N <- ncol(M)
    
    U <- matrix(numeric(0), nrow = p)
    indices <- integer(0)
    
    # Step 1: Find the pixel with the largest norm
    norms <- colSums(M * M)  # squared L2 norm for each column
    idx <- which.max(norms)
    indices <- c(indices, idx)
    U <- M[, idx, drop = FALSE]
    
    start <- 1
    
    # Step 2: Use initial endmembers if provided
    if (!is.null(Maug)) {
      U <- Maug
      # start <- ncol(Maug) + 1  # Not used in original loop
    }
    
    # Step 3: Iteratively find new endmembers
    for (n in start:(q - 1)) {
      # Orthogonal projection matrix
      P <- diag(p) - U %*% solve(t(U) %*% U) %*% t(U)
      
      # Compute projection magnitudes
      projected <- P %*% M
      norms <- colSums(projected * projected)
      
      idx <- which.max(abs(norms))
      indices <- c(indices, idx)
      U <- cbind(U, M[, idx])
    }
    
    return(list(U = U, indices = indices))
  }

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

  test_that("ATGP works same as MATLAB implementation", {
    # Compare with the reference implementation

    set.seed(123)
    # Generate a random matrix with 5 rows and 10 columns
    X <- matrix(rnorm(100*20), nrow = 100)

    res <- atgp(X, p = 3)$indices
    ref <- .ref_hyperAtgp(t(X), q = 3)$indices
    expect_equal(res, ref)
  })

}