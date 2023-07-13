.get_ldu_invariants <- function(data, indices, endmembers = seq_along(indices)) {
  p <- length(indices)
  n_endmembers <- length(endmembers)
  E <- .simplex_E(data, indices)
  
  G <- sapply(endmembers, function(j) {
    A <- E[-p, -j, drop=FALSE]
    c <- E[p, -j]
    crossprod(c, solve(A))
  })

  # If there are more than one endmember, calculate determinants of A matrices
  # This is needed for making estimations compatible
  determinants <- NULL
  if (n_endmembers>1) {  
    determinants <- sapply(endmembers, function(j) {
      det(E[-p, -j, drop=FALSE])
    })
  }

  list("G" = G, "determinants"=determinants)
}


.estimate_volume_change_by_ldu <- function(data, indices, endmembers, new_indices, invariants = NULL) {
  n_points <- length(new_indices)
  n_endmembers <- length(endmembers)
  p <- length(indices)
  n <- ncol(data)

  if (is.null(invariants)) {
    invariants <- .get_ldu_invariants(data, indices, endmembers)
  }
  G <- invariants$G
  determinants <- invariants$determinants

  d <- data[new_indices, n]
  # Same as:
  # B <- rbind(rep(1, n_points), t(data[new_indices, -n]))
  # V[,j] <- d - g %*% B
  Bt <- data[new_indices, -n, drop=FALSE]
  V <- sweep(Bt %*% G[-1, ], 2, G[1, ], "+")
  V <- sweep(V, 1, d, "-")

  if (n_endmembers>1) {
    V <- sweep(V, 2, invariants$determinants, "*")
  }

  abs(V)
}


.test(.estimate_volume_change_by_ldu) <- function() {
  set.seed(927)
  vertices <- rbind(c(-5, 0), c(0, 4), c(10, 0))
  data <- .get_simplex_points(vertices)
  p <- ncol(data) + 1
  m <- nrow(data)
  indices <- 1:p

  test_that("Basic functionality", {
    # All rows, all endmembers
    expect_equal(
      .estimate_volume_change_by_ldu(data, indices, 1:p, 1:m),
      .estimate_volume_change_by_volume(data, indices, 1:p, 1:m)
    )

    # Single endmember replacement
    detA <- abs(det(.simplex_E(data, indices)[-p,-1]))
    expect_equal(
      detA*.estimate_volume_change_by_ldu(data, indices, 1, 1:m),
      .estimate_volume_change_by_volume(data, indices, 1, 1:m)
    )

    # Single row replacement
    expect_equal(
      .estimate_volume_change_by_ldu(data, indices, 1:p, m),
      .estimate_volume_change_by_volume(data, indices, 1:p, m)
    )
  })
  
  test_that("Use invariant", {
    expect_equal(
      .estimate_volume_change_by_ldu(data, indices, 1:p, 1:m),
      .estimate_volume_change_by_ldu(
        data, NULL, 1:p, 1:m, invariants=.get_ldu_invariants(data, indices)
      )
    )
  })
}


.nfindr_ldu_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  volume_best  <- simplex_volume(data, indices, factorial = FALSE)
  invariants <- .get_ldu_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_ldu(
        data, indices_best, 1:p, i,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > volume_best+1.5e-8)) {
        # Update current simplex vertices
        j <- which.max(estimates)
        indices_best[j] <- i
        volume_best <- estimates[j]
        invariants <- .get_ldu_invariants(data, indices_best, 1:p)
        # Mark that a replacement took place
        is_replacement <- TRUE
        # For debugging
        n_replacements <- n_replacements + 1
        if (debug.level > 1) {
          replacements <- rbind(replacements, indices_best)
        }
      }
    }
  }

  result <- list("indices" = indices_best)
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}

.nfindr_ldu_points <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (j in 1:p) {
      invariants <- .get_ldu_invariants(data, indices_best, j)
      estimates <- .estimate_volume_change_by_ldu(
        data, indices_best, j, 1:m,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > estimates[indices_best[j]])) {
        # Update current simplex vertices
        i <- which.max(estimates)
        indices_best[j] <- i
        # Mark that a replacement took place
        is_replacement <- TRUE
        # For debugging
        n_replacements <- n_replacements + 1
        if (debug.level > 1) {
          replacements <- rbind(replacements, indices_best)
        }
      }
    }
  }

  result <- list("indices" = indices_best)
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}

.nfindr_ldu_both <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  volume_best  <- simplex_volume(data, indices, factorial = FALSE)
  invariants <- .get_ldu_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    estimates <- .estimate_volume_change_by_ldu(
      data, indices_best, 1:p, 1:m,
      invariants = invariants
    )
  
    if (any(estimates > volume_best+1.5e-8)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
      volume_best <- estimates[max_ij[1], max_ij[2]]
      invariants <- .get_ldu_invariants(data, indices_best, 1:p)
      # Mark that a replacement took place
      is_replacement <- TRUE
      # For debugging
      n_replacements <- n_replacements + 1
      if (debug.level > 1) {
        replacements <- rbind(replacements, indices_best)
      }
    }
  }

  result <- list("indices" = indices_best)
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}
