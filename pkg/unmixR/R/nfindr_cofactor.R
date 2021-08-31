.get_cofactor_invariants <- function(data, indices, endmembers = seq_along(indices)) {
  p <- length(indices)
  n_endmembers <- length(endmembers)
  E <- .simplex(data, p, indices)
  
  # Calculate cofactor matrices
  cofactors <- matrix(NA_real_, nrow=p, ncol=n_endmembers)
  for (i in 1:p) {
    for (j in 1:n_endmembers) {
      if (((i+j) %% 2) == 0) {
        cofactors[i,j] <- det(E[-i,-endmembers[j]])
      } else {
        cofactors[i,j] <- -det(E[-i,-endmembers[j]])
      }
    }
  }
  
  list("cofactors" = cofactors)
}

.test(.get_cofactor_invariants) <- function() {
  data <- .points_2d
  p <- 3
  correct_volume <- simplex.volume(data,1:p,factorial=F)
  test_that("One endmember", {
    for (j in 1:p) {
      inv <- .get_cofactor_invariants(data, 1:p, j)
      expect_equal(names(inv), c("cofactors"))
      expect_equal(dim(inv$cofactors), c(p,1))
      expect_equal(
        abs(cbind(1,data[j,,drop=F]) %*% inv$cofactors),
        correct_volume,
        check.attributes=FALSE
      )
    }
  })

  test_that("Many endmembers", {
    inv <- .get_cofactor_invariants(data, 1:p, 1:2)
    expect_equal(names(inv), c("cofactors"))
    expect_equal(dim(inv$cofactors), c(p,2))
    expect_equal(
      abs(c(1, data[1,,drop=F]) %*% inv$cofactors[,1]),
      correct_volume,
      check.attributes=FALSE
    )
    expect_equal(
      abs(c(2, data[2,,drop=F]) %*% inv$cofactors[,2]),
      correct_volume,
      check.attributes=FALSE
    )

    inv <- .get_cofactor_invariants(data, 1:p, 1:p)
    expect_equal(names(inv), c("cofactors"))
    expect_equal(dim(inv$cofactors), c(p,p))
    expect_equal(
      abs(diag(cbind(rep(1,p),data[1:p,]) %*% inv$cofactors)),
      rep(correct_volume, p),
      check.attributes=FALSE
    )
  })

  test_that("Default all endmembers", {
    expect_equal(.get_cofactor_invariants(data, 1:p), .get_cofactor_invariants(data, 1:p, 1:p))
  })
}

.estimate_volume_change_by_cofactor <- function(data, indices, endmembers, new_indices, invariants=NULL) {
  n_points <- length(new_indices)
  n_endmembers <- length(endmembers)
  p <- length(indices)

  if (is.null(invariants)) {
    invariants <- .get_cofactor_invariants(data, indices, endmembers)
  }
  cofactors <- invariants$cofactors
  
  # Same as:
  # R <- cbind(rep(1,n_points), data[new_indices,])
  # determinants <- R %*% cofactors
  determinants <- data[new_indices,,drop=FALSE] %*% cofactors[-1,,drop=FALSE]
  determinants <- sweep(determinants, 2, cofactors[1,], "+")

  abs(determinants)
}

.test(.estimate_volume_change_by_cofactor) <- function() {
  p <- ncol(.points_2d) + 1
  m <- nrow(.points_2d)
  indices <- 1:p
  # Volume of the current simplex
  V <- .correct_volume_changes[1, 1]
  
  test_that("Basic functionality", {
    # All rows, all endmembers
    estimates <- .estimate_volume_change_by_cofactor(.points_2d, indices, 1:p, 1:m)
    expect_equal(dim(estimates), dim(.correct_volume_changes))
    expect_equal(estimates, 2*.correct_volume_changes)

    # Single endmember replacement
    estimates <- .estimate_volume_change_by_cofactor(.points_2d, indices, 1, 1:m)
    expect_equal(dim(estimates), c(m, 1))
    expect_equal(estimates, 2*.correct_volume_changes[, 1, drop = F])

    # Single row replacement
    estimates <- .estimate_volume_change_by_cofactor(.points_2d, indices, 1:p, m)
    expect_equal(dim(estimates), c(1, p))
    expect_equal(estimates, 2*.correct_volume_changes[m, , drop = F])
  })
  
  test_that("Use invariant", {
    expect_equal(
      .estimate_volume_change_by_cofactor(.points_2d, indices, 1:p, 1:m),
      .estimate_volume_change_by_cofactor(
        .points_2d, NULL, 1:p, 1:m, invariants=.get_cofactor_invariants(.points_2d, indices)
      )
    )
  })
}


.nfindr_cofactor_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  volume_best  <- simplex.volume(data, indices, factorial = FALSE)
  invariants <- .get_cofactor_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_cofactor(
        data, indices_best, 1:p, i,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > volume_best+1.5e-8)) {
        # Update current simplex vertices
        j <- which.max(estimates)
        indices_best[j] <- i
        volume_best <- estimates[j]
        invariants <- .get_cofactor_invariants(data, indices_best, 1:p)
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

  result <- list(
    "indices" = indices_best,
    "endmembers" = data[indices_best,]
  )
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}

.nfindr_cofactor_points <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  volume_best  <- simplex.volume(data, indices, factorial = FALSE)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (j in 1:p) {
      invariants <- .get_cofactor_invariants(data, indices_best, j)
      estimates <- .estimate_volume_change_by_cofactor(
        data, indices_best, j, 1:m,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > volume_best+1.5e-8)) {
        # Update current simplex vertices
        i <- which.max(estimates)
        indices_best[j] <- i
        volume_best <- estimates[i]
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

  result <- list(
    "indices" = indices_best,
    "endmembers" = data[indices_best,]
  )
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}

.nfindr_cofactor_both <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  volume_best  <- simplex.volume(data, indices, factorial = FALSE)
  invariants <- .get_cofactor_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    estimates <- .estimate_volume_change_by_cofactor(
      data, indices_best, 1:p, 1:m,
      invariants = invariants
    )
  
    if (any(estimates > volume_best+1.5e-8)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
      volume_best <- estimates[max_ij[1], max_ij[2]]
      invariants <- .get_cofactor_invariants(data, indices_best, 1:p)
      # Mark that a replacement took place
      is_replacement <- TRUE
      # For debugging
      n_replacements <- n_replacements + 1
      if (debug.level > 1) {
        replacements <- rbind(replacements, indices_best)
      }
    }
  }

  result <- list(
    "indices" = indices_best,
    "endmembers" = data[indices_best,]
  )
  if (debug.level > 0) {
    result[["iterations_count"]] <- k
    result[["replacements_count"]] <- n_replacements
  }
  if (debug.level > 1) {
    result[["replacements"]] <- replacements
  }
  
  result
}
