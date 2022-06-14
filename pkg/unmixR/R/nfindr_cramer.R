##' Estimate volume change (N-FINDR)
##'
##' See [estimate_volume_change()] with `Cramer` estimator for the details
##'
##' @noRd
.estimate_volume_change_by_cramer <- function(data, indices, endmembers, new_indices, Einv = NULL) {
  if (is.null(Einv)) {
    p <- length(indices)
    E <- .simplex(data, length(indices), indices)
    Einv <- solve(E)
  }
  
  # Same as: ratios <- Einv %*% .simplex_E(data, new_indices)
  # Here we use the fact that the first row in `.simplex_E` would be 1s,
  # so we avoid creation of a new matrix `.simplex_E`
  ratios <-
    Einv[endmembers, 1] +
    tcrossprod(Einv[endmembers, -1], data[new_indices, , drop = F])
  
  t(abs(ratios))
}

.test(.estimate_volume_change_by_cramer) <- function() {
  p <- ncol(.points_2d) + 1
  m <- nrow(.points_2d)
  indices <- 1:p
  # Volume of the current simplex
  V <- .correct_volume_changes[1, 1]
  
  test_that("Basic functionality", {
    # All rows, all endmembers
    estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1:p, 1:m)
    expect_equal(dim(estimates), dim(.correct_volume_changes))
    expect_equal(V * estimates, .correct_volume_changes)
    
    # Single endmember replacement
    estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1, 1:m)
    expect_equal(dim(estimates), c(m, 1))
    expect_equal(V * estimates, .correct_volume_changes[, 1, drop = F])
    
    # Single row replacement
    estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1:p, m)
    expect_equal(dim(estimates), c(1, p))
    expect_equal(V * estimates, .correct_volume_changes[m, , drop = F])
  })
}


.nfindr_cramer_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)
  
  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  Einv <- solve(.simplex(data, length(indices_best), indices_best))
  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_cramer(data, indices_best, 1:p, i, Einv=Einv)
      estimates <- as.numeric(estimates)
      if (any(estimates > 1+1.5e-8)) {
        # Update current simplex vertices
        j <- which.max(estimates)
        indices_best[j] <- i
        Einv <- solve(.simplex(data, length(indices_best), indices_best))
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

.nfindr_cramer_points <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)
  
  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  Einv <- solve(.simplex(data, length(indices_best), indices_best))

  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (j in 1:p) {
      estimates <- .estimate_volume_change_by_cramer(data, indices_best, j, 1:m, Einv=Einv)
      estimates <- as.numeric(estimates)
      if (any(estimates > 1+1.5e-8)) {
        # Update current simplex vertices
        i <- which.max(estimates)
        indices_best[j] <- i
        Einv <- solve(.simplex(data, length(indices_best), indices_best))
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

.nfindr_cramer_both <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)
  
  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  Einv <- solve(.simplex(data, length(indices_best), indices_best))

  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    
    estimates <- .estimate_volume_change_by_cramer(data, indices_best, 1:p, 1:m, Einv=Einv)
    if (any(estimates > 1+1.5e-8)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
      Einv <- solve(.simplex(data, length(indices_best), indices_best))
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