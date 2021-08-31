##' Estimate volume change (N-FINDR)
##'
##' See [estimate_volume_change()] with `volume` estimator for the details
##'
##' @noRd
.estimate_volume_change_by_volume <- function(data, indices, endmembers, new_indices) {
  n_points <- length(new_indices)
  n_endmembers <- length(endmembers)
  
  V <- matrix(NA_real_, nrow = n_points, ncol = n_endmembers)
  for (i in sequence(n_points)) {
    for (j in sequence(n_endmembers)) {
      candidate_indices <- indices
      candidate_indices[endmembers[j]] <- new_indices[i]
      V[i, j] <- simplex.volume(
        data,
        indices = candidate_indices, factorial = FALSE
      )
    }
  }
  
  V
}

.test(.estimate_volume_change_by_volume) <- function() {
  p <- ncol(.points_2d) + 1
  m <- nrow(.points_2d)
  indices <- 1:p
  
  test_that("Basic functionality", {
    # All rows, all endmembers
    estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1:p, 1:m)
    expect_equal(dim(estimates), dim(.correct_volume_changes))
    expect_equal(estimates, 2 * .correct_volume_changes)
    
    # Single endmember replacement
    estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1, 1:m)
    expect_equal(dim(estimates), c(m, 1))
    expect_equal(estimates, 2 * .correct_volume_changes[, 1, drop = F])
    
    # Single row replacement
    estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1:p, m)
    expect_equal(dim(estimates), c(1, p))
    expect_equal(estimates, 2 * .correct_volume_changes[m, , drop = F])
  })
}


.nfindr_volume_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
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

  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_volume(data, indices_best, 1:p, i)
      estimates <- as.numeric(estimates)
      if (any(estimates > volume_best)) {
          # Update current simplex vertices
          j <- which.max(estimates)
          indices_best[j] <- i
          volume_best <- estimates[j]
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

.nfindr_volume_points <- function(data, indices, iter_max, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)
  
  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  volume_best  <- simplex.volume(data, indices, factorial = FALSE)
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    for (j in 1:p) {
      estimates <- .estimate_volume_change_by_volume(data, indices_best, j, 1:m)
      estimates <- as.numeric(estimates)
      if (any(estimates > volume_best)) {
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



.nfindr_volume_both <- function(data, indices, iter_max, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)
  
  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  volume_best  <- simplex.volume(data, indices, factorial = FALSE)
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  
  while ((k<iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE
    
    estimates <- .estimate_volume_change_by_volume(data, indices_best, 1:p, 1:m)
    if (any(estimates > volume_best)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
      volume_best <- estimates[max_ij[1], max_ij[2]]
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

