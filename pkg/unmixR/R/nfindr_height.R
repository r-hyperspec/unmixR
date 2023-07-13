.estimate_volume_change_by_height <- function(data, indices, endmembers, new_indices) {
  ratios <- vapply(
    endmembers,
    function(j) {
      base <- data[indices[-j],,drop=FALSE]
      
      # Calculate the null space, generalization of normal vector for an arbitrary dimension
      # e.g. for a plain triangle in 3D, a null space will be a 2D hyper plane orthogonal to the base
      N <- MASS::Null(t(
        # Remove first row from all other rows to move from points to vectors, i.e.
        # from [em1, em2, em3, ...] we switch to [(em2-em1), (em3-em1), ...]
        sweep(base, 2L, base[1,], check.margin=FALSE)[-1,,drop = FALSE]
      ))
      
      # Handle a special case, e.g. length(indices) == 2
      if (ncol(N) == 0) {
        N <- diag(1, nrow = ncol(data), ncol = ncol(data))
      }
      
      # Project points onto the null space and remove base offset of the base
      # Since the null space was calculated for [(em2-em1), (em3-em1), ...],
      # the projection of [em1, em2, em3, ...] is not zero and therefore has to be compensated
      # 
      # Alternatively, it is possible to do apply same shift before the projection,
      # i.e. `sweep(data[...], 2L, base[1,]) %*% N` but we chose to do it after the projection
      # because the projected data has smaller dimension.
      base_offset <- as.vector(base[1,] %*% N)
      proj <- data[new_indices, ] %*% N
      proj <- sweep(proj, 2L, base_offset)
      
      # Calculate height ratios
      # same as `apply(proj, 1, norm, type="2")` but works slightly faster
      heights <- sqrt(rowSums(proj*proj))
      if (indices[j] %in% new_indices) {
        height_current <- heights[new_indices == indices[j]]
      } else {
        height_current <- (data[indices[j],] %*% N) - base_offset
        height_current <- sqrt(sum(height_current*height_current))
      }
      
      # Return height ratios (which corresponds to volume ratios)
      return(heights / height_current)
    },
    FUN.VALUE = rep(NA_real_, length(new_indices))
  )
  
  # Force output to be a matrix.
  # If there is only one value in `endmembers`, then the output of vapply is a vector
  return(
    matrix(ratios,nrow = length(new_indices), ncol = length(endmembers))
  )
}


.test(.estimate_volume_change_by_height) <- function() {
  p <- ncol(.points_2d) + 1
  m <- nrow(.points_2d)
  indices <- 1:p
  # Volume of the current simplex
  V <- .correct_volume_changes[1, 1]
  
  test_that("Basic functionality", {
    # All rows, all endmembers
    estimates <- .estimate_volume_change_by_height(.points_2d, indices, 1:p, 1:m)
    expect_equal(dim(estimates), dim(.correct_volume_changes))
    expect_equal(V * estimates, .correct_volume_changes)
    
    # Single endmember replacement
    estimates <- .estimate_volume_change_by_height(.points_2d, indices, 1, 1:m)
    expect_equal(dim(estimates), c(m, 1))
    expect_equal(V * estimates, .correct_volume_changes[, 1, drop = F])
    
    # Single row replacement
    estimates <- .estimate_volume_change_by_height(.points_2d, indices, 1:p, m)
    expect_equal(dim(estimates), c(1, p))
    expect_equal(V * estimates, .correct_volume_changes[m, , drop = F])
  })
  
  # Convert 2d points to 3d by adding constant 3rd coordinate and rotating in 3d
  # These operations are not supposed to change the volume rations
  a <- 1; b <- 2; c <- 0.5
  .points_3d <- cbind(.points_2d, 1) %*%
    rbind(
      c(cos(a), -sin(a), 0),
      c(sin(a),  cos(a), 0),
      c(0,      0,       1)
    ) %*%
    rbind(
      c(cos(b),  0, sin(b)),
      c(0,       1,      0),
      c(-sin(b), 0, cos(b))
    ) %*%
    rbind(
      c(1, 0,           0),
      c(0, cos(c), sin(c)),
      c(0, sin(c), cos(c))
    )
  test_that("No dimension reduction", {
    # All rows, all endmembers
    estimates <- .estimate_volume_change_by_height(.points_3d, indices, 1:p, 1:m)
    expect_equal(dim(estimates), dim(.correct_volume_changes))
    expect_equal(V * estimates, .correct_volume_changes)
    
    # Single endmember replacement
    estimates <- .estimate_volume_change_by_height(.points_3d, indices, 1, 1:m)
    expect_equal(dim(estimates), c(m, 1))
    expect_equal(V * estimates, .correct_volume_changes[, 1, drop = F])
    
    # Single row replacement
    estimates <- .estimate_volume_change_by_height(.points_3d, indices, 1:p, m)
    expect_equal(dim(estimates), c(1, p))
    expect_equal(V * estimates, .correct_volume_changes[m, , drop = F])
  })
}


.nfindr_height_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
  warning("This combination of iterator and volume change estimator is not optimized. For better performance, it is recommended to change either of the two.")

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

    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_height(
        data, indices_best, 1:p, i
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > 1+1.5e-8)) {
        # Update current simplex vertices
        j <- which.max(estimates)
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

.nfindr_height_points <- function(data, indices, iter_max=10, debug.level=0) {
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
      estimates <- .estimate_volume_change_by_height(
        data, indices_best, j, 1:m
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > 1+1.5e-8)) {
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

.nfindr_height_both <- function(data, indices, iter_max=10, debug.level=0) {
  warning("This combination of iterator and volume change estimator is not optimized. For better performance, it is recommended to change either of the two.")
  
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

    estimates <- .estimate_volume_change_by_height(
      data, indices_best, 1:p, 1:m
    )
  
    if (any(estimates > 1+1.5e-8)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
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
