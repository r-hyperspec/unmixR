.get_invariants <- function(data, indices, endmembers = seq_along(indices)) {
  n_endmembers <- length(endmembers)
  
  # Calculate normal vectors
  normal_vectors <- sapply(
    endmembers,
    function(j) {
      indices_base <- indices[-j]
      MASS::Null(t(
        # Remove first row from all other rows to move from points to vectors
        scale(data[indices_base[-1], , drop = FALSE], center = data[indices_base[1], ], scale = FALSE)
      ))
    }
  )
  
  # Calculate heights of bases
  base_vertices <- rep(1, n_endmembers)
  base_vertices[endmembers == 1] <- 2
  h_base <- rowSums(data[indices[base_vertices], ] * t(normal_vectors))

  
  # Calculate heights of current vertices
  h_current <- rowSums(data[indices[endmembers], ] * t(normal_vectors))
  h_current <- abs(h_current - h_base)
    
  list(
    "normal_vectors" = normal_vectors,
    "h_base" = h_base,
    "h_current" = h_current
  )  
}

.estimate_volume_change_by_height <- function(data, indices, endmembers, new_indices, invariants=NULL) {
  n_points <- length(new_indices)
  n_endmembers <- length(endmembers)
  p <- length(indices)

  if (is.null(invariants)) {
    invariants <- .get_invariants(data, indices, endmembers)
  }
  normal_vectors <- invariants$normal_vectors
  h_base <- invariants$h_base
  h_current <- invariants$h_current
  
  H <- data[new_indices, ] %*% normal_vectors

  # Remove height of the base
  # The normal vector was calculated by moving from points to vectors
  # space, basically, by shifting all vertices. And the same shift was not
  # applied to the projected points. So, it might happen that the projection
  # of the base points to the normal vector are not 0. That is why here
  # we remove the height of the base
  # TODO: Comment the scaling
  if (n_endmembers > 1) {
    H <- scale(H, center = h_base, scale = h_current)
  } else {
    H <- scale(H, center = h_base, scale = FALSE)
  }

  abs(H)
}

.nfindr_height_endmembers <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  invariants <- .get_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    for (i in 1:m) {
      estimates <- .estimate_volume_change_by_height(
        data, indices_best, 1:p, i,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > 1+1.5e-8)) {
        # Update current simplex vertices
        j <- which.max(estimates)
        indices_best[j] <- i
        invariants <- .get_invariants(data, indices_best, 1:p)
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
      invariants <- .get_invariants(data, indices_best, j)
      estimates <- .estimate_volume_change_by_height(
        data, indices_best, j, 1:m,
        invariants = invariants
      )
      estimates <- as.numeric(estimates)
      if (any(estimates > invariants$h_current)) {
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

.nfindr_height_both <- function(data, indices, iter_max=10, debug.level=0) {
  p <- length(indices)
  m <- nrow(data)

  k <- 0
  n_replacements <- 0
  is_replacement <- TRUE
  indices_best <- indices
  if (debug.level > 1) {
    replacements <- matrix(indices_best, nrow=1)
  }
  invariants <- .get_invariants(data, indices, 1:p)

  while ((k < iter_max) && is_replacement) {
    k <- k + 1
    is_replacement <- FALSE

    estimates <- .estimate_volume_change_by_height(
      data, indices_best, 1:p, 1:m,
      invariants = invariants
    )
  
    if (any(estimates > 1+1.5e-8)) {
      # Update current simplex vertices
      max_ij <- arrayInd(which.max(estimates), dim(estimates))
      indices_best[max_ij[2]] <- max_ij[1]
      invariants <- .get_invariants(data, indices_best, 1:p)
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
