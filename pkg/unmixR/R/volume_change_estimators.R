# ##' Estimate volume change (N-FINDR)
# ##'
# ##' See [estimate_volume_change()] with `volume` estimator for the details
# ##'
# ##' @noRd
# .estimate_volume_change_by_volume <- function(data, indices, endmembers, new_indices) {
#   n_points <- length(new_indices)
#   n_endmembers <- length(endmembers)
# 
#   V <- matrix(NA_real_, nrow = n_points, ncol = n_endmembers)
#   for (i in sequence(n_points)) {
#     for (j in sequence(n_endmembers)) {
#       candidate_indices <- indices
#       candidate_indices[endmembers[j]] <- new_indices[i]
#       V[i, j] <- simplex.volume(
#         data,
#         indices = candidate_indices, factorial = FALSE
#       )
#     }
#   }
# 
#   V
# }
# 
# .test(.estimate_volume_change_by_volume) <- function() {
#   p <- ncol(.points_2d) + 1
#   m <- nrow(.points_2d)
#   indices <- 1:p
#   
#   test_that("Basic functionality", {
#     # All rows, all endmembers
#     estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1:p, 1:m)
#     expect_equal(dim(estimates), dim(.correct_volume_changes))
#     expect_equal(estimates, 2 * .correct_volume_changes)
# 
#     # Single endmember replacement
#     estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1, 1:m)
#     expect_equal(dim(estimates), c(m, 1))
#     expect_equal(estimates, 2 * .correct_volume_changes[, 1, drop = F])
# 
#     # Single row replacement
#     estimates <- .estimate_volume_change_by_volume(.points_2d, indices, 1:p, m)
#     expect_equal(dim(estimates), c(1, p))
#     expect_equal(estimates, 2 * .correct_volume_changes[m, , drop = F])
#   })
# }

# ##' Estimate volume change (N-FINDR)
# ##'
# ##' See [estimate_volume_change()] with `Cramer` estimator for the details
# ##'
# ##' @noRd
# .estimate_volume_change_by_cramer <- function(data, indices, endmembers, new_indices, Einv = NULL) {
#   if (is.null(Einv)) {
#     p <- length(indices)
#     E <- .simplex(data, length(indices), indices)
#     Einv <- solve(E)
#   }
# 
# 
#   # Same as: ratios <- Einv %*% .simplex_E(data, new_indices)
#   # Here we use the fact that the first row in `.simplex_E` would be 1s,
#   # so we avoid creation of a new matrix `.simplex_E`
#   ratios <-
#     Einv[endmembers, 1] +
#     tcrossprod(Einv[endmembers, -1], data[new_indices, , drop = F])
# 
#   t(abs(ratios))
# }
# 
# .test(.estimate_volume_change_by_cramer) <- function() {
#   p <- ncol(.points_2d) + 1
#   m <- nrow(.points_2d)
#   indices <- 1:p
#   # Volume of the current simplex
#   V <- .correct_volume_changes[1, 1]
#   
#   test_that("Basic functionality", {
#     # All rows, all endmembers
#     estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1:p, 1:m)
#     expect_equal(dim(estimates), dim(.correct_volume_changes))
#     expect_equal(V * estimates, .correct_volume_changes)
# 
#     # Single endmember replacement
#     estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1, 1:m)
#     expect_equal(dim(estimates), c(m, 1))
#     expect_equal(V * estimates, .correct_volume_changes[, 1, drop = F])
# 
#     # Single row replacement
#     estimates <- .estimate_volume_change_by_cramer(.points_2d, indices, 1:p, m)
#     expect_equal(dim(estimates), c(1, p))
#     expect_equal(V * estimates, .correct_volume_changes[m, , drop = F])
#   })
#   
#   #TODO: Test invariant
# }


# # TODO: Multiply endmembers WARNING
# .estimate_volume_change_by_height <- function(data, indices, endmembers, new_indices, normal_vectors = NULL) {
#   n_points <- length(new_indices)
#   n_endmembers <- length(endmembers)
#   p <- length(indices)
# 
#   if (is.null(normal_vectors)) {
#     normal_vectors <- sapply(
#       endmembers,
#       function(j) {
#         indices_base <- indices[-j]
#         MASS::Null(t(
#           # Remove first row from all other rows to move from points to vectors
#           scale(data[indices_base[-1], , drop=FALSE], center = data[indices_base[1],], scale = FALSE)
#         ))
#       }
#     )
#   } else {
#     if (!is.vector(normal_vectors)) {
#       normal_vectors <- matrix(normal_vectors, ncol = 1)
#     }
#   }
# 
#   H <- matrix(NA_real_, nrow = n_points, ncol = n_endmembers)
#   for (j in sequence(n_endmembers)) {
#     heights <- data[new_indices, ] %*% normal_vectors[, j, drop = F]
#     # Remove height of the base
#     # The normal vector was calculated by moving from points to vectors
#     # space, basically, by shifting all vertices. And the same shift was not
#     # applied to the projected points. So, it might happen that the projection
#     # of the base points to the normal vector are not 0. That is why here
#     # we remove the height of the base:
#     # `indices[-endmembers[j]]` - are base points, `indices[-endmembers[j]][1]` is 
#     # the first base point. In fact, it does not matter which base point to take
#     # since the all have the same height.
#     H[, j] <- heights - heights[indices[-endmembers[j]][1]]
#   }
# 
#   abs(H)
# }
# 
# .test(.estimate_volume_change_by_height) <- function() {
#   p <- ncol(.points_2d) + 1
#   m <- nrow(.points_2d)
#   indices <- 1:p
#   normal_vectors <- sapply(1:p, function(j){
#     base <- .points_2d[indices[-j],]
#     base_vector <- base[2,] - base[1,]
#     c(base_vector[2], -base_vector[1]) / sqrt(sum(base_vector^2))
#   })
#   base_lengths <- sapply(1:p, function(j){
#     base <- .points_2d[indices[-j],]
#     base_vector <- base[2,] - base[1,]
#     sqrt(sum(base_vector^2))
#   })
# 
#   test_that("Basic functionality", {
#     # Single endmember replacement
#     for(j in 1:p) {
#       estimates <- .estimate_volume_change_by_height(.points_2d, indices, j, 1:m)
#       expect_equal(dim(estimates), c(m, 1))
#       expect_equal(base_lengths[j]*estimates, 2*.correct_volume_changes[, j, drop = F])
#       expect_equal(estimates, .points_2d %*% normal_vectors[,j,drop=F])
#     }
#     # TODO:
#     # All rows, all endmembers
#     # Single row replacement
#   })
#   
#   #TODO: Test invariant
# }


# TODO: Values are not comparable between different EM replacements
# Fails on: 
# > .points_2d[c(2,1,3),]
# [,1] [,2]
# [1,]    3    0
# [2,]    0    0
# [3,]    0    4
# > nfindrLDU(data = .points_2d, indices = c(2,1,3), p=3)
# > nfindrSeqLDU(data = .points_2d, indices = c(2,1,3), p=3)
# .estimate_volume_change_by_ldu <- function(data, indices, endmembers, new_indices, G = NULL) {
#   n_points <- length(new_indices)
#   n_endmembers <- length(endmembers)
#   p <- length(indices)
#   n <- ncol(data)
# 
#   if (is.null(G)) {
#     E <- .simplex(data, p, indices)
#     G <- sapply(endmembers, function(j) {
#       A <- E[-p, -j, drop=FALSE]
#       c <- E[p, -j, drop=FALSE]
#       crossprod(c, solve(A))
#     })
#   }
# 
#   V <- matrix(NA_real_, nrow = n_points, ncol = n_endmembers)
#   for (j in sequence(n_endmembers)) {
#     g <- G[, endmembers[j], drop = F]
#     for (i in sequence(n_points)) {
#       d <- data[new_indices, n]
#       # Same as:
#       # B <- rbind(rep(1, n_points), t(data[new_indices, -n]))
#       # V[,j] <- d - g %*% B
#       Bt <- data[new_indices, -n]
#       V[, j] <- d - g[1, 1] - Bt %*% g[-1, , drop = F]
#     }
#   }
# 
#   abs(V)
# }


##' Estimate volume change (N-FINDR)
##'
##' Estimates how the volume of the provided simplex (`data`+`indices`) will
##'   change by replacing endmembers in `endmembers` positions by new points
##'   (new vertex candidates provided by `data`+`new_indices`).
##'
##' @details
##' The estimates are returned in a form of a matrix with `length(new_indices)`
##'   rows and `length(endmembers)` columns. Where an element in i-th  row and
##'   j-th column contains an estimation of the simplex volume change if the
##'   `endmembers[j]`-th vertex of the simplex would be replaced by
##'   `new_indices[i]`-th row in `data`.
##' 
##' TODO: Add details for each estimator
##'
##' @note Comparison between estimated volume changes should be done carefully
##'   since is some cases (depending on the chosen estimator) values between
##'   different rows/columns are incomparable and require additional computation
##'   to make all values comparable
##'
##' @param data Matrix of possible vertex points in rows
##'
##' @param indices Row indices in `data` of the simplex vertices
##'
##' @param endmembers Indices of vertices to be replaced
##'
##' @param new_indices Row indices in `data` of new points to replace the vertex
##'
##' @param estimator Volume change estimation algorithm. Must be one of
##'   * `volume` - exact volume (`1/(p-1)!` is ignored, since it is the same for all volumes)
##'   * `height` - height over the base
##'   * `Cramer` - volumes ratio (\{V_new / V_old}) calculated using Cramer's rule
##'
##' @return A matrix of `length(new_indices)`x`length(endmembers)` which element
##'   in i-th row and j-th column contains an estimation of the simplex volume
##'   change if the `endmembers[j]`-th vertex of the simplex would be replaced
##'   by `new_indices[i]`-th row in `data`.
##'   
##' @references 
##' TODO: Add references
##' 
##' @rdname nfindr
estimate_volume_change <- function(data, indices, endmembers = 1:length(indices), new_indices = 1:nrow(data), estimator = c("volume", "height", "Cramer")) {
  estimator <- match.arg(estimator)
  estimator_method <- get0(paste0(".estimate_volume_change_by_", tolower(estimator)), mode = "function")
  estimator_method(data, indices, endmembers, new_indices)
}

.test(estimate_volume_change) <- function() {
  p <- ncol(.points_2d) + 1
  m <- nrow(.points_2d)
  indices <- 1:p
  
  test_that("By volume", {
    expect_equal(
      estimate_volume_change(.points_2d, indices, estimator = "volume"),
      .estimate_volume_change_by_volume(.points_2d, indices, 1:p, 1:m)
    )
    expect_equal(
      estimate_volume_change(.points_2d, indices, 1:p, 1:m, estimator = "volume"),
      .estimate_volume_change_by_volume(.points_2d, indices, 1:p, 1:m)
    )
    expect_equal(
      estimate_volume_change(.points_2d, indices, endmembers=1, estimator = "volume"),
      .estimate_volume_change_by_volume(.points_2d, indices, 1, 1:m)
    )
    expect_equal(
      estimate_volume_change(.points_2d, indices, new_indices=1, estimator = "volume"),
      .estimate_volume_change_by_volume(.points_2d, indices, 1:p, 1)
    )
  })
  
  test_that("By Cramer's rule", {
    expect_equal(
      estimate_volume_change(.points_2d, indices, estimator = "Cramer"),
      .estimate_volume_change_by_cramer(.points_2d, indices, 1:p, 1:m)
    )
  })

  test_that("By height", {
    expect_equal(
      estimate_volume_change(.points_2d, indices, endmembers=1, estimator = "height"),
      .estimate_volume_change_by_height(.points_2d, indices, 1, 1:m)
    )
  })
}