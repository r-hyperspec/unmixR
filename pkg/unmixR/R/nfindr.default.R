#' Find initial endmember candidates by selecting extreme points along coordinate axes.
#' @param data matrix with coordinates in rows
#' @param p number of endmembers to select
#' @noRd
.init_extreme_coordinates <- function(data, p) {
  indices <- c()
  i <- 1
  while((length(indices)<p) && (i<=ncol(data))) {
    new_indices <- c(which.max(data[,i]), which.min(data[,i]))
    new_indices <- new_indices[!(new_indices %in% indices)]
    indices <- c(indices, new_indices)
    i <- i+1
  }
  
  return(list("indices"=indices[1:p]))
}

#' Find initial endmember candidates by projecting the data onto random vectors
#' and selecting the two extreme points.
#' @param data matrix with coordinates in rows
#' @param p number of endmembers to select
#' @noRd
.init_random_projections <- function(data, p) {
  indices <- c()
  m <- ncol(data)
  while(length(indices)<p) {
    w <- stats::rnorm(m, sd = 1)
    projections <- as.vector(data %*% w)
    new_indices <- c(which.max(projections), which.min(projections))
    new_indices <- new_indices[!(new_indices %in% indices)]
    indices <- c(indices, new_indices)
  }
  
  return(list("indices"=indices[1:p]))
}


#' @name nfindr
#' @rdname nfindr
#' @include nfindr.R
#' @export
nfindr.default <- function(
  x,
  p,
  init= c("projections", "random", "coordinates"),
  iter = c("points", "endmembers", "both"),
  estimator = c("Cramer", "volume", "height", "cofactor", "LDU"),
  iter_max = 10,
  n_init = 1,
  ...
  ) {

  m <- nrow(x)
  n <- ncol(x)
  
  # check for p being with the valid range, >= 2
  if (!is.numeric(p) || p < 2) {
    stop("p must be a positive integer >= 2")
  }

  ## Normalize string arguments -----
  iter <- tolower(match.arg(iter))
  estimator <- tolower(match.arg(estimator))
  if (is.character(init)) {
    init <- tolower(match.arg(init))
  }

  # Check dimensions and number of endmembers ------
  if (n != p - 1) {
    warning(
      "Applying N-FINDR without dimension reduction might be inefficient. ",
      "Consider reducing the dimensionality of the data first, e.g. with PCA."
    )
    if (estimator != "height") {
      warning("Note, `estimator` parameter is forced to 'height'.")
    }
    estimator = "height"
  }

  ## Parse init --------
  # Convert init into list where each element of the list is a set of initial indices
  if (is.numeric(init) && (length(init) == p)) {
    init <- list(init)
    if ( (.options("debuglevel") > 0L) && (n_init != 1L) ) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (init == "random") {
    init <- lapply(1:n_init, function(i) sample(m, p))
  } else if (init == "projections") {
    init <- lapply(1:n_init, function(i) .init_random_projections(x, p)$indices)
  } else if (init == "coordinates") {
    init <- lapply(1:n_init, function(i) .init_extreme_coordinates(x, p)$indices)
  } else {
    stop("Unexpected `init` value.")
  }

  ## Check number of outer-most iterations --------
  # for "both" type iteration increase the number of iteration
  # to approximately similar amount that "points" estimator would have
  if (iter == "both") {
    iter_max <- iter_max * p
  }

  ## Check the selected nfindr method --------
  nfindr_func <- get0(
    paste(".nfindr", estimator, iter, sep = "_"),
    mode = "function"
  )
  if (is.null(nfindr_func)) {
    stop("Invalid options iter and/or estimator parameters")
  }

  # transform the input into a matrix
  data <- as.matrix(x)

  ## Do N-FINDR -----
  results_list <- lapply(
    init,
    function(indices) {
      nfindr_func(data, indices, iter_max = iter_max)
    }
  )
  
  if (length(results_list) == 1) {
    result <- results_list[[1]]
    # sort the indices to normalize the order between runs
    result$indices <- sort(result$indices)
  } else {
    result <- list()
    
    # Remove duplicate results to avoid unnecessary volume calculations
    # sort the indices to normalize the order between runs
    unique_indices <- unique(t(
      sapply(results_list, function(r) sort(r$indices))
    ))
    
    if (nrow(unique_indices) == 1){
      result$indices <- unique_indices[1,]
    } else {
      volumes <- apply(unique_indices, 1, function(indices) simplex_volume(data, indices, factorial=FALSE))
      result$indices <- unique_indices[which.max(volumes),]
    }

    if (.options("debuglevel") > 0L) {
      result[["iterations_count"]] <- sapply(results_list, function(r) r$iterations_count)
      result[["replacements_count"]] <- sapply(results_list, function(r) r$replacements_count)
    }
    if (.options("debuglevel") > 1L) {
      result[["replacements"]] <- sapply(results_list, function(r) r$replacements)
    }
  }
    
  class(result) <- "nfindr"
  
  return(result)
}
