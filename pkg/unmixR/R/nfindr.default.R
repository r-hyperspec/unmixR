#' @name nfindr
#' @rdname nfindr
#' @include nfindr.R
#' @export
nfindr.default <- function(
  x,
  p,
  init = c("projections", "random", "coordinates_sequence", "coordinates_random"),
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
    # Single numeric vector of indices
    init <- list(init)
    if ( (.options("debuglevel") > 0L) && (n_init != 1L) ) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (is.list(init)) {
    # List of numeric vectors (multiple initializations)
    if ( (.options("debuglevel") > 0L) && (n_init != length(init)) ) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (is.function(init)) {
    # Call the function n_init times
    init <- lapply(1:n_init, function(i) {
      result <- init(x, p)
      # Handle both direct indices and list with indices element
      if (is.list(result) && ("indices" %in% names(result)) ) {
        result$indices
      } else {
        result
      }
    })
  } else if (init == "random") {
    init <- lapply(1:n_init, function(i) sample(m, p))
  } else if (init == "projections") {
    init <- lapply(1:n_init, function(i) random_projections(x, p)$indices)
  } else if (init == "coordinates_sequence") {
    init <- lapply(1:n_init, function(i) extreme_coordinates(x, p, random = FALSE)$indices)
  } else if (init == "coordinates_random") {
    init <- lapply(1:n_init, function(i) extreme_coordinates(x, p, random = TRUE)$indices)
  } else {
    stop("Unexpected `init` value. Must be a string, numeric vector, list of numeric vectors, or function.")
  }

  stopifnot(
    is.list(init) && all(sapply(init, is.numeric)) && all(sapply(init, length) == p)
  )

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
  
  # Combine the results
  result <- list()
  
  # Remove duplicate results to avoid unnecessary volume calculations
  # sort the indices to normalize the order between runs
  unique_indices <- unique(t(
    sapply(results_list, function(r) sort(r$indices))
  ))
  
  if (nrow(unique_indices) == 1){
    # if there is only one unique result, return it
    result$indices <- unique_indices[1,]
  } else {
    # if there are multiple unique results, select the one with the largest volume
    volumes <- apply(unique_indices, 1, function(indices) simplex_volume(data, indices, factorial=FALSE))
    result$indices <- unique_indices[which.max(volumes),]
  }

  # Add counts for debugging
  if (.options("debuglevel") > 0L) {
    result[["iterations_count"]] <- sapply(results_list, function(r) r$iterations_count)
    result[["replacements_count"]] <- sapply(results_list, function(r) r$replacements_count)
  }

  # Add replacements for debugging
  if (.options("debuglevel") > 1L) {
    result[["replacements"]] <- lapply(results_list, function(r) r$replacements)
  }

  class(result) <- "nfindr"
  
  return(result)
}
