##' @name nfindr
##' @rdname nfindr
##' @include nfindr.R
##' @export
nfindr.default <- function(
  x,
  p,
  init= c("random"),
  iter = c("points", "endmembers", "both"),
  estimator = c("Cramer", "volume", "height", "cofactor", "LDU"),
  iter_max = 10,
  n_init = 10,
  debug.level = 0,
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
  init <- tolower(match.arg(init))
  
  ## Parse init --------
  # Convert init into list where each element of the list is a set of initial indices
  if (is.numeric(init) && (length(init) == p)) {
    init <- list(init)
    if (debug.level>0) {
      warning("`n_init` is ignored since specific initial endmember indices were provided.")
    }
  } else if (init == "random") {
    init <- lapply(1:n_init, function(i) sample(m, p))
  } else {
    stop("Unexpected `init` value.")
  }

  ## Check number of outer-most iterations --------
  # for "both" type iteration increase the number of iteration
  # to approximately similar amount that "points" estimator would have
  if (iter == "both") {
    iter_max <- iter_max * p
  }

  # Check dimensions and number of endmembers ------
  if (n != p - 1) {
    estimator = "height"
    if (debug.level>0) {
      warning("Applying N-FINDR without dimension reduction. Note, `estimator` parameter will be ignored.")
    }
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
      nfindr_func(data, indices, iter_max = iter_max, debug.level = debug.level)
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

    if (debug.level > 0) {
      result[["iterations_count"]] <- sapply(results_list, function(r) r$iterations_count)
      result[["replacements_count"]] <- sapply(results_list, function(r) r$replacements_count)
    }
    if (debug.level > 1) {
      result[["replacements"]] <- sapply(results_list, function(r) r$replacements)
    }
  }

  result[["endmembers"]] <- data[result$indices,]
    
  class(result) <- "nfindr"
  
  return(result)
}
