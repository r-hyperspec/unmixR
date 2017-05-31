##' Retrieve Endmembers
##'
##' Retrieves the endmembers from a unmixed dataset.
##'
##' @param object An object of class \code{nfindr}, \code{vca}, or \code{ice}
##'   from an unmixing analysis.
##'
##' @return A matrix where each row is an endmember as calculated by the
##'   unmixing algorithm.
##'
##' @export
##'
##' @seealso \code{\link{vca}}, \code{\link{ice}} and \code{\link{nfindr}} for examples.
##'
##' @include unmixR-package.R

endmembers <- function(object) {
	
	# N-FINDR & VCA return the same structure
	if ((class(object) == "nfindr") | (class(object) == "vca")) {
		d <- object[["data"]]
		i <- object[["indices"]]
		
		if (length(i) == nrow(d)) ans <- d # EMonly was TRUE when unmixing was done
		if (length(i) != nrow(d)) ans <- d[i,] # EMonly was FALSE when unmixing was done
		
		}

	# ICE returns a different structure
	if (class(object) == "ice") ans <- object$endmembers

	return(ans)
}

.test(endmembers) <- function() {
  context ("endmembers")
  
  data(demo_data, envir = environment())
    
  test_that ("Same endmember spectra extracted from N-FINDR & VCA", {
  	
  	# This assumes the correct indices were found.  A more specific test
  	# of this function would be to verify dimensions rather than contents.
  	# As written this accomplishes both checks.
  	
  	res1 <- endmembers(nfindr(demo_data, 2))
  	res2 <- endmembers(vca(demo_data, 2))
    expect_equal (res1, res2)
  })
  
  # Need to add a test for ice once ice is reviewed.
}
