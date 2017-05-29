
#' Volume of simplices
#'
#' @param data matrix with coordinates in rows
#' @param indices  indices of the  \code{ncol(data) + 1} vertex points. 
#' Defaults to all rows.
#' @param factorial logical indicating whether proper volumes 
#' (i.e. not only the determinant, but actually taking into account the 
#' proper 1 / n! prefactor) 
#'
#' @return volume of the simplex
#' @export
#'
#' @examples
#' data <- prcomp (laser)$x [, 1 : 2]
#' plot (data, pch = 19, col = matlab.dark.palette (nrow (data)))
#' lines (data [c (1, 84, 50, 1),])
#' 
#' simplex.volume (data, indices = c (1, 84, 50))
simplex.volume <- function (data, indices = seq_len (nrow (data)), factorial = TRUE){
  simplex <- .simplex (data = data, indices = indices, p = length (indices))
  V = abs (det (simplex)) 
  
  if (factorial)
    V <- V / factorial (length (indices) - 1)
  
  V
}

.test (simplex.volume) <- function (){
  
  context ("simplex.volume")
  data <- svd (.testdata$x, nv = 0)

  ## uncentered svd of .testdata$xhas column 1 constant
  data <- data$u [, -1] %*% diag (data$d [-1])
  data.3p <- data [.correct, ]

  ## data now has the axes aligned for direct calculation of triangle area
  area <- apply (data.3p, 2, range)
  area <- apply (area, 2, diff)
  area <- 1/2 * prod (area)

  test_that("correct volumes for triangle data", {
    expect_equal (simplex.volume (data, indices = .correct, factorial = TRUE), area)
    expect_equal (simplex.volume (data, indices = .correct, factorial = FALSE), area * 2)

    expect_equal (simplex.volume (data.3p, factorial = TRUE), area)
  })
}
