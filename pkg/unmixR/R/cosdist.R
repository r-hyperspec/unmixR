##' cosine distance
##' 
##' computes the cosine distance matrix between the rows of \code{x}.
##' 
##' Cosine distance between two row vectors a and b is defined as
##' 
##' \deqn{cos \alpha = \frac{a \times b^T}{\left||a\right|| \cdot \left||b\right||}}{cos alpha = (a x t (b)) / (||a|| * ||b||)}
##' 
##' with the vector norms 
##' \deqn{\left||x\right|| = \sqrt{\sum x_i^2}}{||x|| = sqrt (sum (x[i]^2))}
##' 
##' @param x matrix (or object that can be coerced into matrix with \code{as.matrix (x)}
##' 
##' cosine distance is computed for the rows of \code{x}
##' @param ... ignored 
##' @return dist object: lower triangular matrix with pairwise distances
##' @seealso See e.g. 
##' 
##' Varmuza, K. and Filzmoser, P.: Introduction to multivariate statistical analysis in chemometrics, CRC Press, 2009, 
##' Chapter 2.4 Distances and Similarities.
##' 
##' Theodoridis, S. and Koutroumbas, K.: Pattern Recognition Academic Press, 2006,
##' Chapter 11.2.2 Proximity Measures between Two Points.
##' @author Claudia Beleites
##' @include unmixR-package.R

cos.dist <- function (x, ...){
  x <- as.matrix (x)              # fortify for all classes with as.matrix method
  
  D <- tcrossprod (x)
  
  len <- rowSums (x^2)            # squared length of each row vector
  len <- sqrt (outer (len, len))  # normalize by product of lengths
  
  D <- D / len
  
  as.dist (D)
} 

.test (cos.dist) <- function (){
  D.manual <- matrix (NA, nrow (.testdata), nrow (.testdata))
  for (a in seq_len (nrow (.testdata)))
    for (b in seq_len (nrow (.testdata))){
      xa <- .testdata$x [a,]
      xb <- .testdata$x [b,]  
      D.manual [a, b] <- t (xa) %*% xb / sqrt (sum (xa^2) * sum (xb^2))
    }
  
  checkEqualsNumeric (as.dist (D.manual), cos.dist (.testdata$x))
}