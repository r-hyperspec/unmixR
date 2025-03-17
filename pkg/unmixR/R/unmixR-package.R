#' Functions implementing the N-FINDR, Iterated Constrained
#' Endmembers (ICE), Vertex Component Analysis (VCA), and Automatic Target
#' Generation Procedure (ATGP) algorithms which can recover pure component
#' spectra and their respective concentrations from a hyperspectral data set.
#'
#' @section Acknowledgement:
#' The development of \pkg{unmixR} was supported by Google Summer of Code in
#' 2013 (Conor McManus, v0.1) and 2016 (Anton Belov, v0.2).  The support of
#' Google is greatly appreciated.
#' This project has received funding from the European Union’s Horizon 2020 research
#' and innovation programme under the Marie Sklodowska-Curie Actions (Grant Agreement 861122)
#' as part of IMAGE-IN project.
#'
#' @name unmixR-package
#' @title Hyperspectral Unmixing Methods
#' @docType package
#'
#' @author Anton Belov, Conor McManus, Claudia Beleites, Bryan A. Hanson, Simon Fuller, Rustam Guliev.
#'
#' Maintainer: Claudia Beleites <chemometrie@beleites.de>
#'
#' @rdname unmixR-package
#' @aliases unmixR
#' @keywords package
#' @keywords hyperspectral
#' @import hyperSpec
#' @import nnls
#' @import MASS

# This code needs to be here due to the order in which
# things are sourced, apparently.  It made more sense to
# me to put it unittests.R but that causes problems.  BH.
	
{
  `.test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
  }
} 
