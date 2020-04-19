##' circumstance package
##'
##'
##' \pkg{circumstance} provides tools for parallelizing certain \pkg{pomp} calculations.
##'
##' @name circumstance-package
##' @import methods
##' @importFrom utils globalVariables
##'
NULL

## the following line quiets concerns of
## R CMD check regarding the foreach iterator variables
utils::globalVariables("iter_i")
