##' circumstance package
##'
##'
##' \pkg{circumstance} provides tools for parallelizing certain \pkg{pomp} calculations.
##'
##' @name circumstance-package
##' @aliases circumstance,package
##' @docType package
##' @import methods
##' @importFrom utils globalVariables
##'
NULL

## the following line quiets concerns of
## R CMD check regarding the foreach iterator variables
utils::globalVariables(c("iter_i"))

##' @importFrom future nbrOfWorkers
get_parinfo <- function () {
  list(
    workers=future::nbrOfWorkers()
  )
}
