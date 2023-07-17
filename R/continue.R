##' Continue an iterative calculation
##'
##' Continue an iterative computation where it left off.
##'
##' @name continue
##' @importFrom pomp continue
##' @include package.R
##' @rdname continue
##' @param object the result of an iterative \pkg{pomp} computation
##' @param ... additional arguments will be passed to the underlying method.
##' This allows one to modify parameters used in the original computations.
##' @inheritParams pfilter
##' @seealso \code{\link{mif2}}
NULL

setGeneric(
  "continue",
  function (object, ...)
    standardGeneric("continue")
)
