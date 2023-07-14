##' Continue an iterative calculation
##'
##' Continue an iterative computation where it left off.
##'
##' @name continue
##' @aliases continue,missing-method continue,ANY-method
##' @include package.R
##' @rdname continue
##' @param object the result of an iterative \pkg{pomp} computation
##' @param ... additional arguments will be passed to the underlying method.
##' This allows one to modify parameters used in the original computations.
##' @inheritParams pfilter
##' @seealso \code{\link{mif2}}
NULL

##' @name continue
##' @rdname continue
##' @export
setGeneric(
  "continue",
  function (object, ...)
    standardGeneric("continue")
)

setMethod(
  "continue",
  signature=signature(object="missing"),
  definition=function (...) {
    reqd_arg("continue","object")
  }
)

setMethod(
  "continue",
  signature=signature(object="ANY"),
  definition=function (object, ...) {
    undef_method("continue",object)
  }
)
