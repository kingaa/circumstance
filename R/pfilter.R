##' Parallel particle filter computations
##'
##' Runs multiple instances of \code{pfilter} using \code{foreach}.
##'
##' @name pfilter
##' @rdname pfilter
##' @importFrom pomp pfilter
##' @importFrom foreach foreach %dopar%
##' @include circumstance-package.R
##'
##' @param Nrep number of replicate particle filter computations to run.
##' @param ... all additional arguments are passed to \code{pomp::\link[pomp]{pfilter}}
##'
##' @seealso \code{pomp::\link[pomp]{pfilter}}.
##'
##' @example tests/pfilter.R
##'
NULL

setGeneric(
  "pfilter",
  function (Nrep, ...)
    standardGeneric("pfilter")
)

##' @aliases pfilter
##' @aliases pfilter-pomp,numeric
##' @rdname pfilter
##' @export
setMethod(
    "pfilter",
    signature=signature(Nrep="numeric"),
    definition = function(Nrep, ...) {
      foreach (i=seq_len(Nrep),.combine=c) %dopar% {
        pomp::pfilter(...)
      }
    }
 )
