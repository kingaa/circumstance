##' Parallel particle filter computations
##'
##' Runs multiple instances of \code{pfilter} using \code{foreach}.
##'
##' @name pfilter
##' @rdname pfilter
##' @importFrom pomp2 pfilter
##' @importFrom foreach foreach %dopar%
##' @include pompsi-package.R
##'
##' @inheritParams pomp2::pfilter
##'
##' @param Nrep number of replicate particle filter computations to run.
##'
NULL

setGeneric(
  "pfilter",
  function (data, Nrep, ...)
    standardGeneric("pfilter")
)
##' @aliases pfilter
##' @aliases pfilter-pomp,numeric
##' @rdname pfilter
##' @export
setMethod(
    "pfilter",
    signature=signature(data="pomp",Nrep="numeric"),
    definition = function(data, Nrep, ...) {
      foreach (i=seq_len(Nrep),.combine=c) %dopar% {
        pomp2::pfilter(data,...)
      }
    }
 )
