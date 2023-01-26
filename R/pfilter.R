##' Parallel particle filter computations
##'
##' Runs multiple instances of \code{pfilter} using \code{foreach}.
##'
##' @name pfilter
##' @rdname pfilter
##' @importFrom pomp pfilter
##' @importFrom foreach foreach %dopar%
##' @include package.R
##'
##' @param data passed to \code{\link[pomp:pfilter]{pomp::pfilter}}
##' @param Nrep number of replicate particle filter computations to run.
##' By default, \code{Nrep = 1}.
##' @param ... all additional arguments are passed to \code{\link[pomp:pfilter]{pomp::pfilter}}
##'
##' @seealso \code{\link[pomp:pfilter]{pomp::pfilter}}.
##'
##' @example examples/pfilter.R
##'
NULL

setGeneric(
  "pfilter",
  function (data, Nrep, ...)
    standardGeneric("pfilter")
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "ANY", Nrep = "numeric"),
  definition = function (data, Nrep, ...) {
    foreach (seq_len(Nrep),.combine=c) %dopar% {
      pomp::pfilter(data,...)
    } -> res
    names(res) <- seq_len(Nrep)
    res
  }
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "ANY", Nrep = "missing"),
  definition = pomp::pfilter
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "pompList", Nrep = "numeric"),
  definition = function (data, Nrep, ...) {
    npo <- length(data)
    njobs <- Nrep*npo
    foreach (iter_i=seq_len(njobs),.combine=c) %dopar% {
      ipo <- (iter_i-1)%%npo+1
      rep <- (iter_i-1)%/%npo+1
      pomp::pfilter(data[[ipo]],...)
    } -> res
    nm <- names(data)
    if (is.null(nm)) nm <- seq_len(npo)
    names(res) <- sprintf("%s_%d",nm,rep(seq_len(Nrep),each=npo))
    res
  }
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "pompList", Nrep = "missing"),
  definition = function (data, ...) {
    circumstance::pfilter(data,Nrep=1L,...)
  }
)
