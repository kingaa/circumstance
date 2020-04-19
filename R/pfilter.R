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
##' @param data passed to \code{pomp::\link[pomp]{pfilter}}
##' @param Nrep number of replicate particle filter computations to run.
##' By default, \code{Nrep = 1}.
##' @param ... all additional arguments are passed to \code{pomp::\link[pomp]{pfilter}}
##'
##' @seealso \code{pomp::\link[pomp]{pfilter}}.
##'
##' @example tests/pfilter.R
##'
NULL

setGeneric(
  "pfilter",
  function (data, Nrep, ...)
    standardGeneric("pfilter")
)

##' @aliases pfilter
##' @aliases pfilter-ANY,numeric
##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "ANY", Nrep = "numeric"),
  definition = function (data, Nrep, ...) {
    foreach (iter_i=seq_len(Nrep),.combine=c) %dopar% {
      pomp::pfilter(data,...)
    } -> res
    names(res) <- seq_len(Nrep)
    res
  }
)

##' @aliases pfilter-ANY,missing
##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "ANY", Nrep = "missing"),
  definition = pomp::pfilter
)

##' @aliases pfilter-pompList,numeric
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
    if (is.null(names(data))) {
      names(res) <- sprintf("%d_%d",seq_len(npo),rep(seq_len(Nrep),each=npo))
    } else {
      names(res) <- sprintf("%s_%d",names(data),rep(seq_len(Nrep),each=npo))
    }
    res
  }
)

##' @aliases pfilter-pompList,missing
##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "pompList", Nrep = "missing"),
  definition = function (data, ...) {
    jobs <- seq_len(length(data))
    foreach (iter_i=jobs,.combine=c) %dopar% {
      pomp::pfilter(data[[iter_i]],...)
    } -> res
    if (is.null(names(data))) {
      names(res) <- jobs
    } else {
      names(res) <- names(data)
    }
    res
  }
)
