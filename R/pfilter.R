##' Parallel particle filter computations
##'
##' Runs multiple instances of \code{pfilter} using \code{foreach}.
##'
##' @name pfilter
##' @rdname pfilter
##' @importFrom pomp pfilter
##' @importFrom foreach foreach
##' @importFrom doFuture %dofuture%
##' @include package.R
##' @param data passed to \code{\link[pomp:pfilter]{pomp::pfilter}}
##' @param Nrep number of replicate particle filter computations to run.
##' By default, \code{Nrep = 1}.
##' @param ... all additional arguments are passed to \code{\link[pomp:pfilter]{pomp::pfilter}}
##' @param seed seed for the parallel random-number generator.
##' Setting \code{seed=TRUE} passes the current seed to the parallel RNG.
##' See \code{\link[doFuture]{\%dofuture\%}} for more information.
##' @param chunk.size average number of elements processed per chunk.
##' See \code{\link[doFuture]{\%dofuture\%}} for more information.
##' @param scheduling average number of chunks that each worker processes.
##' See \code{\link[doFuture]{\%dofuture\%}} for more information.
##' This is ignored unless \code{chunk.size=NULL}.
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
  definition = function (data, Nrep, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      seq_len(Nrep),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::pfilter(data,...)
    } -> res
    names(res) <- seq_len(Nrep)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "ANY", Nrep = "missing"),
  definition = function (data, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach(
      1L,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::pfilter(data,...)
    } -> res
    res[[1L]]
  }
)

##' @rdname pfilter
##' @export
setMethod(
  "pfilter",
  signature=signature(data = "pompList", Nrep = "numeric"),
  definition = function (data, Nrep, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    npo <- length(data)
    njobs <- Nrep*npo
    foreach (
      iter_i=seq_len(njobs),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      ipo <- (iter_i-1)%%npo+1
      rep <- (iter_i-1)%/%npo+1
      pomp::pfilter(data[[ipo]],...)
    } -> res
    nm <- names(data)
    if (is.null(nm)) nm <- seq_len(npo)
    names(res) <- sprintf("%s_%d",nm,rep(seq_len(Nrep),each=npo))
    attr(res,"parinfo") <- get_parinfo()
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
