##' Particle Markov chain Monte Carlo in parallel
##'
##' Runs multiple instances of \code{pmcmc} using \code{foreach}.
##'
##' @name pmcmc
##' @rdname pmcmc
##' @importFrom pomp pmcmc
##' @importFrom foreach foreach %dopar%
##' @include pfilter.R mif2.R
##'
##' @param data passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}
##' @param starts data frame containing parameters at which to begin iterated filtering
##' @param ... all additional arguments are passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}
##'
##' @seealso \code{\link[pomp:pmcmc]{pomp::pmcmc}}.
##'
NULL

setGeneric(
  "pmcmc",
  function (data, starts, ...)
    standardGeneric("pmcmc")
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "ANY", starts = "data.frame"),
  definition = function (data, starts, ...) {
    foreach (iter_i=seq_len(nrow(starts)),.combine=c) %dopar% {
      pomp::pmcmc(data,params=starts[iter_i,],...)
    } -> res
    names(res) <- row.names(starts)
    attr(res,"doPar") <- get_doPar_info()
    res
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "ANY", starts = "missing"),
  definition = function (data, ...) {
    pomp::pmcmc(data,...)
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "pompList", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"doPar") <- get_doPar_info()
    res
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "pfilterList", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"doPar") <- get_doPar_info()
    res
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "pmcmcList", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"doPar") <- get_doPar_info()
    res
  }
)

##' @rdname continue
##' @export
setMethod(
  "continue",
  signature=signature(object = "pmcmcList"),
  definition = function (object, ...) {
    foreach (iter_i=seq_along(object),.combine=c) %dopar% {
      pomp::continue(object[[iter_i]],...)
    } -> res
    names(res) <- names(object)
    attr(res,"doPar") <- get_doPar_info()
    res
  }
)
