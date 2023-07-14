##' Particle Markov chain Monte Carlo in parallel
##'
##' Runs multiple instances of \code{pmcmc} using \code{foreach}.
##'
##' @name pmcmc
##' @rdname pmcmc
##' @importFrom pomp pmcmc
##' @importFrom foreach foreach
##' @importFrom doFuture %dofuture%
##' @include pfilter.R mif2.R
##' @param data passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}
##' @param starts data frame containing parameters at which to begin iterated filtering
##' @param ... all additional arguments are passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}
##' @inheritParams pfilter
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
  definition = function (data, starts, ...,
    .options.future = list(seed = TRUE)
  ) {
    foreach (
      iter_i=seq_len(nrow(starts)),
      .combine=c,
      .options.future=.options.future
    ) %dofuture% {
      pomp::pmcmc(data,params=starts[iter_i,],...)
    } -> res
    names(res) <- row.names(starts)
    attr(res,"parinfo") <- get_parinfo()
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
  definition = function (data, ...,
    .options.future = list(seed = TRUE)
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=.options.future
    ) %dofuture% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "pfilterList", starts = "missing"),
  definition = function (data, ...,
    .options.future = list(seed = TRUE)
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=.options.future
    ) %dofuture% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname pmcmc
##' @export
setMethod(
  "pmcmc",
  signature=signature(data = "pmcmcList", starts = "missing"),
  definition = function (data, ...,
    .options.future = list(seed = TRUE)
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=.options.future
    ) %dofuture% {
      pomp::pmcmc(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname continue
##' @export
setMethod(
  "continue",
  signature=signature(object = "pmcmcList"),
  definition = function (object, ...,
    .options.future = list(seed = TRUE)
  ) {
    foreach (
      iter_i=seq_along(object),
      .combine=c,
      .options.future=.options.future
    ) %dofuture% {
      pomp::continue(object[[iter_i]],...)
    } -> res
    names(res) <- names(object)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)
