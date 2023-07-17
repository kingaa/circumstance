##' Parallel iterated filtering
##'
##' Runs multiple instances of \code{mif2} using \code{foreach}.
##'
##' @name mif2
##' @rdname mif2
##' @importFrom pomp mif2
##' @importFrom foreach foreach
##' @importFrom doFuture %dofuture%
##' @include pfilter.R continue.R
##' @param data passed to \code{\link[pomp:mif2]{pomp::mif2}}
##' @param starts data frame containing parameters at which to begin iterated filtering
##' @param ... all additional arguments are passed to \code{\link[pomp:mif2]{pomp::mif2}}
##' @inheritParams pfilter
##' @seealso \code{\link[pomp:mif2]{pomp::mif2}}.
##'
NULL

setGeneric(
  "mif2",
  function (data, starts, ...)
    standardGeneric("mif2")
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "ANY", starts = "data.frame"),
  definition = function (data, starts, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      iter_i=seq_len(nrow(starts)),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::mif2(data,params=starts[iter_i,],...)
    } -> res
    names(res) <- row.names(starts)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "ANY", starts = "missing"),
  definition = function (data, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      1L,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::mif2(data,...)
    } -> res
    res[[1L]]
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "pompList", starts = "missing"),
  definition = function (data, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::mif2(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "pfilterList", starts = "missing"),
  definition = function (data, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::mif2(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "mif2List", starts = "missing"),
  definition = function (data, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      iter_i=seq_along(data),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::mif2(data[[iter_i]],...)
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
  signature=signature(object = "mif2List"),
  definition = function (object, ...,
    seed = TRUE, chunk.size = NULL, scheduling = 1
  ) {
    foreach (
      iter_i=seq_along(object),
      .combine=c,
      .options.future=list(
        seed=seed,
        chunk.size=chunk.size,
        scheduling=scheduling
      )
    ) %dofuture% {
      pomp::continue(object[[iter_i]],...)
    } -> res
    names(res) <- names(object)
    attr(res,"parinfo") <- get_parinfo()
    res
  }
)
