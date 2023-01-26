##' Parallel iterated filtering
##'
##' Runs multiple instances of \code{mif2} using \code{foreach}.
##'
##' @name mif2
##' @rdname mif2
##' @importFrom pomp mif2
##' @importFrom foreach foreach %dopar%
##' @include pfilter.R
##'
##' @param data passed to \code{\link[pomp:mif2]{pomp::mif2}}
##' @param starts data frame containing parameters at which to begin iterated filtering
##' @param ... all additional arguments are passed to \code{\link[pomp:mif2]{pomp::mif2}}
##'
##' @seealso \code{\link[pomp:mif2]{pomp::mif2}}.
##'
NULL

setGeneric(
  "mif2",
  function (data, starts, ...)
    standardGeneric("mif2")
)

setGeneric(
  "continue",
  function (object, ...)
    standardGeneric("continue")
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "ANY", starts = "data.frame"),
  definition = function (data, starts, ...) {
    foreach (iter_i=seq_len(nrow(starts)),.combine=c) %dopar% {
      pomp::mif2(data,params=starts[iter_i,],...)
    } -> res
    names(res) <- row.names(starts)
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "ANY", starts = "missing"),
  definition = pomp::mif2
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "pompList", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::mif2(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "pfilterList", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::mif2(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    res
  }
)

##' @rdname mif2
##' @export
setMethod(
  "mif2",
  signature=signature(data = "mif2List", starts = "missing"),
  definition = function (data, ...) {
    foreach (iter_i=seq_along(data),.combine=c) %dopar% {
      pomp::mif2(data[[iter_i]],...)
    } -> res
    names(res) <- names(data)
    res
  }
)

##' @rdname continue
##' @export
setMethod(
  "continue",
  signature=signature(object = "mif2List"),
  definition = function (object, ...) {
    foreach (iter_i=seq_along(object),.combine=c) %dopar% {
      pomp::continue(object[[iter_i]],...)
    } -> res
    names(res) <- names(object)
    res
  }
)
