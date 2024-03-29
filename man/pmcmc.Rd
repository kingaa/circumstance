% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmcmc.R
\name{pmcmc}
\alias{pmcmc}
\alias{pmcmc,ANY,data.frame-method}
\alias{pmcmc,ANY,missing-method}
\alias{pmcmc,pompList,missing-method}
\alias{pmcmc,pfilterList,missing-method}
\alias{pmcmc,pmcmcList,missing-method}
\title{Particle Markov chain Monte Carlo in parallel}
\usage{
\S4method{pmcmc}{ANY,data.frame}(data, starts, ..., seed = TRUE, chunk.size = NULL, scheduling = 1)

\S4method{pmcmc}{ANY,missing}(data, ..., seed = TRUE, chunk.size = NULL, scheduling = 1)

\S4method{pmcmc}{pompList,missing}(data, ..., seed = TRUE, chunk.size = NULL, scheduling = 1)

\S4method{pmcmc}{pfilterList,missing}(data, ..., seed = TRUE, chunk.size = NULL, scheduling = 1)

\S4method{pmcmc}{pmcmcList,missing}(data, ..., seed = TRUE, chunk.size = NULL, scheduling = 1)
}
\arguments{
\item{data}{passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}}

\item{starts}{data frame containing parameters at which to begin iterated filtering}

\item{...}{all additional arguments are passed to \code{\link[pomp:pmcmc]{pomp::pmcmc}}}

\item{seed}{seed for the parallel random-number generator.
Setting \code{seed=TRUE} passes the current seed to the parallel RNG.
See \code{\link[doFuture]{\%dofuture\%}} for more information.}

\item{chunk.size}{average number of elements processed per chunk.
See \code{\link[doFuture]{\%dofuture\%}} for more information.}

\item{scheduling}{average number of chunks that each worker processes.
See \code{\link[doFuture]{\%dofuture\%}} for more information.
This is ignored unless \code{chunk.size=NULL}.}
}
\description{
Runs multiple instances of \code{pmcmc} using \code{foreach}.
}
\seealso{
\code{\link[pomp:pmcmc]{pomp::pmcmc}}.
}
