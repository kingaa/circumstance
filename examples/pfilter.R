library(circumstance)
library(doFuture)
library(doRNG)
registerDoFuture()
registerDoRNG()

ou2() -> ou2

plan(sequential)
system.time(ou2 |> pfilter(Np=10000,Nrep=6) -> pfs)

plan(multicore)
system.time(ou2 |> pfilter(Np=10000,Nrep=6) -> pfs)
