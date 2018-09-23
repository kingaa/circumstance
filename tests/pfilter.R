options(digits=3)

set.seed(789744859)

library(pompsi)
library(doParallel)
library(doRNG)

registerDoParallel()
registerDoRNG(789744859)

ou2() -> ou2
pfilter(ou2,Np=1000,Nrep=10) -> pfs
logmeanexp(logLik(pfs),se=TRUE)

registerDoSEQ()
