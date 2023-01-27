options(digits=3)
png(filename="pfilter-%02d.png",res=100)

set.seed(789744859)

library(circumstance)
library(tidyr)
library(dplyr)
library(ggplot2)
library(doFuture)
library(doRNG)

registerDoFuture()
registerDoRNG(789744859)

plan(sequential)

ou2() -> ou2
ou2 |>
  pfilter(Np=1000,Nrep=6) -> pfs

pfs |>
  logLik() |>
  logmeanexp(se=TRUE)

pfs |>
  as.data.frame() |>
  head()

ou2 |>
  pfilter(Np=1000) |>
  logLik()

pfs |>
  lapply(simulate) |>
  setNames(LETTERS[1:6]) |>
  conjoin() |>
  pfilter(Np=200) -> pfs2

pfs |>
  lapply(simulate) |>
  unname() |>
  conjoin() |>
  pfilter(Nrep=2,Np=200) -> pfs3

bind_rows(
  a=as.data.frame(pfs2),
  b=as.data.frame(pfs3),
  .id="list"
) |>
  separate(.L1,c("po","rep")) |>
  ggplot(aes(x=time,y=ess,group=rep,color=rep))+
  geom_line(alpha=0.5)+
  facet_wrap(~po)+
  theme_bw()

dev.off()
