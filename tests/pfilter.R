options(digits=3)
png(filename="pfilter-%02d.png",res=100)

set.seed(789744859)

library(circumstance)
library(tidyr)
library(ggplot2)
library(doParallel)
library(doRNG)

registerDoParallel()
registerDoRNG(789744859)

ou2() -> ou2
ou2 %>%
  pfilter(Np=1000,Nrep=6) -> pfs

pfs %>%
  logLik() %>%
  logmeanexp(se=TRUE)

pfs %>%
  as.data.frame() %>%
  head()

ou2 %>%
  pfilter(Np=1000) %>%
  logLik()

pfs %>%
  lapply(simulate) %>%
  setNames(LETTERS[1:6]) %>%
  do.call(c,.) %>%
  pfilter(Nrep=2,Np=200) -> pfs2

pfs2 %>%
  data.frame() %>%
  separate(.id,c("po","rep")) %>%
  ggplot(aes(x=time,y=ess,group=rep,color=rep))+
  geom_line()+
  facet_wrap(~po)

registerDoSEQ()

dev.off()
