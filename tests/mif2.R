options(digits=3)
png(filename="mif2-%02d.png",res=100)

set.seed(432621217)

library(circumstance)
library(tidyr)
library(dplyr)
library(ggplot2)
library(doFuture)
library(doRNG)

registerDoFuture()
registerDoRNG(311289009)

plan(sequential)

ou2() -> ou2

coef(ou2) |> parmat(3) |> t() |> as.data.frame() -> starts
starts$alpha_3 <- c(-0.1,0.1,0.3)
starts$alpha_1 <- 0.6

ou2 |>
  mif2(
    starts=starts,
    Np=500,Nmif=10,
    rw.sd=rw_sd(alpha_3=0.01,alpha_1=0.01),
    cooling.fraction.50=0.1
  ) -> mfs

mfs |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  melt() |>
  mutate(iteration=as.integer(iteration)) |>
  ggplot(aes(x=iteration,y=value,group=.L1))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  theme_bw()

mfs |>
  as.data.frame() |>
  head()

mfs |> logLik()

mfs |>
  continue(Nmif=5) |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  melt() |>
  mutate(iteration=as.integer(iteration)) |>
  ggplot(aes(x=iteration,y=value,group=.L1))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  theme_bw()

mfs |>
  mif2(Nmif=5) |>
  as.data.frame() |>
  head()

mfs |>
  lapply(as_pomp) |>
  combine() |>
  mif2(
    Np=500,Nmif=3,
    rw.sd=rw_sd(alpha_3=0.01,alpha_1=0.01),
    cooling.fraction.50=0.01
  ) |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  melt() |>
  mutate(iteration=as.integer(iteration)) |>
  ggplot(aes(x=iteration,y=value,group=.L1))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  theme_bw()

mfs |>
  lapply(as,"pfilterd_pomp") |>
  combine() |>
  mif2(
    Np=500,Nmif=3,
    rw.sd=rw_sd(alpha_3=0.01,alpha_1=0.01),
    cooling.fraction.50=0.01
  ) |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  melt() |>
  mutate(iteration=as.integer(iteration)) |>
  ggplot(aes(x=iteration,y=value,group=.L1))+
  geom_line()+
  facet_grid(variable~.,scales="free_y")+
  theme_bw()

dev.off()
