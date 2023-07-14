options(digits=3)
png(filename="pmcmc-%02d.png",res=100)

set.seed(432621217)

library(circumstance)
library(tidyr)
library(dplyr)
library(ggplot2)
library(doFuture)

plan(sequential)

ou2() -> ou2

coef(ou2) |> parmat(3) |> t() |> as.data.frame() -> starts
starts$alpha_3 <- c(-0.1,0.1,0.3)
starts$alpha_1 <- 0.6

ou2 |>
  window(end=10) |>
  pmcmc(
    starts=starts,
    Np=500,Nmcmc=100,
    proposal=mvn_diag_rw(rw.sd=c(alpha_1=0.001,alpha_3=0.001))
  ) -> pms

pms |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  plot()

pms |>
  as.data.frame() |>
  head()

pms |> logLik()

pms[[1]] |> pmcmc()

pms |>
  continue(Nmcmc=10) |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  plot()

pms |>
  pmcmc(Nmcmc=10) |>
  as.data.frame() |>
  head()

pms |>
  lapply(as_pomp) |>
  concat() |>
  pmcmc(
    Np=500,Nmcmc=20,
    proposal=mvn_diag_rw(rw.sd=c(alpha_1=0.001,alpha_3=0.001))
  ) |>
  traces(pars=c("alpha_1","alpha_3","loglik")) |>
  plot()

pms |>
  lapply(as,"pfilterd_pomp") |>
  concat() |>
  pmcmc(
    Nmcmc=100,
    proposal=mvn_diag_rw(rw.sd=c(alpha_1=0.001))
  ) |>
  traces(pars=c("alpha_1","loglik")) |>
  plot()

dev.off()
