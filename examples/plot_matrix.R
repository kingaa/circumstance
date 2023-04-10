\donttest{ # requires dplyr
  library(dplyr)

  data.frame(
    a=rexp(n=1000,rate=1/3),
    b=rnorm(1000)
  ) |>
    mutate(
      c=a+b^2,
      d=a-b^3
    ) -> x

  print(plot_matrix(x,alpha=0.2))

  g <- plot_matrix(
    x[-2],
    labels=c(
      expression(alpha),
      expression(beta),
      expression(phi)
    ),
    alpha=0.3
  )
  print(g)

  print(plot_matrix(as.list(x),alpha=0.2,breaks="scott"))
}
