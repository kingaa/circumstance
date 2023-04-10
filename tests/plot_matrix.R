library(dplyr)
library(grid)
library(circumstance)

png(filename="plot_matrix-%01d.png",res=100)

x <- data.frame(a=rexp(n=1000,rate=1/3),b=rnorm(1000))
mutate(x,c=a+b^2,d=a-b^3) -> x

plot_matrix(x,alpha=0.2)

plot_matrix(
  x[-2],
  labels=c(
    expression(alpha),
    expression(beta),
    expression(phi)
  ),
  alpha=0.3
)

(g <- plot_matrix(x[1]))

g <- plot_matrix(as.list(x),alpha=0.2,breaks='scott')
print(g)

try(plot_matrix(numeric(10)))

print(g,vp=plotViewport(c(1,1,10,10)),newpage=T)
print(g,vp=viewport(x=0.8,y=0.8,width=0.4,height=0.4))

dev.off()

try(plot_matrix(LETTERS))
