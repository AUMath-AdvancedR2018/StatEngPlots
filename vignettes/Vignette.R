## ---- echo = FALSE, fig.width = 7, fig.height = 5------------------------
library("StatEngPlots")

abc <- rweibull(1000,4)
ppcc(abc)


## ---- echo = FALSE, fig.width = 7, fig.height = 5------------------------

a <- rnorm(100,0,1)
sdplot(a,divide=FALSE,nr=10)


## ---- echo = FALSE, fig.width = 7, fig.height = 5------------------------

b <- rpois(100,3)
meanplot(b,divide=FALSE,nr=10)


