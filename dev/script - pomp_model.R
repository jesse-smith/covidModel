proposal <- pomp::mvn.rw.adaptive(
  rw.sd = 0.1,
  scale.start = 100,
  shape.start = 100
)

Np <- function(k) 10L * round(1 + k * (1e4 - 10) / (10 * t))

params <- c()

rinit <- pomp::Csnippet(

)

rsim <- pomp::Csnippet(

)

rprocess <- pomp::euler(
  step.fun = rsim,
  delta.t = 1 / 24
)

dmeasure <- pomp::Csnippet(

)

dprior <- pomp::Csnippet(

)
