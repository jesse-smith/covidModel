define_proposal <- function(rw.sd, scale.start, shape.start) {
  pomp::mvn.rw.adaptive(
    rw.sd = rw.sd,
    scale.start = scale.start,
    shape.start = shape.start
  )
}

calc_Np <- function(k, k_start, k_end, Np_start, Np_end) {

  # Anything before k_start returns Np_start
  if (k <= k_start) return(Np_start)

  # Anything after k_end returns Np_end
  if (k >= k_end) return(Np_end)

  # Anything in between changes linearly with k from Np_start to Np_end
  as.integer(Np_start + (Np_end - Np_start) * (k - k_start) / (k_end - k_start))
}


define_initialization <- function() {
  pomp::Csnippet(

  )
}

define_step_fun <- function() {
  pomp::Csnippet(

  )
}

define_simulation <- function(dt = 1 / 24) {
  pomp::euler(
    step.fun = define_step_fun(),
    delta.t = dt
  )
}

define_evaluation <- function() {
  pomp::Csnippet(

  )
}

define_prior <- function() {
  pomp::Csnippet(

  )
}
