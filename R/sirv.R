#' Simulate a SIR Model with Vaccination and Variants
#'
#' `sirv()` simulates a SIR model with vaccinations, a variant type with
#' increased transmissibility, and a variant type with immunity resistance +
#' increased transmissibility. For simplicity, vaccine- and naturally-acquired
#' immunity (from non-vaccine-resistant variants) are considered equivalent.
#'
#' @param start `Date` or coercible to `Date`. The start date for simulation.
#'
#' @param end `Date` or coercible to `Date`. The end date for simulation.
#'
#' @param rt `numeric`.
#'   The observed reproduction number at the start of simulation;
#'   assumed constant other than population and variant effects.
#'
#' @param r0 `numeric`. The baseline reproduction number of the non-variant
#'   strains. If this is supplied, `rt` is ignored.
#'
#' @param pop `numeric`. The size of the population being modeled.
#'
#' @param incid `numeric`.
#'   The observed number of cases per day at the start of simulation.
#'
#' @param total_cases `numeric`.
#'   The observed number of total cases at the start of simulation.
#'
#' @param total_vac `numeric`.
#'   The number of vaccinated people at the start of simulation.
#'
#' @param vac_per_day `numeric`.
#'   The number of people vaccinated per day during simulation.
#'
#' @param pct_vac_s `numeric` between `0` and `1`.
#'   The fraction of vaccinated people vaccinated who have never had COVID
#'   (i.e. are in the susceptible, or "S", compartment).
#'
#' @param vac_eff `numeric` between `0` and `1`.
#'   Efficacy of vaccination and natural immunity for non-resistant variants.
#'
#' @param vac_eff_p1 `numeric` between `0` and `1`.
#'   Efficacy of vaccination and natural immunity for resistant variants.
#'
#' @param pct_uk `numeric` between `0` and `1`.
#'   Percent of cases that are B.1.1.7 (UK) variant.
#'
#' @param pct_p1 `numeric` between `0` and `1`.
#'   Percent of cases that are P.1 (Brazilian) variant.
#'
#' @param detect `numeric` between `0` and `1`.
#'   The percent of true cases detected; applies to both past and future cases.
#'
#' @return A `tibble` with columns
#'   `time <date>`, `S <dbl>`, `I <dbl>`, `R <dbl>`,
#'   `pct_uk <dbl>`, `pct_p1 <dbl>`
sirv <- function(
  start = Sys.Date(),
  end = Sys.Date() + 60L,
  rt = 1.07,
  r0 = NULL,
  pop = 937166,
  incid = 100,
  total_cases = 91350,
  detect = 1/3,
  total_vac = 238340,
  vac_per_day = 3500,
  pct_vac_s = 0.8,
  vac_eff = 0.9,
  vac_eff_p1 = 0.6,
  pct_uk = 0.2,
  pct_p1 = 0.001,
  rt_uk_coef = 1.5,
  rt_p1_coef = 1.5
) {

  # Mean Serial Interval is 5 days
  si <- 5

  # Calculate wild type reproduction number given `pct_uk` and observed `rt`
  pct_n <- 1 - pct_uk - pct_p1
  if (is.null(r0)) {
    r0 <- rt / (pct_n + pct_uk * rt_uk_coef + pct_p1 * rt_p1_coef)
  } else {
    rt <- r0 * (pct_n + pct_uk * rt_uk_coef + pct_p1 * rt_p1_coef)
  }

  # Calculate active cases
  active_cases <- (incid * si / rt)

  # Initial compartment values
  R <- (total_cases - active_cases) / detect + total_vac * pct_vac_s
  I <- active_cases / detect
  S <- pop - I - R

  cat(paste0(
    "\n",
    "Rn:  ", round(r0, 2), "\n",
    "Ruk: ", round(r0 * rt_uk_coef, 2), "\n",
    "Rp1: ", round(r0 * rt_p1_coef, 2), "\n",
    "Rt:  ", round(rt, 2),
    "\n"
  ))

  # Calculate effective initial susceptible
  S0 <- S + (1 - vac_eff)*(1 - pct_p1)*R + (1 - vac_eff_p1)*pct_p1*R

  # Calculate vaccination rate
  v_rate <- vac_per_day * pct_vac_s

  # Translate to model parameters
  beta_n <- (r0 / si) / S0
  beta_uk <- rt_uk_coef * beta_n
  beta_p1 <- rt_p1_coef * beta_n
  gamma  <- 1 / si

  # Pass initial values and parameters to deSolve
  y <- c(
    S = S,
    I = I,
    R = R,
    pct_uk = pct_uk,
    pct_p1 = pct_p1,
    reinf_n  = 0,
    reinf_p1 = 0,
    pct_R_imm_n  = 0,
    pct_R_imm_p1 = 0,
    i = incid / detect
  )

  params <- c(
    c(beta_n = beta_n, beta_uk = beta_uk, beta_p1 = beta_p1, gamma = gamma),
    c(pop = pop, v_rate = v_rate, vac_eff = vac_eff, vac_eff_p1 = vac_eff_p1)
  )

  # Create times for simulation
  dt <- 0.1
  t_span <- as.double(end - start)
  t <- seq(0, t_span, by = dt)

  # Simulate and convert to `tibble`
  result <- deSolve::ode(
    y = y,
    times = t,
    func = sirv_fn,
    parms = params
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(dplyr::near(.data[["time"]], round(.data[["time"]]))) %>%
    dplyr::mutate(
      time = start + .data[["time"]],
      dplyr::across(-"time", as.double),
      i_obs = .data[["i"]] * {{ detect }}
    ) %>%
    dplyr::select(-dplyr::contains(c("reinf_", "_R_imm_")))

  result
}

#' Workhorse Function for `sirv()`
#'
#' `sirv_fn()` updates derivatives for the \code{\link[covidModel:sirv]{sirv()}}
#' function. It is passed to the `func` parameter of
#' \code{\link[deSolve:ode]{ode()}}.
#'
#' @param t `numeric`. The current timestep.
#'
#' @param y `numeric`. A named vector of current states.
#'
#' @param params `numeric`. A named vector of input parameters.
#'
#' @param ... Additional arguments; currently unused.
#'
#' @return A `list` with containing a `numeric` vector of output derivatives for
#'   the state components
#'
#' @keywords internal
sirv_fn <- function(t, y, params, ...) {
  with(as.list(c(y, params)), {

    # When the number of active cases rounds to 0, the epidemic is over
    if (round(I) == 0) {
      return(list(c(
        dS = 0,
        dI = 0,
        dR = 0,
        dPuk = 0,
        dPp1 = 0,
        d_reinf_n  = 0,
        d_reinf_p1 = 0,
        d_pct_R_imm_n  = 0,
        d_pct_R_imm_p1 = 0,
        di = -i
      )))
    }

    # Save previous observed cases
    i_prev <- i

    # Ensure compartments are non-negative
    if (S < 0) S <- 0
    if (I < 0) I <- 0
    if (R < 0) R <- 0

    # Ensure percentages are non-negative
    if (pct_uk < 0) pct_uk <- 0
    if (pct_p1 < 0) pct_p1 <- 0

    # Ensure percentages add to 1
    if (pct_uk > 1) {
      pct_uk <- 1
      pct_p1 <- 0
    }
    if (pct_p1 > 1) {
      pct_p1 <- 1
      pct_uk <- 0
    }

    # Create synthetic I compartments for different variants
    Iuk <- pmax(I * pct_uk, 0)
    Ip1 <- pmax(I * pct_p1, 0)
    In  <- pmax(I - Iuk - Ip1, 0)

    # Translate into flows

    # S -> I
    is_n  <- pmax(S * In  * beta_n, 0)
    is_uk <- pmax(S * Iuk * beta_uk, 0)
    is_p1 <- pmax(S * Ip1 * beta_p1, 0)

    # R -> I
    Rn  <- pmax(R * (1 - pct_R_imm_p1 - pct_R_imm_n), 0)
    Rp1 <- pmax(R * (1 - pct_R_imm_p1), 0)
    ir_n  <- pmax(Rn  * In  * beta_n  * (1 - vac_eff), 0)
    ir_uk <- pmax(Rn  * Iuk * beta_uk * (1 - vac_eff), 0)
    ir_p1 <- pmax(Rp1 * Ip1 * beta_p1 * (1 - vac_eff_p1), 0)

    # * -> I
    i_n  <- pmax(is_n  + ir_n, 0)
    i_uk <- pmax(is_uk + ir_uk, 0)
    i_p1 <- pmax(is_p1 + ir_p1, 0)

    # I -> R
    r_n  <- pmax(In  * gamma, 0)
    r_uk <- pmax(Iuk * gamma, 0)
    r_p1 <- pmax(Ip1 * gamma, 0)

    # Update synthetic compartment derivatives
    dIn  <- i_n  - r_n
    dIuk <- i_uk - r_uk
    dIp1 <- i_p1 - r_p1

    # Handle negative synthetic compartment values
    if (In +  dIn  < 0) {
      dIn  <- -In
      r_n  <- pmax(i_n  - dIn, 0)
    }
    if (Iuk + dIuk < 0) {
      dIuk <- -Iuk
      r_uk <- pmax(i_uk - dIuk, 0)
    }
    if (Ip1 + dIp1 < 0) {
      dIp1 <- -Ip1
      r_p1 <- pmax(i_p1 - dIp1, 0)
    }

    # S -> R (vaccinated)
    v <- pmin(S, v_rate)

    # Total I's
    is <- pmax(is_n + is_uk + is_p1, 0)
    ir <- pmax(ir_n + ir_uk + ir_p1, 0)

    # Totals
    s <- 0
    i <- pmax(is + ir, 0)
    r <- pmax(r_n + r_uk + r_p1, 0)

    # Update compartment derivatives
    dS   <- s - is - v
    dI   <- i - r
    dR   <- r + v - ir

    # Handle negative compartment values
    if (S + dS < 0) {
      dS <- -S
    }
    if (I + dI < 0) {
      dI <- -I

    }
    if (R + dR < 0) {
      dR <- -R
    }
    if (Iuk + dIuk < 0) {
      dIuk <- -Iuk
    }
    if (Ip1 + dIp1 < 0) {
      dIp1 <- -Ip1
    }


    # Update reinfection derivatives
    d_reinf_n  <- pmax(ir_n + ir_uk, 0)
    d_reinf_p1 <- pmax(ir_p1, 0)

    # Update reinfection percentage derivatives
    dP_R_imm_n  <- pmax((reinf_n  + d_reinf_n)  * gamma / R, 0)
    dP_R_imm_p1 <- pmax((reinf_p1 + d_reinf_p1) * gamma / R, 0)

    # Update variant percentage derivatives
    dPuk <- (Iuk + dIuk) / (I + dI) - pct_uk
    dPp1 <- (Ip1 + dIp1) / (I + dI) - pct_p1

    # Update new cases derivative
    di <- i - i_prev

    if (i + di < 0) {
      di <- -i
    }

    list(c(
      dS,
      dI,
      dR,
      dPuk,
      dPp1,
      d_reinf_n,
      d_reinf_p1,
      dP_R_imm_n,
      dP_R_imm_p1,
      di
    ))
  })
}


