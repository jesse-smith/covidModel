#' Simulate a SIR Model with Vaccination and Variants
#'
#' `sirv()` simulates a SIR model with vaccinations, a variant type with
#' increased transmissibility, and a variant type with immunity resistance. For
#' simplicity, vaccine- and naturally-acquired immunity are considered
#' equivalent.
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
#' @param active_cases `numeric`.
#'   The observed number of active cases at the start of simulation.
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
#' @param vacc_eff_ek `numeric` between `0` and `1`.
#'   Efficacy of vaccination and natural immunity for resistant variants
#'   (with the E484K mutation).
#'
#' @param pct_var `numeric` between `0` and `1`.
#'   Percent of active cases that are variants of some form.
#'
#' @param pct_var_uk `numeric` between `0` and `1`.
#'   Percent of variants that are B.1.1.7 (UK) variant or similarly highly
#'   transmissible. Assumed to not overlap with `pct_var_ek`.
#'
#' @param pct_var_ek `numeric` between `0` and `1`.
#'   Percent of variants that possess the E484K mutation (vaccine resistance).
#'   Assumed to not overlap with `pct_var_uk`.
#'
#' @param detect `numeric` between `0` and `1`.
#'   The percent of true cases detected; applies to both past and future cases.
#'
#' @return A `tibble` with columns
#'   `time <date>`, `S <dbl>`, `I <dbl>`, `R <dbl>`,
#'   `pct_uk <dbl>`, `pct_ek <dbl>`
sirv <- function(
  start = Sys.Date(),
  end = Sys.Date() + 60L,
  rt = 1,
  r0 = NULL,
  pop = 937166,
  active_cases = 500,
  total_cases = 91350,
  total_vac = 222550,
  vac_per_day = 3500,
  pct_vac_s = 0.8,
  vac_eff = 0.9,
  vac_eff_ek = 0.6,
  pct_var = 0.1 / 0.85,
  pct_var_uk = 0.85,
  pct_var_ek = 0,
  detect = 1/3
) {

  is_scalar_numeric <- function(x) {
    rlang::is_scalar_integer(x) || rlang::is_scalar_double(x)
  }

  # Check inputs
  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)
  coviData::assert_all(start <= end)
  coviData::assert_all(is_scalar_numeric(rt), 0 <= rt)
  coviData::assert_all(is_scalar_numeric(pop), 0 <= pop)
  coviData::assert_all(
    is_scalar_numeric(active_cases),
    0 <= active_cases
  )
  coviData::assert_all(
    is_scalar_numeric(total_cases),
    0 <= total_cases,
    active_cases <= total_cases
  )
  coviData::assert_all(
    is_scalar_numeric(total_vac),
    0 <= total_vac,
    total_vac <= pop
  )
  coviData::assert_all(
    is_scalar_numeric(vac_per_day),
    0 <= vac_per_day,
    vac_per_day <= pop
  )
  coviData::assert_all(
    is_scalar_numeric(pct_vac_s),
    0 <= pct_vac_s,
    pct_vac_s <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(vac_eff),
    0 <= vac_eff,
    vac_eff <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(vac_eff_ek),
    0 <= vac_eff_ek,
    vac_eff_ek <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(pct_var),
    0 <= pct_var,
    pct_var <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(pct_var_uk),
    0 <= pct_var_uk,
    pct_var_uk <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(pct_var_ek),
    0 <= pct_var_ek,
    pct_var_ek <= 1
  )
  coviData::assert_all(
    is_scalar_numeric(detect),
    0 < detect,
    detect <= 1,
    active_cases / detect <= pop,
    total_cases  / detect <= pop
  )

  coviData::assert_all((pct_vac_s * total_vac) + (total_cases / detect) <= pop)
  coviData::assert_all(pct_var_uk + pct_var_ek <= 1)


  # Calculate absolute percentages for variants
  pct_uk <- pct_var * pct_var_uk
  pct_ek <- pct_var * pct_var_ek

  # Initial compartment values
  R <- (total_cases - active_cases) / detect + total_vac * pct_vac_s
  I <- active_cases / detect
  S <- pop - I - R

  # Calculate wild type reproduction number given `pct_uk` and observed `rt`
  if (is.null(r0)) {
    r0 <- rt / (1 + 0.5 * pct_uk)
  } else {
    rt <- r0 * (1 + 0.5 * pct_uk)
  }

  cat(paste0("R_wt: ", round(r0, 2), "\nR_v:  ", round(r0*1.5, 2), "\nRt: ", round(rt, 2)))

  # Calculate effective initial susceptible
  S0 <- S + (1 - vac_eff)*(1 - pct_ek)*R + (1 - vac_eff_ek)*pct_ek*R

  # Calculate vaccination rate
  v_rt <- vac_per_day * pct_vac_s

  # Translate to model parameters
  si <- 5
  beta_n <- (r0 / si) / S0
  beta_uk <- 1.5 * beta_n
  beta_ek <- beta_n
  gamma  <- 1 / si

  # Pass initial values and parameters to deSolve
  y <- c(S = S, I = I, R = R, pct_uk = pct_uk, pct_ek = pct_ek)
  params <- c(
    c(beta_n = beta_n, beta_uk = beta_uk, beta_ek = beta_ek, gamma = gamma),
    c(S0 = S0, pop = pop),
    c(v_rt = v_rt, vac_eff = vac_eff, vac_eff_ek = vac_eff_ek)
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
      i = c(NA_real_, diff(.data[["I"]])) + dplyr::lag(.data[["I"]]) / 5,
      i_obs = i * {{ detect }}
    )

  if (any(!dplyr::between(round(result[["S"]]), 0, pop))) {
    rlang::warn("One or more values of `S` are outside valid range")
  }
  if (any(!dplyr::between(round(result[["I"]]), 0, pop))) {
    rlang::warn("One or more values of `I` are outside valid range")
  }
  if (any(!dplyr::between(round(result[["R"]]), 0, pop))) {
    rlang::warn("One or more values of `R` are outside valid range")
  }
  if (any(!dplyr::between(round(rowSums(result[c("S", "I", "R")])), 0, pop))) {
    rlang::warn(
      paste(
        "Total size of simulation is outside valid range",
        "at one or more time points"
      )
    )
  }
  if (any(!dplyr::between(result[["pct_uk"]], 0, 1))) {
    rlang::warn("One or more values of `pct_uk` are outside valid range")
  }
  if (any(!dplyr::between(result[["pct_ek"]], 0, 1))) {
    rlang::warn("One or more values of `pct_ek` are outside valid range")
  }
  if (any(!dplyr::between(rowSums(result[c("pct_uk", "pct_ek")]), 0, 1))) {
    rlang::warn(
      paste(
        "Total percent of variant cases is outside valid range",
        "at one or more time points"
      )
    )
  }

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
      return(list(c(dS = 0, dI = 0, dR = 0, dPuk = 0, dPek = 0)))
    }

    # Create synthetic I compartments for different variants
    Iuk <- I * pct_uk
    Iek <- I * pct_ek
    In  <- I - Iuk - Iek

    # Translate into flows
    is_n  <- S * In  * beta_n
    is_uk <- S * Iuk * beta_uk
    is_ek <- S * Iek * beta_ek
    ir_n  <- R * In  * beta_n  * (1 - vac_eff)
    ir_uk <- R * Iuk * beta_uk * (1 - vac_eff)
    ir_ek <- R * Iek * beta_ek * (1 - vac_eff_ek)
    i_n  <- is_n + ir_n
    i_uk <- is_uk + ir_uk
    i_ek <- is_ek + ir_ek
    r_n  <- In  * gamma
    r_uk <- Iuk * gamma
    r_ek <- Iek * gamma
    is <- is_n + is_uk + is_ek
    ir <- ir_n + ir_uk + ir_ek
    s <- 0
    i <- is + ir
    r <- r_n + r_uk + r_ek
    v <- v_rt * S / S0

    # Update derivatives
    dIuk <- i_uk - r_uk
    dIek <- i_ek - r_ek
    dS   <- s - is - v
    dI   <- i - r
    dR   <- r + v - ir

    # Update percentages & derivatives
    pct_uk_next <- (Iuk + dIuk) / (I + dI)
    pct_ek_next <- (Iek + dIek) / (I + dI)
    dPuk <- pct_uk_next - pct_uk
    dPek <- pct_ek_next - pct_ek

    list(c(dS, dI, dR, dPuk, dPek))
  })
}

plot_sirv <- function(x, .y = .data[["I"]] / 3, ylab = "Observed Cases", group = NULL) {
  gg_obj <- x %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["time"]],
        y = {{ .y }},
        group = {{ group }},
        color = {{ group }}
      )
    ) +
    ggplot2::geom_line(alpha = 1)

  gg_obj %>%
    coviData::set_covid_theme() %>%
    coviData::add_axis_labels(xlab = "Date", ylab = ylab)
}
