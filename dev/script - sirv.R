scenarios <- tidyr::expand_grid(
  end = as.Date("2021-05-31"),
  rt = seq(0.9, 1.3, by = 0.1),
  vac_per_day = c(1000, 2500, 5000),
  pct_vac_s = seq(0.6, 0.8, by = 0.1),
  vac_eff = c(0.65, 0.8, 0.95),
  vac_eff_ek = c(0.1, 0.65, 0.95),
  pct_var = seq(0.1, 0.5, by = 0.1),
  pct_var_uk = seq(0.7, 0.9, by = 0.1),
  pct_var_ek = c(0.01, 0.1)
) %>%
  dplyr::filter(vac_eff_ek <= vac_eff)

map_sirv <- function(
  end,
  rt,
  vac_per_day,
  pct_vac_s,
  vac_eff,
  vac_eff_ek,
  pct_var,
  pct_var_uk,
  pct_var_ek
) {
  result <- sirv(
    end = end,
    rt = rt,
    vac_per_day = vac_per_day,
    pct_vac_s = pct_vac_s,
    vac_eff = vac_eff,
    vac_eff_ek = vac_eff_ek,
    pct_var = pct_var,
    pct_var_uk = pct_var_uk,
    pct_var_ek = pct_var_ek
  )
  dplyr::tibble(
    rt = rt,
    vac_per_day = vac_per_day,
    pct_vac_s = pct_vac_s,
    vac_eff = vac_eff,
    vac_eff_ek = vac_eff_ek,
    pct_var = pct_var,
    pct_var_uk = pct_var_uk,
    pct_var_ek = pct_var_ek,
    result = list(result)
  )
}

future::plan(strategy = "multisession")

results <- furrr::future_pmap_dfr(
  scenarios,
  .f = map_sirv,
  .options = furrr::furrr_options(chunk_size = 10L, globals = c("sirv", "sirv_fn"))
)

saveRDS(results, file = "dev/sim_results.rds", ascii = TRUE)

