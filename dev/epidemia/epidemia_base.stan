functions {
  #include convolve.stan
  #include log_interval.stan
}

data {
  int<lower=0> T;
  int<lower=1> obs_days;
  int<lower=1> inf_days;
  real<lower=0> si_shape;
  real<lower=0> si_rate;
  real<lower=0> r_upper;
  vector<lower=0> y[T];
}

transformed data {
  vector io_cdf = gamma_cdf(
    linspaced_int_array(obs_days + 1, 0, obs_days),
    obs_shape,
    obs_rate
  )

  vector si_cdf = gamma_cdf(
    linspaced_int_array(inf_days + 1, 0, inf_days),
    si_shape,
    si_rate
  );

  vector si = si_cdf[2:(inf_days - 1)] - si_cdf[1:inf_days];

  int[N, 5] index = conv_index(y, si);
}

parameters {
  real b_trend;
  real o_trend;
  real<lower=0> sd_trend;
  real<lower=0> sd_level;
  real<lower=0> sd_r;
  real<lower=0> disp;
  vector[T]<lower=0> level;
  vector[T]<lower=-min(level)> trend;
  vector[T]<lower=sqrt(machine_precision())> i_mu;
  vector[T]<lower=0> i;
  vector[T]<lower=0> i_obs;
  vector[T]<lower=0,upper=1> detect;
}

transformed parameters {
}

model {
  i_obs ~ (detect.*convolve(i, io, io_index), disp);
  detect[2:T] ~ beta(detect[1:(T-1)].*i[1:(T-1)]+1, (1-detect[1:(T-1)])*i[1:(T-1)]+1);
  i ~ poisson(1/i_mu);
  i_mu[2:T]  ~ hypergeometric(
    r[1:(T-1)].*convolve(i_mu[1:(T-1)], si, si_index)
  );
  r[2:T] ~ lognormal(level[1:(T-1)], sd_r);
  level[2:T] ~ lognormal(level[1:(T-1)] + trend[1:(T-1)], sd_level);
  trend[2:T] ~ normal(b_trend * trend[1:(T-1)] + o_trend, sd_trend);
}

generated quantities {

}
