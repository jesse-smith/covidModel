vector log_interval(vector x, real lower, real upper, real offset) {
  return log((x + offset - lower)/(upper - x + offset));
}

vector log_interval_inv(vector x, real lower, real upper, real offset) {
  return (upper-lower) * exp(x) / (1 + exp(x) - lower + offset);
}

real offset(vector x) {
  real min_x = min(x);
  return -min_x + sqrt(machine_precision());
}
