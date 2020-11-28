extract_trend_vec <- function(x, t = NULL, period = 7, method = c("stl", "bsts", "gam"), ...) {
	
	S3 <- method %>%
	  rlang::arg_match() %>%
	  set_attr("class", .)
	
	UseMethod("extract_trend_vec", S3)
}

extract_trend_vec.stl <- function(x, t = NULL, period = 7, trend = 30, method = "stl", ...) {
	ts(x, frequency = period) %>%
	  stats::stl(
	    s.window = period,
       t.window = trend,
		robust = TRUE
	  ) %>%
	  extract2(1) %>%
	  extract2("trend")
	  
}

extract_trend_vec.bsts <- function(x, t = NULL, period = 7, niter = 11000, method = "bsts", ...) {
  
  y <- zoo::zoo(x, order.by = t)
  
  remove(x, t)
  
  bsts::AddStudentLocalLinearTrend(y = y) %>%
    bsts::AddSeasonal(y = y, nseasons = s_period) %>%
    bsts::bsts(formula = y, niter = n_iter)
	
}

extract_trend_vec.gam