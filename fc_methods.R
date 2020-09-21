forecast_pkg_fc<- function(x, fc_func, freq, h) {
  x %>% ts(frequency = freq) %>% fc_func(h = h) %>% magrittr::extract2("mean")
}

arima_fc_func <- function(x, h) auto.arima(x) %>% forecast(h = h)
ets_fc_func <- function(x, h) ets(x) %>% forecast(h = h)


arima_fc <- function(x, freq, h) {
  fc <- forecast_pkg_fc(x, arima_fc_func, freq, h)
  list(.preds = as.numeric(fc), time = 40:51)
}

theta_fc <- function(x, freq, h) {
  fc <- forecast_pkg_fc(x, thetaf, freq, h)
  list(.preds = as.numeric(fc), time = 40:51)
}

ets_fc <- function(x, freq, h) {
  fc <- forecast_pkg_fc(x, ets_fc_func, freq, h)
  list(.preds = as.numeric(fc), time = 40:51)
}

arima_monthly_fc <- purrr::partial(arima_fc, freq = 12, h = 12)
theta_monthly_fc <- purrr::partial(theta_fc, freq = 12, h = 12)
ets_monthly_fc <- purrr::partial(ets_fc, freq = 12, h = 12)