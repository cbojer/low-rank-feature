mse <- function(e, na.rm = TRUE) e %>% raise_to_power(2) %>% mean(na.rm = na.rm)
rmse <- function(e, na.rm = TRUE) e %>% mse(na.rm = na.rm) %>% sqrt