library(expsmooth)
library(purrr)
library(dplyr)
library(forecast)
library(magrittr)
library(data.table)


# First step: Ts mat to Data.table
# Melt
# Figure out how it handles missing values
cp_dt <- carparts %>% 
  as.data.table() %>%
  .[, time := .I] %>%
  melt(id.vars = "time", variable.name = "timeseries", value.name = "value") %>%
  .[order(timeseries, time)]

# Seems we can safely drop NA as they are continuous and start in the middle of the series (i.e. 1)
cp_dt <- cp_dt[!is.na(value)]

# Train / Test
train <- cp_dt[time <= 39]
test <- cp_dt[time > 39]

train_feat <- copy(train)

train_feat[, (paste0("lag_", 1:24)) := lapply(1:24, function(x) shift(value, x)), by = .(timeseries)]

train <- non_missing[1:39,]
test <- non_missing[40:51,]
y_test <- test$value 

feat_eng <- rbindlist(list(train, test[, value := NA])) %>%
  .[, (paste0("lag_", 1:24)) := lapply(1:24, function(x) shift(value, x)), by = .(timeseries)]

train_x <- feat_eng[time <= 39]
test_x <- feat_eng[time > 39]


h <- 12
lag_to_use <- 12
cols_to_use <- c('value', paste0("lag_", 1:lag_to_use))

ds <- train_x[time > lag_to_use, ..cols_to_use]
global_ar_model <- lm(value ~ ., data = ds)

test_cols_to_use <- c(cols_to_use, "timeseries")

test_start <- 40
ds <- test_x[time == test_start, ..test_cols_to_use]
preds <- predict(global_ar_model, ds)

pred_list <- vector(mode="list", length = 12)
pred_list[[1]] <- data.table(.preds = preds, timeseries = ds$timeseries, time = 40)

for(i in 2:h) {
  t <- test_start + i - 1
  lags_to_get <- seq(i-1, 1, by = -1)
  lags <- lapply(lags_to_get, function(j) pred_list[[j]]$.preds)
  ds <- test_x[time == t, ..test_cols_to_use] %>% copy() %>% .[, paste0("lag_", 1:(i-1)) := (lags)]
  preds <- predict(global_ar_model, ds)
  pred_list[[i]] <- data.table(.preds = preds, timeseries = ds$timeseries, time = t)
}

pred_df <- rbindlist(pred_list)

preds_joined <- merge(pred_df, cp_dt[time > 39]) %>% .[ ,error := value - .preds]

preds_joined$error %>% rmse()

global_ar_model$residuals %>% rmse()


# For auto arima, only forecast those who are in test
ts_to_fc <- unique(test$timeseries)



arima_preds <- train[timeseries %in% ts_to_fc, arima_monthly_fc(value), by = .(timeseries)]
saveRDS(arima_preds, "arima_carparts.rds")
arima_preds <- readRDS("arima_carparts.rds")

arima_joined <- merge(arima_preds, cp_dt[time > 39]) %>% .[ ,error := value - .preds]
arima_joined$error %>% rmse



theta_preds <- train[timeseries %in% ts_to_fc, theta_monthly_fc(value), by = .(timeseries)]
theta_joined <- merge(theta_preds, cp_dt[time > 39]) %>% .[ ,error := value - .preds]
theta_joined$error %>% rmse


library(h2o)

h2o.init()

matrix_dt <- carparts[1:39,] %>% 
  as.data.table()

dt_h2o <- as.h2o(matrix_dt)


basic_glrm <- h2o.glrm(
  training_frame = dt_h2o,
  k = 20, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "None", 
  transform = "STANDARDIZE", 
  max_iterations = 2000,
  seed = 123,
  impute_original = TRUE,
  ignore_const_cols = FALSE
)


saveRDS(basic_glrm, "glrm.rds")

rec_scaled <- h2o.reconstruct(basic_glrm, dt_h2o, reverse_transform = TRUE)
rec_simple <- h2o.reconstruct(glrm_one, dt_h2o, reverse_transform = TRUE)

err <- (as.matrix(matrix_dt) - as.matrix(rec_scaled))
err_simple <- (as.matrix(matrix_dt) - as.matrix(rec_simple))

rec_rmse <- tibble(
  timeseries = err %>% colnames(), 
  rec_rmse = err %>% apply(2, rmse),
  rec_simple_rmse = err_simple %>% apply(2, rmse),
  adi = matrix_dt %>% apply(2, adi),
  cv2 = matrix_dt %>% apply(2, cv2),
  n = matrix_dt %>% apply(2, length_non_na)
)


global_ar_rmse <- preds_joined %>%
  group_by(timeseries) %>%
  summarize(rmse = rmse(error))

arima_rmse <- arima_joined %>%
  group_by(timeseries) %>%
  summarize(rmse = rmse(error))

theta_rmse <- theta_joined %>%
  group_by(timeseries) %>%
  summarize(rmse = rmse(error))

library(ggplot2)

rec_rmse %>% 
  left_join(global_ar_rmse) %>% 
  ggplot(aes(x = rec_rmse, y = rmse)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

rec_rmse %>% 
  left_join(global_ar_rmse) %>% 
  ggplot(aes(x = rec_simple_rmse, y = rmse)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

perf_stats_ds <- rec_rmse %>% 
  left_join(global_ar_rmse) %>% 
  left_join(select(arima_rmse, timeseries, arima_rmse = rmse)) %>%
  left_join(select(theta_rmse, timeseries, theta_rmse = rmse))

perf_stats_ds %>%
  select(-n) %>%
  tidyr::gather(key = variable, value = value, -timeseries, -rmse) %>%
  ggplot(aes(x = value, y = rmse)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free_x")

perf_stats_ds %>%
  ggplot(aes(x = rmse, y = arima_rmse)) +
  geom_point() +
  geom_smooth(method = "lm")

perf_stats_ds %>%
  ggplot(aes(x = rmse, y = theta_rmse)) +
  geom_point() +
  geom_smooth(method = "lm")

perf_stats_ds %>%
  select(timeseries, rmse, arima_rmse, theta_rmse) %>%
  tidyr::gather(key = var, value = value, -timeseries) %>%
  ggplot(aes(x = value, group = var, color = var)) +
  stat_ecdf() 
  


lm(rmse ~ rec_rmse + adi + cv2, perf_stats_ds) %>% summary
lm(rmse ~ rec_simple_rmse + adi + cv2, perf_stats_ds) %>% summary

lm(arima_rmse ~ rec_rmse + adi + cv2, perf_stats_ds) %>% summary
lm(theta_rmse ~ rec_rmse + adi + cv2, perf_stats_ds) %>% summary
lm(perf_diff ~ rec_rmse + adi + cv2, perf_stats_ds %>% mutate(perf_diff = rmse - arima_rmse)) %>% summary
lm(perf_diff ~ rec_rmse + adi + cv2, perf_stats_ds %>% mutate(perf_diff = rmse - theta_rmse)) %>% summary

perf_stats_ds %>%
  mutate(perf_diff = rmse - arima_rmse) %>% 
  ggplot(aes(x = rec_rmse, y = perf_diff)) +
  geom_point() +
  geom_smooth(method = "lm")

