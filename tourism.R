library(Tcomp)
library(purrr)
library(magrittr)
library(data.table)
library(dplyr)
library(h2o)

source("ts_glrm.R")
source("accuracy.R")
source("fc_methods.R")
source("features.R")
h2o.init()


monthly <- tourism[paste0("M", 1:366)]
quarterly <- tourism[paste0("Q", 1:427)]
yearly <- tourism[paste0("Y", 1:518)]

dt <- do.call(cbind, as.list(monthly))

to_tsdt.data.table <- function(dt) {
  class(dt) <- c("tsdt", "data.table", "data.frame")  
  return(dt)
}

to_tsdt.matrix <- function(tsmat, target.name = "target", keys = NULL, keys.name = "timeseries", index = NULL, index.name = "time") {
  if(nrow(tsmat) == 0) {
    stop("Number of rows have to be larger than zero.")
  }
  
  if(is.null(index)) {
    index <- 1:nrow(tsmat)
  } else {
    if(length(index) != nrow(tsmat)) {
      stop("Index needs to be same length as the number of rows in the provided matrix.")
    }
  }

  wide_dt <- tsmat %>% 
    as.data.table() %>%
    .[, time := index] #Fix this
  
  if(!is.null(keys)) {
    colnames(wide_dt) <- keys
  }
  
  tsdt <- wide_dt %>%
    melt(id.vars = "time", variable.name = keys.name, value.name = target.name)
  
  
  return(
    str(
      tsdt,
      class = c("tsdt", "data.table", "data.frame"),
      keys = keys.name,
      index = 
    )
  )
}

to_tsdt.data.frame <- function(df) {
  
}

to_tsdt.mts <- function(msts, target.name = "target", keys = NULL, index = NULL) {
 dt <- to_tsdt.matrix(msts, target.name = target.name, keys = keys, index = index)
 return(dt)
}

# For each feature: two methods. One on numeric, one on dataframe that takes a key argument?



to_tsdt <- function(dt) {
  UseMethod("to_tsdt", dt)
}
to_wide <- function(tsdt) {
  UseMethod("to_wide", tsdt)
}

to_wide.tsdt <- function(tsdt) {
  tsdt %>% dcast(time ~ timeseries, value.var = "target") %>% select(-time)
}



extract_train <- function(x) data.table(target = as.numeric(x$x), time = 1:x$n, timeseries = x$sn)
monthly_dt <- lapply(monthly, extract_train) %>% rbindlist() #%>% to_tsdt()


monthly_dt

as.character(adi)

glrm <- ts_glrm(monthly_dt)
glrm$err
glrm$model@model$importance

monthly$M3$x %>% plot

library(tsibble)
