
adi <- function(x) length(x)/length(x[x > 0])
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
cv2 <- function(x, na.rm = TRUE) cv(x, na.rm = na.rm)^2
length_non_na <- function(x) na.omit(x) %>% length