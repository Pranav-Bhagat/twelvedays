sample_mean <- function(vec, n) {
  subsample <- sample(vec, n, replace=TRUE)
  mean(subsample, na.rm = TRUE)}


