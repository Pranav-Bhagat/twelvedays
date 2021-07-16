many_sample_means <- function(vec, reps, n) {
  replicate(reps, sample_mean(vec, n))}
