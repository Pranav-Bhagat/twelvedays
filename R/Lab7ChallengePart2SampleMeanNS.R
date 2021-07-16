sample_means_ns <- function(vec, reps, ns) {
  df <- map_dfc(ns, ~many_sample_means(vec, reps, .x))
  colnames(df) <- ns
  return(df %>%
           pivot_longer(colnames(df), names_to = "n", values_to = "sample_mean") %>%
           mutate(n = as.double(n)))}