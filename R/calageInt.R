#' Age Intensity Function
#'
#' This function returns a numeric value indicating
#' the pending time (in terms of time steps, delta) for a given state change of a proxel.


calageInt <- function(state, proxel, delta) {
  ifelse(proxel$State != state, ageint <- 0, ageint <- proxel$ageInt + delta)
  ageint
}
