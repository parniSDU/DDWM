#' Adjust Value Function
#'
#' This function adjusts the values between 0 and 1


adjust_value <- function(x) {
  ifelse(x < 0, 0, ifelse(x > 1, 1, x))
}
