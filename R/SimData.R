#' Simulates warranty logs data
#'
#' For a given set of parameters, simulates warranty logs data
#'
#' @param model a list containing states, transition matrix, distributions and their parameters
#' @param path a string value to save the simulated data
#' @param SimSize a numeric value as the simulation size
#' @param alpha a numeric value as a decision variable (between 0 and W)
#' @param W a numeric value as the warranty period
#' @param K a numeric value as a decision variable (between 1 and N)
#'
#' @return a data frame containing warranty logs data



SimData <- function(alpha, K, W, model, SimSize, path) {
  out <- NULL
  for (i in 1:SimSize) {
    state <- "WorkingState 1"
    t <- 0
    cost <- 0
    data <- data.frame(state = state, time = t, cost = cost)

    while (t < W) {
      d <- next_transition(state, K, t, model, alpha, W)
      next_state <- d$state
      t <- d$time
      cost <- d$cost
      state <- next_state
      data <- rbind(data, d)
    }
    data$Item <- i
    out <- rbind(out, data)

  }
  write.table(out, path, row.names = FALSE)
  return(out)
}
