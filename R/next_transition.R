#' State and the cost of the next transition
#'
#' For a given basic event with stochastic repair time, this function
#' calculates the next state of the system with the associated cost.
#'
#' @param state a string value for the state
#' @param model a list containing states, transition matrix, distributions and their parameters
#' @param t a numeric value as current time
#' @param alpha a numeric value as a decision variable (between 0 and W)
#' @param W a numeric value as the warranty period
#' @param K a numeric value as a decision variable (between 1 and N)
#'
#' @return a list containing the next state and the cost of the next transition


next_transition <- function(state, K, t, model, alpha, W) {
  params <- model$time_distributions[[state]]
  possible_states <- model$transition_states[[state]]
  t <- sum(t, rgamma(1, shape = params["shape"], rate = params["rate"]))
  fstate <- c("FailureState 1", "FailureState 2", "FailureState 3", "FailureState 4")
  wstate <- c("WorkingState 1", "WorkingState 2", "WorkingState 3")

  if (state == "WorkingState 4") {
    next_state <- "FailureState 4"
    cost <- 0
  }

  if (state %in% fstate) {
    if (W - t > alpha) {
      if (state %in% fstate[K:4]) {
        next_state <- possible_states$state[2] # replace

        cost <- rnorm(1, mean = possible_states$cost[2], sd = 30)
      }
      if (state %in% fstate[1:(K - 1)]) {
        next_state <- possible_states$state[1] # repair

        cost <- rnorm(1, mean = possible_states$cost[1], sd = 20)
      }
    } else {
      next_state <- possible_states$state[1] # repair

      cost <- rnorm(1, mean = possible_states$cost[1], sd = 20)
    }
  }
  if (state %in% wstate) {
    prob <- prob_transition[[state]]
    r <- runif(1, 0, 1)
    ifelse(r <= prob, next_state <- possible_states$state[1], next_state <- possible_states$state[2])
    cost <- 0
  }

  out <- list(state = next_state, time = t, cost = cost)
  return(out)
}
