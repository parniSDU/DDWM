#' Transition Probability Function

#' This function returns the transition probability under
#' stochastic repair time for a given basic event and a proxel.


calProb_s <- function(proxel, model , state, delta) {
  t <- proxel$ageInt
  params <- model$time_distributions[[state]]
  possible_states <- model$transition_states[[state]]
  prob <- model$prob_transition[[state]]
  h <- dgamma(t, shape = params["shape"], rate = params["rate"]) / pgamma(t, shape = params["shape"], rate = params["rate"], lower.tail = FALSE)
  fstate <- c("FailureState 2", "FailureState 3", "FailureState 4")
  wstate <- c("WorkingState 1", "WorkingState 2", "WorkingState 3")

  if (state %in% c("WorkingState 4", "FailureState 1")) {
    mw2 <- adjust_value(delta * h)
    mw1 <- 1 - (mw2)
    m <- data.frame(States = c(state, possible_states$state), TranProb = c(mw1, mw2))
  }
  if (state %in% fstate) {
    mw2 <- adjust_value(delta * h)
    mw1 <- 1 - (mw2)
    m <- data.frame(States = c(state, possible_states$state[1]), TranProb = c(mw1, mw2))
  }
  if (state %in% wstate) {
    mw2 <- prob * delta * h
    mf1 <- (1 - prob) * delta * h
    mw1 <- 1 - (mw2 + mf1)
    m <- data.frame(States = c(state, possible_states$state), TranProb = c(mw1, mw2, mf1))
  }

  return(m)
}
