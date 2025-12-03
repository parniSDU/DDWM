#' Transition Probability Function

#' This function returns the transition probability under
#' zero repair time for a given basic event and a proxel.

calProb <- function(proxel, model , state, delta) {

  t <- proxel$ageInt
  params <- model$time_distributions[[state]]
  possible_states <- model$transition_states[[state]]
  prob <- model$prob_transition[[state]]
  h <- dgamma(t, shape = params["shape"], rate = params["rate"]) / pgamma(t, shape = params["shape"], rate = params["rate"], lower.tail = FALSE)
  if (state == "WorkingState 4") {
    mw2 <- delta * h
    mw1 <- 1 - (mw2)
    m <- data.frame(States = c(state, possible_states$state), TranProb = c(mw1, mw2))
  } else {
    mw2 <- prob * delta * h
    mf1 <- (1 - prob) * delta * h
    mw1 <- 1 - (mw2 + mf1)
    m <- data.frame(States = c(state, possible_states$state), TranProb = c(mw1, mw2, mf1))
  }

  return(m)
}
