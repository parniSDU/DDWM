#' Instantaneous Unavailability Vector
#'
#' This function calculates the isntantaneous unavailablity values
#' of a product with stochastic repair time, along with the average warranty cost.
#'
#' @param state a string value for the state
#' @param model a list containing states, transition matrix, distributions and their parameters
#' @param delta a numeric value as time step
#' @param tol a numeric value for the tolerance level
#' @param alpha a numeric value as a decision variable (between 0 and W)
#' @param W a numeric value as the warranty period
#' @param K a numeric value as a decision variable (between 1 and N)

#'
#' @return a numeric vector of warranty costs


Proxel_s <- function(state, W, model, delta, tol, alpha, K) {
  tt <- delta
  steps <- W / delta
  meanCost <- numeric(steps)
  meanCost[1] <- 0

  proxel <- list()
  proxel[[1]] <- data.frame(
    State = "WorkingState 1", ageInt = 0,
    Prob = 1, Cost = 0
  )
  ##
  funsext <- function(s, model, delta, tt, alpha, W, K) {
    pL <- nextLevel_s(prox[s, ], model, delta, tt, alpha, W, K)
    ind <- which(pL$Prob < tol)
    if (length(ind) != 0) {
      pL <- pL[-ind, ]
    }
    return(pL)
  }

  ffunsext <- function(s) {
    return(funsext(s, model, delta, tt, alpha, W, K))
  }
  ###
  i <- 2
  while (i <= (steps + 1)) {
    Pro <- NULL
    prox <- proxel[[i - 1]]
    pL <- plyr::ldply(lapply(1:nrow(prox), ffunsext), data.frame)

    Pro <- pL
    meanCost[i] <- sum(Pro$Cost)
    proxel[[i]] <- Pro
    print(data.frame(i, tt, Pro))
    i <- i + 1
    tt <- tt + delta
  }
  out <- list(CumCost = cumsum(meanCost))
  return(out)
}
