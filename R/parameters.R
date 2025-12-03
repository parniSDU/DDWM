#' Estimates the model parameters from the data
#'
#' For a given dataset and a warranty policy, this function
#' calculates all the model parameters.
#'
#' @param data a data frame containing the data set
#' @param use_estimated a logic value indicating whether to use estimated states or not.
#' @param cutpoint a numeric value as the cutpoint
#' @param policy a numeric value as the warranty policy (1 or 2)
#'
#' @return a list of parameters

parameters <- function(data, use_estimated = TRUE, cutpoint, policy) {

  if (use_estimated) {
    data$chosen_column <- data$Estimated_state
  } else {
    data$chosen_column <- data$state
  }

  reutrnedItems <- (unique(data$Item))
  reutrnedItemsNo <- length(reutrnedItems)

  f1 <- data %>% filter(chosen_column == "FailureState 1")
  f1Items <- unique(f1$Item)
  f1No <- length(f1Items)
  w2Items <- setdiff(reutrnedItems, f1Items)
  p1 <- length(w2Items) / reutrnedItemsNo

  f1index <- unique(f1$Item)
  r1 <- NULL

  for (i in f1index) {
    d <- data %>% filter(Item == i)
    index <- which(d$chosen_column == "FailureState 1")
    r1 <- c(r1, d$time[index + 1] - d$time[index])
  }

  lambda1 <- 1 / mean(r1)
  mu1 <- 1 / mean(f1$time)

  w1 <- data %>% filter(chosen_column == "WorkingState 1")
  replaceindex <- which(w1$cost > 500)


  cm1 <- mean(w1[c(-replaceindex), "cost"])
  cr4 <- mean(w1[c(replaceindex), "cost"])
  cr3 <- NA
  if (policy == 2) {
    cr3 <- mean(filter(w1[c(replaceindex), ], cost < cutpoint)$cost)
    cr4 <- mean(filter(w1[c(replaceindex), ], cost > cutpoint)$cost)
  }


  # F2
  f2 <- data %>% filter(chosen_column == "FailureState 2")
  w3Items <- setdiff(w2Items, unique(f2$Item))
  p2 <- length(w3Items) / length(w2Items)

  f2index <- unique(f2$Item)
  r2 <- NULL
  w2f2 <- NULL

  for (i in f2index) {
    d <- data %>% filter(Item == i)
    index <- which(d$chosen_column == "FailureState 2")
    r2 <- c(r2, d$time[index + 1] - d$time[index])
    index <- index[-1]
    w2f2 <- c(w2f2, d$time[index] - d$time[index - 1])
  }
  lambda2 <- 1 / mean(r2)
  mu2 <- 1 / mean(w2f2)

  w2 <- data %>% filter(chosen_column == "WorkingState 2")
  cm2 <- mean(w2[, "cost"])

  #   F3

  f3 <- data %>% filter(chosen_column == "FailureState 3")
  w4Items <- setdiff(w3Items, unique(f3$Item))
  p3 <- length(w4Items) / length(w3Items)


  f3index <- unique(f3$Item)
  r3 <- NULL
  w3f3 <- NULL
  for (i in f3index) {
    d <- data %>% filter(Item == i)
    index <- which(d$chosen_column == "FailureState 3")
    r3 <- c(r3, d$time[index + 1] - d$time[index])
    index <- index[-1]
    w3f3 <- c(w3f3, d$time[index] - d$time[index - 1])
  }
  lambda3 <- 1 / mean(r3)
  mu3 <- 1 / mean(w3f3)

  w3 <- data %>% filter(chosen_column == "WorkingState 3")
  cm3 <- mean(w3[, "cost"])


  #    F4
  f4 <- data %>% filter(chosen_column == "FailureState 4")
  w4Items <- setdiff(w4Items, unique(f4$Item))


  f4index <- unique(f4$Item)
  r4 <- NULL
  w4f4 <- NULL
  for (i in f4index) {
    d <- data %>% filter(Item == i)
    index <- which(d$chosen_column == "FailureState 4")
    r4 <- c(r4, d$time[index + 1] - d$time[index])
    index <- index[-1]
    w4f4 <- c(w4f4, d$time[index] - d$time[index - 1])
  }
  lambda4 <- 1 / mean(r4)
  mu4 <- 1 / mean(w4f4)

  w4 <- data %>% filter(chosen_column == "WorkingState 4")
  cm4 <- mean(w4[, "cost"])



  out <- data.frame(
    P1 = p1, P2 = p2, P3 = p3, mu1 = mu1, mu2 = mu2, mu3 = mu3, mu4 = mu4,
    Theta1 = lambda1, Theta2 = lambda2, Theta3 = lambda3, Theta4 = lambda4,
    cm1 = cm1, cm2 = cm2, cm3 = cm3, cm4 = cm4, cr3 = cr3, cr4 = cr4
  )
  return(out)
}
