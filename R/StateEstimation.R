#' Estimates the degradation state of the product using cost of repair/replace
#'
#' For a given data set of warranty logs, estimates
#' the degradation state of the product from its cost of repair/replace
#'
#' @param data a data frame containing warranty logs
#' @param policy a numeric value as the warranty policy (1 or 2)
#'
#' @return a list containing the plots of the warranty logs,
#' a data set with estimated states, accuracy and the cutpoints


StateEstimation <- function(data, policy = 2) {

  wstate<-paste("WorkingState ",1:4, sep="")
  fstate<-paste("FailureState ",1:4, sep="")
  states<-c(wstate, fstate)

  df <- data[data$state_type == "w", ]
  df$rtime <- df$time
  df$wstate <- df$state

  df$ftime <- data$time[data$state_type == "f"]
  df$fstate <- data$state[data$state_type == "f"]
  df <- df %>% dplyr::select(Item, fstate, ftime, wstate, rtime, cost)

  # Fit Gaussian mixture model to repaired items only
  modell <- Mclust(df$cost[df$cost < 500], G = 4)
  # Classification of each observation
  states <- modell$classification

  # Cut-off points (boundaries between components)
  boundaries <- sort(modell$parameters$mean)
  cut_points <- (boundaries[-length(boundaries)] + boundaries[-1]) / 2

  if (policy == 2) {
    model1 <- Mclust(df$cost[df$cost > 500], G = 2)
    states1 <- model1$classification
    boundaries1 <- sort(model1$parameters$mean)
    cut_points1 <- (boundaries1[-length(boundaries1)] + boundaries1[-1]) / 2
  }


  # prepare the plot

  labels <- c("Cutpoint1", "Cutpoint2", "Cutpoint3")
  if (policy == 2) {
    labels <- c(labels, "Cutpoint4")
  }
  okabe_ito <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00")


  p <- ggplot(df, aes(x = ftime, y = cost, color = factor(fstate))) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = cut_points, linetype = "dashed", color = "red") +
    scale_y_continuous(
      breaks = sort(c(cut_points, pretty(df$cost, 5))),
      labels = function(x) {
        ifelse(x %in% cut_points,
          labels[match(x, cut_points)],
          as.character(x)
        )
      }
    ) +
    scale_color_manual(values = okabe_ito) +
    labs(x = "Time of Failure", y = "Cost", color = "Failure States", linetype = " ") +
    guides(
      colour = guide_legend(position = "inside"),
      linetype = guide_legend(position = "inside")
    ) +
    theme_minimal() +
    theme(legend.justification.inside = c(1, 1))
  if (policy == 2) {
    p <- p +
      geom_hline(yintercept = c(cut_points, cut_points1), linetype = "dashed", color = "red") +
      scale_y_continuous(
        breaks = sort(c(cut_points, cut_points1, pretty(df$cost, 5))),
        labels = function(x) {
          ifelse(x %in% c(cut_points, cut_points1),
            labels[match(x, c(cut_points, cut_points1))],
            as.character(x)
          )
        }
      )
  }

  ### Estimating States
  df$Estimated_fstate <- NULL
  df$Estimated_fstate[df$cost <= cut_points[1]] <- "FailureState 1"
  df$Estimated_fstate[df$cost <= cut_points[2] & df$cost > cut_points[1]] <- "FailureState 2"
  df$Estimated_fstate[df$cost <= cut_points[3] & df$cost > cut_points[2]] <- "FailureState 3"
  df$Estimated_fstate[df$cost > cut_points[3]] <- "FailureState 4"
  if (policy == 2) {
    df$Estimated_fstate[df$cost < cut_points1[1] & df$cost > 500] <- "FailureState 3"
    df$Estimated_fstate[df$cost > cut_points1[1] & df$cost > 500] <- "FailureState 4"
  }


  df$Estimated_wstate <- NA
  df$Estimated_wstate[df$cost > 500] <- "WorkingState 1"
  df$Estimated_wstate[df$Estimated_fstate == "FailureState 1"] <- "WorkingState 1"
  df$Estimated_wstate[df$Estimated_fstate == "FailureState 2"] <- "WorkingState 2"
  if (policy == 2) {
    df$Estimated_wstate[df$Estimated_fstate == "FailureState 3" &
      df$cost < cut_points1[1] & df$cost > 500] <- "WorkingState 1"
    df$Estimated_wstate[df$Estimated_fstate == "FailureState 3" &
      df$cost < 500] <- "WorkingState 3"
  } else {
    df$Estimated_wstate[df$Estimated_fstate == "FailureState 3"] <- "WorkingState 3"
  }
  df$Estimated_wstate[df$Estimated_fstate == "FailureState 4" & df$cost < 500] <- "WorkingState 4"

  pe <- ggplot(df, aes(x = ftime, y = cost, color = factor(Estimated_fstate))) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = cut_points, linetype = "dashed", color = "red") +
    scale_y_continuous(
      breaks = sort(c(cut_points, pretty(df$cost, 5))),
      labels = function(x) {
        ifelse(x %in% cut_points,
          labels[match(x, cut_points)],
          as.character(x)
        )
      }
    ) +
    scale_color_manual(values = okabe_ito) +
    labs(x = "Time of Failure", y = "Cost", color = "Failure States", linetype = " ") +
    guides(
      colour = guide_legend(position = "inside"),
      linetype = guide_legend(position = "inside")
    ) +
    theme_minimal() +
    theme(legend.justification.inside = c(1, 1))
  if (policy == 2) {
    pe <- pe +
      geom_hline(yintercept = c(cut_points, cut_points1), linetype = "dashed", color = "red") +
      scale_y_continuous(
        breaks = sort(c(cut_points, cut_points1, pretty(df$cost, 5))),
        labels = function(x) {
          ifelse(x %in% c(cut_points, cut_points1),
            labels[match(x, c(cut_points, cut_points1))],
            as.character(x)
          )
        }
      )
  }


  new_df <- bind_rows(
    df %>% dplyr::select(Item, Estimated_state = Estimated_fstate, state = fstate, time = ftime, cost = 0),
    df %>% dplyr::select(Item, Estimated_state = Estimated_wstate, state = wstate, time = rtime, cost = cost),
  ) %>%
    arrange(Item, time) # Sort by Item first, then by time

  ConMat <- filter(new_df, state %in% fstate)
  ConMat <- confusionMatrix(as.factor(ConMat$Estimated_state), as.factor(ConMat$state))$overall[1]
  if (policy == 2) {
    cp <- c(cut_points, cut_points1)
  } else {
    cp <- cut_points
  }

  out <- list(Plot_original = p, Plot_Estimated = pe, EstimatedData = new_df, Acc = ConMat, CutPoints = cp)
  return(out)
}
