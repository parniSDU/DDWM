#' Pre processes a data set
#'
#' For a given data set, this function pre processes the data for .
#'
#' @param data a data frame containing the data set
#' @param W a numeric value as the warranty period
#'
#' @return a data frame processed data

PreData <- function(data, W) {
  wstate <- paste("WorkingState ", 1:4, sep = "")
  fstate <- paste("FailureState ", 1:4, sep = "")
  states <- c(wstate, fstate)

  flist <- NULL
  cost <- numeric()
  t <- W
  SimSize <- length(unique(data$Item))

  for (i in 1:SimSize) {
    x <- data[data$Item == i, ]
    x <- x[x$time <= t, ]
    cost[i] <- sum(x$cost)

    findex <- which(x$state %in% fstate)


    xf <- x[findex, ]

    if (nrow(xf) > 0) {
      df <- x %>%
        mutate(
          state_type = case_when(
            state %in% wstate ~ "w",
            state %in% fstate ~ "f",
            TRUE ~ NA_character_
          )
        )

      tf <- xf[1, "time"]
      df <- df %>% filter(df$time >= tf)
      matching_rows <- list()
      for (j in 1:(nrow(df) - 1)) {
        window <- df[j:(j + 1), ]
        type_sequence <- paste(window$state_type, collapse = "")

        # Check for the exact pattern
        if (type_sequence == "fw") {
          matching_rows[[length(matching_rows) + 1]] <- window
        }
      }
      df <- bind_rows(matching_rows)

      flist <- rbind(flist, df)
    }
  }
  return(flist)
}
