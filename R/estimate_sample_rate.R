# This is a utility function for estimating the sample_rate in Hz based off the timestamps given.


.estimate_sample_rate <- function(data) {

  # estimate sample rate

    trial <- split(data, data$trial)
    sample_rates <- sapply(trial, function(data) {

      # estimate sample rate (ms) from difference between timestamps
      time <- data$time - data$time[1] # start trial timestamps at 0
      sample_rate <- mean(diff(time)) #difference between timestamps, expressing ms per sample
      sample_rate

    })
    #average sample rate across all trials
    sample_rate <- 1000/mean(sample_rates)
    sample_rate
  }
