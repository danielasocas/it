################################################################################
#                              Time conversion                              #
################################################################################

transform.time <- function(data) {
  # Transform the time UTC to posix with timezone Lisboa. 
  #
  # Args:
  #   data: data to use.
  #   utc: column with the time in format UTC. 
  #   feature: new column with time in posix .
  #
  # Returns:
  #   A new column time with the following format: year-month-day hour-min-sec
  
  data <- data %>% 
    mutate(time = as.POSIXct(data$seconds, origin="1970-01-01"))
  
  data$time
}

