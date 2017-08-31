################################################################################
#                              Number of sessions                              #
################################################################################

plot.n_sessions <- function(data1, data2, data3, aggregator, size = 14) {
  # Plots the number of sessions of 3 data frames.
  #
  # Args:
  #   data1: data with all edges.
  #   data2: data only with edges with over 50 sessions. 
  #   data3: data only with edges with over 170 sessions.
  #   agregator: any lubridate agregator e.g: day,wday,hour. To group the data. 
  #   size: size of the plot font.
  #
  # Returns:
  #   A plot comparing the number of sessions between data1, data2 y data 3
  #   grouped by the aggregator. 
  
  data1 %>% 
    subset(way_id %in% data2$way_id) %>% 
    group_by(aggregator = aggregator(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    ggplot(aes(aggregator, sessions)) + 
    geom_line(mapping = aes(colour = ">50 sessions")) +
    geom_line(data = summarise(group_by(data1, aggregator = aggregator(time)),
                               sessions = n_distinct(session_id)), 
              aes(aggregator, sessions, colour = "All sessions")) +
    geom_line(data = summarise(group_by(data3, aggregator = aggregator(time)),
                               sessions = n_distinct(session_id)),
              aes(aggregator, sessions, colour = ">170 sessions")) +
    labs(colour="")  +
    ylab("# Sessions") +
    theme_bw(base_size = size ) 
}

################################################################################
#                                   Speed                                      #
################################################################################

plot.speed <- function(data1, data2, data3, aggregator, stat = mean,  
                            size = 14) {
  # Plots the number of sessions of 3 data frames.
  #
  # Args:
  #   data1: data with all edges.
  #   data2: data only with edges with over 50 sessions. 
  #   data3: data only with edges with over 170 sessions.
  #   agregator: any lubridate agregator e.g: day,wday,hour. To group the data. 
  #   stat: stat summary to apply the speed. E.g.: mean, median,etc.
  #   size: size of the plot font.
  #
  # Returns:
  #   A plot comparing the number of sessions between data1, data2 y data 3
  #   grouped by the aggregator. 
  
  data1 %>% 
    subset(way_id %in% data2$way_id) %>% 
    group_by(aggregator = aggregator(time)) %>% 
    summarise(speed = stat((speed*18)/5)) %>% 
    ggplot(aes(aggregator, speed)) + 
    geom_line(mapping = aes(colour = ">50 sessions")) +
    geom_line(data = summarise(group_by(data1, aggregator = aggregator(time)),
                               speed = stat((speed*18)/5)), 
              aes(aggregator, speed, colour = "All sessions")) +
    geom_line(data = summarise(group_by(data3, aggregator = aggregator(time)),
                               speed = stat((speed*18)/5)),
              aes(aggregator, speed, colour = ">170 sessions")) +
    labs(colour="")  +
    ylab("Avg speed(km/h)") +
    scale_y_continuous(breaks = seq(0,30,5)) + 
    theme_bw(base_size = size ) 
}
