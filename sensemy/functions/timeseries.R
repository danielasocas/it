# Daniela S. Gil
# Timeseries

#######################################################################
#                     Timeseries functions                            #
#######################################################################

##############   Speed (Avg. of session and then by period)   ##################

plot.timeseries <- function(index, data = df_superhotedges_april16pt, 
                            aggregator = wday, period = "30 mins" ) {
  # Plots the speed timeserie of an edge. Averages the speed by session and then
  # by time. 
  #
  # Args:
  #   index: the edge of the plot. 
  #   data: data with all edges.
  #   agregator: any lubridate agregator e.g: day,wday,hour. To group the data. 
  #   period: the breaking period time of the timeseri.
  #
  # Returns:
  #   A plot with the timeseries colored but the hour and shaped by the day.  
  
  data %>%
    filter(way_id == df_osm_edge_ids[index,1] ) %>% 
    group_by(session_id,heading, period = cut(time, breaks=period)) %>%
    summarise(session_speed = mean(speed*18/5)) %>%  
    group_by(heading, period) %>%
    summarise(avg_speed = mean(session_speed))  %>% 
    ggplot(aes(period, avg_speed)) + 
    geom_point(aes(colour = factor(wday(period)))) +
    ggtitle(paste("Avg. speed:",df_osm_edge_ids[index,2],
                  df_osm_edge_ids[index,1], sep = " " )) +
    ylab("Avg. speed (km/h)") + 
    xlab("Half hour period") +
    facet_wrap(~heading, nrow = 2) +
    theme_light( )
}

#-----------   Grouped by hour,day and weekday   ------------ #

plot.speed_avg <- function( data = df_superhotedges_april16pt, 
                            aggregator = c(hour, wday, day) ) {
  # Plots the speed timeserie of an edge. Averages the speed by session and then
  # by time. 
  #
  # Args:
  #   index: the edge of the plot. 
  #   data: data with all edges.
  #   agregator: any lubridate agregator e.g: day,wday,hour. To group the data. 
  #   period: the breaking period time of the timeseri.
  #
  # Returns:
  #   A boxplot with the timeseries colored but the hour .  
  
data %>%  
  merge(df_osm_edge_ids) %>% 
  group_by(address, session_id, heading, aggregator = aggregator(time)) %>% 
  summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
  ggplot(aes(factor(aggregator),avg_speed, colour = heading)) + 
  geom_boxplot() + 
  facet_wrap(~ address, nrow = 4) 
}


#########################   Speed (Average of all points)   ####################

# Light, its fast. Shows every point with class time as color.  
tseries_light <- function(index) { 
  df_superhotedges_april16pt_all %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    ggplot(aes(x= time, y= (speed*18)/5)) + 
    geom_point(aes(colour = class), alpha = 1/10) + 
    ylab("Speed(km/h)") + 
    xlab("Date")+ 
    ggtitle(paste("All points for way_id:",df_osm_edge_ids[index,2],
                  df_osm_edge_ids[index,1], sep = " " ))}

lapply(1:2, FUN = tseries_light)

#--------------------------------------------------------------------------------

# Timeseries mean per hour. 
tseries_avg_h <- function(index, data = df_superhotedges_april16pt) { 
  data %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0, wday(time) != 1 , wday(time) != 7  ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~factor(hour), y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', 
            name = "Speed per hour (WW)") %>%
    layout(title = paste("Avg speed per hour",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Km/h"),
           xaxis = list(title="Hour without weekends"))}

# Timeseries mean per week. 
tseries_avg_wd <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~wday, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers',
            name = "Speed per d/week") %>% 
    layout(title = paste("Avg speed per day of the week:",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Km/h"),
           xaxis = list(title="Day of the week"))}

# Timeseries mean per day. 
tseries_avg_d <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~day, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', 
            name = "Speed per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Km/h"),
           xaxis = list(title="Day"))}


#---------------------------------#
#          Function all           #
#---------------------------------#

tseries_speed <- function(index) {
  p1 <- tseries_avg_h(index) 
  p2 <- tseries_avg_wd(index)
  p3 <- tseries_avg_d(index)
  
  subplot(p2,p1,p3, nrows = 3, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}

#lapply(1:2, FUN = tseries)


#########################   Number sessions   ########################

# Number sessions per hour. 
tseries_nsessions_h <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~factor(hour), y= ~sessions, type = 'scatter', mode = 'lines+markers', 
            name = "#sessions per hour (WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#SS"),
           xaxis = list(title="Hour without weekends"))
}

# Number sessions per day of the week. 
tseries_nsessions_wd <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1]) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers', 
            name = "#sessions per d_week") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#SS"),
           xaxis = list(title="Day of the week"))}

# Number sessions per day. 
tseries_nsessions_d <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    group_by(day = day(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~day, y= ~sessions, type = 'scatter', mode = 'lines+markers', 
            name = "#sessions per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#SS"),
           xaxis = list(title="Day"))}

#---------------------------------#
#          Function all           #
#---------------------------------#

#Per edge

tseries_nsession <- function(index) {
  m1 <- tseries_nsessions_h(index)
  m2 <- tseries_nsessions_wd(index) 
  m3 <- tseries_nsessions_d(index)
  subplot(m1,m2,m3,nrows = 3, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}

##lapply(1:2, FUN = tseries_nsession)


#########################   Intersession time   ########################

#Intersession time per hour. 
inter_h <- function(index) { 
  df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~factor(hour), y= ~intersession_t, type = 'scatter', mode = 'lines+markers', 
            name = "Intersession per day (WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Min"),
           xaxis = list(title="Hour"))}
#lapply(1, FUN = inter_h)

#Intersession time per day. 
inter_d <- function(index) { 
  df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~day, y= ~intersession_t, type = 'scatter', mode = 'lines+markers',
            name = "Intersession per day(WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Min"),
           xaxis = list(title="Day"))}

#lapply(1:12, FUN = inter_d)

tseries_inter <- function(index) {
  i1 <- inter_h(index)
  i2 <- inter_d(index) 
  subplot(i1,i2,nrows = 2, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}
#lapply(1, FUN = inter)

#########################   Altogether   ########################

#---------------------------------#
#            function             #
#---------------------------------#

tseries <- function(index) {
  p1 <- tseries_avg_h(index)
  p2 <- tseries_avg_wd(index)
  p3 <- tseries_avg_d(index)
  
  m1 <- tseries_nsessions_h(index)
  m2 <- tseries_nsessions_wd(index) 
  m3 <- tseries_nsessions_d(index)
  
  i1 <- inter_h(index)
  i2 <- inter_d(index)
  subplot(p1,p2,p3,m1,m2,m3,i1,i2,nrows = 8, shareY = T) %>% 
    layout(legend= list(orientation= 'h'))}

#lapply(1:2, FUN = tseries)

####################   New Speed (Average of all points)   ####################

# New speed per hour. 
newspeed_avg_h <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, hour = hour(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(hour), y= ~avg_speed,color= ~heading , type = 'box', 
            name = "Avg speed per hour (WW)") %>%
    layout(title = paste("Avg speed per hour",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)", range(0,100)),
           xaxis = list(title="Hour without weekends", range(0,24)))
}

#lapply(1:2, FUN = newspeed_avg_h)

# New speed per week. 
newspeed_avg_wd <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, wday = wday(time, label = T)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(wday), y= ~avg_speed, type = 'box', name = "Avg speed day of the week(WW)") %>%
    layout(title = paste("Avg speed per day of the week:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day of the week"))} 

#lapply(1, FUN = newspeed_avg_wd)

# New speed per day. 
newspeed_avg_d <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, day = day(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(day), y= ~avg_speed, type = 'box', name = "Avg speed day April16 (WW)") %>%
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day")) 
}

#lapply(1:2, FUN = newspeed_avg_d)

#---------------------------------#
#          Function all           #
#---------------------------------#

#Per edge

newspeed_avg <- function(index) {
  n1 <- newspeed_avg_h(index)
  n2 <- newspeed_avg_wd(index) 
  n3 <- newspeed_avg_d(index)
  subplot(n1,n2,n3,nrows = 3, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}

#lapply(1, FUN = newspeed_avg)

#######################################################################
#                     More plots                                      #
#######################################################################

 
#----------- Avg speed Density ------------#
  
#Density lines average speed all together
prueba <- df_superhotedges_april16pt %>%
  filter( wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, session_id) %>% 
  summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) 

factor(prueba$way_id)

dens <-prueba %>% 
  merge(y = df_osm_edge_ids, using = way_id) %>% 
  ggplot(aes(x= avg_speed)) +
  geom_density(aes(group=factor(address), color=address)) + 
  ggtitle("Density of average speed per edge(without weekends)") + 
  xlab("Average speed(km/h)") +
  ylab("Density") 

ggplotly(dens)  

#----------- Avg speed - Standard deviation ------------#

## Per session
sd_session <- function(index) {  
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, hour = hour(time)) %>% 
    summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
    plot_ly( x= ~avg_speed, y= ~sd,color = ~hour, type= 'scatter' , 
             name = "Per session ") %>%
    layout(title = paste("By session per hour avg. speed April16",df_osm_edge_ids[index,2], 
                         df_osm_edge_ids[index,1], sep = " " ),
           xaxis = list(title="Speed (km/h)"),
           yaxis = list(title="Standard deviation")) 
}


## Per hour
sd_h <- function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, hour = hour(time)) %>% 
    summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
    plot_ly( x= ~avg_speed, y= ~sd, color = ~hour, type= 'scatter' , 
             name = "Per hour") %>%
    layout(title = paste("By hour avg. speed",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           xaxis = list(title="Speed (km/h)"),
           yaxis = list(title="Standard deviation"))
}



## Per day and hour
sd_day_h <-  function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id,day = day(time), hour = hour(time)) %>% 
    summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
    plot_ly( x= ~avg_speed, y= ~sd,color = ~hour, type= 'scatter' , name = "Per day and hour") %>%
    layout(title = paste("By day and hour avg.speed",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           xaxis = list(title="Average Speed (km/h)"),
           yaxis = list(title="Standard deviation"))
}

#lapply(1, FUN = sd_day_h)

#----------------Function all----------------#
#Per edge

sd <- function(index) {
  sd1 <- sd_day_h(index)
  sd2 <- sd_h(index) 
  sd3 <- sd_session(index)
  subplot(sd1,sd2,sd3,nrows = 3, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}

#lapply(1, FUN = sd)


#----------- Avg speed - # sessions ------------#
speed_s <-  function(index) { 
  df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, hour = hour(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x = ~avg_speed,color=~heading,  type = 'histogram', name = "Avg speed April16") %>%
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           xaxis = list(title="Average Speed (km/h)"),
           yaxis = list(title="Number of sessions"))
}

#lapply(1, FUN = speed_s)

#######################################################################
#                     Interactive but slow                            #
#######################################################################

#All at once, consumes too much memory and crashes. 

# df_superhotedges_april16pt %>% 
#   split(df_superhotedges_april16pt$way_id) %>% 
#   #lapply(function(df)  plot_ly(df, x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
#            layout(title = "Time series of speed for way_id" ,yaxis = list(title="Speed (km/h)"),
#                   xaxis = list(title="Date"))) %>% 
#   subplot(nrows = NROW(.), shareX = TRUE)
# 

