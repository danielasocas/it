# Daniela S. Gil
# Timeseries individual draft

#######################################################################
#                    Individual plots                                 #
#######################################################################

#All speed points by way_id

# With plotly, consumes time and memory.
df_superhotedges_april16pt %>% 
  filter( way_id == 37141967 ) %>% 
  plot_ly(x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
  layout(title = paste("Time series of speed for:",37141967, sep = " " ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

# The average per day by way_id.
df_superhotedges_april16pt %>% 
  filter( way_id == 37141967 ) %>% 
  group_by(day = day(time)) %>% 
  summarise(speed = mean(speed)) %>% 
  plot_ly(x= ~day, y= ~(speed*18)/5) %>% 
  layout(title = paste("Avg speed per day for way_id",37141967, sep = " " ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

#Boxplots speed all.
df_superhotedges_april16pt %>% 
  filter(speed > 0) %>% 
  group_by(hour = hour(time)) %>% 
  plot_ly(x= ~factor(hour), y= ~(speed*18)/5, type = 'box',boxpoints = FALSE) %>% 
  layout(title = paste("Avg speed per hour" ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Hour"))

#---------------------------------#
#       number of sessions        #
#---------------------------------#

# By hour.
df_superhotedges_april16pt %>% 
  group_by(hour = hour(time)) %>% 
  summarise(sessions = n_distinct(session_id)) %>% 
  plot_ly(x= ~hour, y= ~sessions, type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Number of session per hour of the week April16",
         yaxis = list(title="#sessions"),
         xaxis = list(title="Hour"))

#By day of the week.
df_superhotedges_april16pt %>% 
  group_by(wday = wday(time, label = TRUE)) %>% 
  summarise(sessions = n_distinct(session_id)) %>% 
  plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Number of session per day of the week April16",
         yaxis = list(title="#sessions"),
         xaxis = list(title="Hour"))

#######################################################################
#                    Speed different                                  #
#######################################################################

#Base 
df_superhotedges_april16pt_speed_h <- df_superhotedges_april16pt %>% 
  group_by(way_id, session_id, hour(time)) %>% 
  summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) 

df_superhotedges_april16pt_speed %>% 
  group_by(way_id) %>% 
  summarise(speed = mean(avg_speed), median_speed = mean(median_speed)) %>% 
  plot_ly(x= ~factor(way_id), y= ~speed, type = 'scatter', mode = 'lines+markers',
          name = "Average") %>%
  layout(title = "New Avg speed per edge",
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Way_id")) %>% 
  add_trace(x= ~factor(way_id), y= ~median_speed, type = 'scatter', mode = 'lines+markers', 
            name = "Median")

#######################################################################
#                     More plots                                      #
#######################################################################


#----------- Avg speed Density ------------#

#Separate
df_superhotedges_april16pt %>%
  filter( way_id == df_osm_edge_ids[5,1], wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, session_id) %>% 
  summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
  plot_ly( x= ~avg_speed, type = 'histogram', name = "Avg speed April16") %>%
  layout(title = paste("",df_osm_edge_ids[5,2],df_osm_edge_ids[5,1], sep = " " ),
         xaxis = list(title="Speed (km/h)"),
         yaxis = list(title="Frequency")) %>% 
  add_trace(x= ~avg_speed, y= ~density(avg_speed))

### Facet grid
df_superhotedges_april16pt %>% 
  filter( wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, session_id, hour = hour(time)) %>% 
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  merge(y = df_osm_edge_ids, using = way_id) %>% 
  ggplot(aes(x=avg_speed, y=sd, color = hour)) +
  geom_point() +
  geom_smooth(aes(colour = avg_speed, fill = avg_speed)) +
  facet_wrap(~ address) + 
  ggtitle("By session per hour avg. speed April16")


### Facet grid
df_superhotedges_april16pt %>% 
  filter( wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, hour = hour(time)) %>%
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  merge(y = df_osm_edge_ids, using = way_id) %>% 
  ggplot(aes(x=avg_speed, y=sd, color = hour)) +
  geom_point() +
  geom_smooth(aes(colour = avg_speed, fill = avg_speed)) +
  facet_wrap(~ address) + 
  ggtitle("By hour avg. speed April16")


### Facet grid
df_superhotedges_april16pt %>% 
  filter( wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id,day = day(time), hour = hour(time)) %>% 
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  merge(y = df_osm_edge_ids, using = way_id) %>% 
  ggplot(aes(x=avg_speed, y=sd, color = hour)) +
  geom_point() +
  geom_smooth(aes(colour = avg_speed, fill = avg_speed)) +
  facet_wrap(~ address) + 
  ggtitle("By day and hour avg.speed")