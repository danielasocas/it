# Daniela S. Gil
# Timeseries

# With plotly, consumes time and memory.

df_superhotedges_april16pt %>% 
  filter( way_id == list_osm_edge[7] ) %>% 
  plot_ly(x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
  layout(title = paste("Time series of speed for way_id",list_osm_edge[7], sep = " " ),yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

# The average per day 

df_superhotedges_april16pt %>% 
  filter( way_id == list_osm_edge[2] ) %>% 
  group_by(day = day(time)) %>% 
  summarise(speed = mean(speed)) %>% 
  plot_ly(x= ~day, y= ~(speed*18)/5) %>% 
  layout(title = paste("Time series of avg speed per day for way_id",list_osm_edge[7], sep = " " ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

#######################################################################
#                     Timeseries functions                            #
#######################################################################

# Light, its fast. 

tseries_light <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == list_osm_edge[index], speed > 0 ) %>% 
    ggplot(aes(x= time, y= (speed*18)/5)) + 
    geom_point(aes(colour = class), alpha = 1/10) + 
    ylab("Speed(km/h)") + 
    xlab("Date")+ 
    ggtitle(paste("Time series of speed for way_id",list_osm_edge[index], sep = " " ))}

#lapply(1:2, FUN = tseries_light)

# Timeseries mean per day. 

tseries_avg <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == list_osm_edge[index], speed > 0 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~day, y= ~(speed*18)/5, type = 'scatter', mode = 'lines') %>% 
    layout(title = paste("Time series of avg speed per day for way_id",list_osm_edge[index], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Date"))}

lapply(1:3, FUN = tseries_avg)

#---------------------------
# Not
#---------------------------

#All at once, consumes too much memory and crashes. 

df_superhotedges_april16pt %>% 
  split(df_superhotedges_april16pt$way_id) %>% 
  lapply(function(df)  plot_ly(df, x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
           layout(title = "Time series of speed for way_id" ,yaxis = list(title="Speed (km/h)"),
                  xaxis = list(title="Date"))) %>% 
  subplot(nrows = NROW(.), shareX = TRUE)





















