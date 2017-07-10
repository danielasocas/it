# Daniela S. Gil
# Timeseries

#######################################################################
#                     Preparing data                                  #
#######################################################################
V1 <- c(219537531,35078500,35145521,316459169,37141967,41380172,208262837,41380173,37338733,63192525,106334851,37141970)
V2 <- c("Rua da Igreja de Paranhos",
"Rua Doutor Manuel Pereira Da Silva",
"Rua D. Roberto Frias 1",
"Rua D. Roberto Frias 2",
"Rua D. Frei Vicente da Soledade e Castro",
"Parking pelo Pque Quinta de Lamas",
"Parking FEUP Norte",
"Parking Biblioteca 2",
"Parking Biblioteca 1",
"Rua Henrique de Sousa Reis 1",
"Rua Henrique de Sousa Reis 2",
"Rua D. Eduardo Santos Silva")

df_osm_edge_ids <- data.frame(matrix(c(V1,V2), nrow = 12))
df_osm_edge_ids <-data.frame(matrix(c(list_osm_edge,superhot_edges_names), nrow=12))
colnames(df_osm_edge_ids) <- c("way_id", "address")

#######################################################################
#                    Individual plots                                 #
#######################################################################

#All speed points by way_id
# With plotly, consumes time and memory.

df_superhotedges_april16pt %>% 
  filter( way_id == 37141967 ) %>% 
  plot_ly(x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
  layout(title = paste("Time series of speed for:",37141967, sep = " " ),yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

# The average per day by way_id

df_superhotedges_april16pt %>% 
  filter( way_id == 37141967 ) %>% 
  group_by(day = day(time)) %>% 
  summarise(speed = mean(speed)) %>% 
  plot_ly(x= ~day, y= ~(speed*18)/5) %>% 
  layout(title = paste("Avg speed per day for way_id",37141967, sep = " " ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Date"))

#Boxplots speed all 
df_superhotedges_april16pt %>% 
  filter(speed > 0) %>% 
  group_by(hour = hour(time)) %>% 
  plot_ly(x= ~hour, y= ~(speed*18)/5, type = 'box',boxpoints = FALSE) %>% 
  layout(title = paste("Avg speed per hour" ),
         yaxis = list(title="Speed (km/h)"),
         xaxis = list(title="Hour"))

#---------------------------------#
#       number of sessions        #
#---------------------------------#

df_superhotedges_april16pt %>% 
  group_by(hour = hour(time)) %>% 
  summarise(sessions = n_distinct(session_id)) %>% 
  plot_ly(x= ~hour, y= ~sessions, type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Number of session per hour of the week April16",
         yaxis = list(title="Number of sessions"),
         xaxis = list(title="Hour"))

df_superhotedges_april16pt %>% 
  group_by(wday = wday(time, label = TRUE)) %>% 
  summarise(sessions = n_distinct(session_id)) %>% 
  plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Number of session per day of the week April16",
         yaxis = list(title="Number of sessions"),
         xaxis = list(title="Hour"))

#######################################################################
#                     Timeseries functions                            #
#######################################################################

#########################   Speed   ########################

# Light, its fast. 
tseries_light <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    ggplot(aes(x= time, y= (speed*18)/5)) + 
    geom_point(aes(colour = class), alpha = 1/10) + 
    ylab("Speed(km/h)") + 
    xlab("Date")+ 
    ggtitle(paste("Time series of speed:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ))}

#lapply(1:2, FUN = tseries_light)

# Timeseries mean per hour. 
tseries_avg_h <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0, wday(time) != 1 , wday(time) != 7  ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~hour, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per hour") %>%
    layout(title = paste("Avg speed per hour",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Hour without weekends"))}

# Timeseries mean per week. 
tseries_avg_wd <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~wday, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per d/week") %>% 
    layout(title = paste("Avg speed per day of the week:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day of the week"))}

# Timeseries mean per day. 
tseries_avg_d <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~day, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day"))}


#---------------------------------#
#          Function all           #
#---------------------------------#

tseries_speed <- function(index) {
  p1 <- tseries_avg_h(index) 
  p2 <- tseries_avg_wd(index)
  p3 <- tseries_avg_d(index)

  subplot(p2,p1,p3, nrows = 3, shareY = T)
}

lapply(1:2, FUN = tseries)


#########################   Number sessions   ########################

# Timeseries mean per hour. 
tseries_nsessions_h <- function(index) { df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~hour, y= ~sessions, type = 'scatter', mode = 'lines+markers',name = "#sessions per hour") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#sessions"),
           xaxis = list(title="Hour without weekends"))
}

# Timeseries mean per day of the week. 
tseries_nsessions_wd <- function(index) { df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1]) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers', name = "#sessions per d_week") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#sessions"),
           xaxis = list(title="Day of the week"))}

# Timeseries mean per day. 
tseries_nsessions_d <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    group_by(day = day(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~day, y= ~sessions, type = 'scatter', mode = 'lines+markers', name = "#sessions per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#sessions"),
           xaxis = list(title="Day"))}

#---------------------------------#
#          Function all           #
#---------------------------------#

#Per Rua

tseries_nsession <- function(index) {
  m1 <- tseries_nsessions_h(index)
  m2 <- tseries_nsessions_wd(index) 
  m3 <- tseries_nsessions_d(index)
  subplot(m1,m2,m3,nrows = 3, shareY = T)
}

lapply(1:2, FUN = tseries_nsession)


#########################   Intersession time   ########################

inter_h <- function(index) { df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~hour, y= ~intersession_t, type = 'scatter', mode = 'lines+markers', name = "Intersession per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="intersession time(min)"),
           xaxis = list(title="Hour"))}

inter_d <- function(index) { df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~day, y= ~intersession_t, type = 'scatter', mode = 'lines+markers', name = "Intersession per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="intersession time (min)"),
           xaxis = list(title="Day"))}

#lapply(1:12, FUN = inter_h)

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
  subplot(p1,m1,i1,p2,m2,p3,m3,i2,nrows = 6, shareY = T)
}

lapply(1:2, FUN = tseries)



#######################################################################
#                     Interactive but slow                            #
#######################################################################

#All at once, consumes too much memory and crashes. 

df_superhotedges_april16pt %>% 
  split(df_superhotedges_april16pt$way_id) %>% 
  lapply(function(df)  plot_ly(df, x= ~time, y= ~(speed*18)/5, color = ~class) %>% 
           layout(title = "Time series of speed for way_id" ,yaxis = list(title="Speed (km/h)"),
                  xaxis = list(title="Date"))) %>% 
  subplot(nrows = NROW(.), shareX = TRUE)


####################################################################





