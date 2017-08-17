# Daniela S. Gil
# Timeseries

#######################################################################
#                     Preparing data                                  #
#######################################################################
V1 <- c(219537531,35078500,35145521,316459169,37141967,41380172,208262837,41380173,37338733,63192525,106334851,37141970,37141970.2, 36979228)
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
"Rua D. Eduardo Santos Silva",
"Rua D. Eduardo Santos Silva 2")

V1 <- c(35078500,35145521,316459169,37141967,63192525,106334851,37141970,37141970.2,36979228)
V2 <- c("Rua Dr. Manuel Pereira Da Silva",
        "Rua D. Roberto Frias 1",
        "Rua D. Roberto Frias 2",
        "Rua Dr. Frei Vicente da Soledade e Castro",
        "Rua Henrique de Sousa Reis 1",
        "Rua Henrique de Sousa Reis 2",
        "Rua D. Eduardo Santos Silva",
        "Rua D. Eduardo Santos Silva 2",
        "Rua Dr. Placido da Costa")


df_osm_edge_ids <- data.frame(matrix(c(V1,V2), nrow = 9))
colnames(df_osm_edge_ids) <- c("way_id", "address")
df_osm_edge_ids <-data.frame(matrix(c(list_osm_edge,superhot_edges_names), nrow=13))

df_osm_edge_ids <- df_osm_edge_ids[c(2:5,10:13),]

df_osm_edge_ids <- df_osm_edge_ids %>% 
  mutate(type = if_else( way_id == 35078500 | way_id == 37141967 | way_id == 63192525 | way_id == 106334851 , "h", "v"))


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
  plot_ly(x= ~factor(hour), y= ~(speed*18)/5, type = 'box',boxpoints = FALSE) %>% 
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
         yaxis = list(title="#sessions"),
         xaxis = list(title="Hour"))

df_superhotedges_april16pt %>% 
  group_by(wday = wday(time, label = TRUE)) %>% 
  summarise(sessions = n_distinct(session_id)) %>% 
  plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Number of session per day of the week April16",
         yaxis = list(title="#sessions"),
         xaxis = list(title="Hour"))

#######################################################################
#                     Timeseries functions                            #
#######################################################################


#########################   Speed (Average of all points)   ########################
# Light, its fast. Shows every point with class time as color.  
tseries_light <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    ggplot(aes(x= time, y= (speed*18)/5)) + 
    geom_point(aes(colour = class), alpha = 1/10) + 
    ylab("Speed(km/h)") + 
    xlab("Date")+ 
    ggtitle(paste("Time series of speed for way_id:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ))}

#lapply(1:2, FUN = tseries_light)

# Timeseries mean per hour. 
tseries_avg_h <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0, wday(time) != 1 , wday(time) != 7  ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~factor(hour), y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per hour (WW)") %>%
    layout(title = paste("Avg speed per hour",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Km/h"),
           xaxis = list(title="Hour without weekends"))}

# Timeseries mean per week. 
tseries_avg_wd <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~wday, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per d/week") %>% 
    layout(title = paste("Avg speed per day of the week:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Km/h"),
           xaxis = list(title="Day of the week"))}

# Timeseries mean per day. 
tseries_avg_d <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], speed > 0 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(speed = mean(speed)) %>% 
    plot_ly(x= ~day, y= ~(speed*18)/5, type = 'scatter', mode = 'lines+markers', name = "Speed per day") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
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

lapply(1:2, FUN = tseries)


#########################   Number sessions   ########################

# Number sessions per hour. 
tseries_nsessions_h <- function(index) { df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~factor(hour), y= ~sessions, type = 'scatter', mode = 'lines+markers',name = "#sessions per hour (WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#SS"),
           xaxis = list(title="Hour without weekends"))
}

# Number sessions per day of the week. 
tseries_nsessions_wd <- function(index) { df_superhotedges_april16pt %>% 
    filter(way_id == df_osm_edge_ids[index,1]) %>% 
    group_by(wday = wday(time, label = TRUE)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~wday, y= ~sessions, type = 'scatter', mode = 'lines+markers', name = "#sessions per d_week") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="#SS"),
           xaxis = list(title="Day of the week"))}

# Number sessions per day. 
tseries_nsessions_d <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1] ) %>% 
    group_by(day = day(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
    plot_ly(x= ~day, y= ~sessions, type = 'scatter', mode = 'lines+markers', name = "#sessions per day") %>% 
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

lapply(1:2, FUN = tseries_nsession)


#########################   Intersession time   ########################

#Intersession time per hour. 
inter_h <- function(index) { df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(hour = hour(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~factor(hour), y= ~intersession_t, type = 'scatter', mode = 'lines+markers', name = "Intersession per day (WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Min"),
           xaxis = list(title="Hour"))}
lapply(1, FUN = inter_h)

#Intersession time per day. 
inter_d <- function(index) { df_intersession_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(day = day(time)) %>% 
    summarise(intersession_t = mean(intersession_time)) %>% 
    plot_ly(x= ~day, y= ~intersession_t, type = 'scatter', mode = 'lines+markers', name = "Intersession per day(WW)") %>% 
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Min"),
           xaxis = list(title="Day"))}

lapply(1:12, FUN = inter_d)

tseries_inter <- function(index) {
  i1 <- inter_h(index)
  i2 <- inter_d(index) 
  subplot(i1,i2,nrows = 2, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}
lapply(1, FUN = inter)

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

lapply(1:2, FUN = tseries)

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
    plot_ly(x= ~factor(way_id), y= ~speed, type = 'scatter', mode = 'lines+markers',name = "Average") %>%
    layout(title = "New Avg speed per edge",
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Way_id")) %>% 
    add_trace(x= ~factor(way_id), y= ~median_speed, type = 'scatter', mode = 'lines+markers', name = "Median")


# New speed per hour. 
newspeed_avg_h <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, hour = hour(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(hour), y= ~avg_speed,color= ~heading , type = 'box', name = "Avg speed per hour (WW)") %>%
    layout(title = paste("Avg speed per hour",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)", range(0,100)),
           xaxis = list(title="Hour without weekends", range(0,24)))
}

lapply(1:2, FUN = newspeed_avg_h)

# New speed per week. 
newspeed_avg_wd <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, wday = wday(time, label = T)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(wday), y= ~avg_speed, type = 'box', name = "Avg speed day of the week(WW)") %>%
    layout(title = paste("Avg speed per day of the week:",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day of the week"))} 

lapply(1, FUN = newspeed_avg_wd)

# New speed per day. 
newspeed_avg_d <- function(index) { df_superhotedges_april16pt %>% 
    filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id,heading, day = day(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(day), y= ~avg_speed, type = 'box', name = "Avg speed day April16 (WW)") %>%
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)"),
           xaxis = list(title="Day")) 
}

lapply(1:2, FUN = newspeed_avg_d)

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

lapply(1, FUN = newspeed_avg)

#######################################################################
#                     more plots                                      #
#######################################################################

 
#--- Avg speed Density

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

#------------------ Avg speed / #sd
##--- Per session
sd_session <- function(index) {  df_superhotedges_april16pt %>% 
  filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, session_id, hour = hour(time)) %>% 
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  plot_ly( x= ~avg_speed, y= ~sd,color = ~hour, type= 'scatter' , name = "Per session ") %>%
  layout(title = paste("By session per hour avg. speed April16",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
         xaxis = list(title="Speed (km/h)"),
         yaxis = list(title="Standard deviation")) 
}


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


##--- Per hour
sd_h <- function(index) {   df_superhotedges_april16pt %>% 
  filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, hour = hour(time)) %>% 
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  plot_ly( x= ~avg_speed, y= ~sd, color = ~hour, type= 'scatter' , name = "Per hour") %>%
  layout(title = paste("By hour avg. speed",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
         xaxis = list(title="Speed (km/h)"),
         yaxis = list(title="Standard deviation"))
}

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


##--- Per day and hour
sd_day_h <-  function(index) { df_superhotedges_april16pt %>% 
  filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id,day = day(time), hour = hour(time)) %>% 
  summarise(avg_speed = mean(speed*18/5), sd = sd(speed)) %>% 
  plot_ly( x= ~avg_speed, y= ~sd,color = ~hour, type= 'scatter' , name = "Per day and hour") %>%
  layout(title = paste("By day and hour avg.speed",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
         xaxis = list(title="Average Speed (km/h)"),
         yaxis = list(title="Standard deviation"))
}

lapply(1, FUN = sd_day_h)

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

#----------------Function all----------------#


#Per edge

sd <- function(index) {
  sd1 <- sd_day_h(index)
  sd2 <- sd_h(index) 
  sd3 <- sd_session(index)
  subplot(sd1,sd2,sd3,nrows = 3, shareY = T)%>% 
    layout(legend= list(orientation= 'h'))
}

lapply(1, FUN = sd)


#--- Avg speed / #sessions
speed_s <-  function(index) { df_superhotedges_april16pt %>% 
  filter( way_id == df_osm_edge_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
  group_by(way_id, session_id, heading, hour = hour(time)) %>% 
  summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
  plot_ly(x = ~avg_speed,color=~heading,  type = 'histogram', name = "Avg speed April16") %>%
    layout(title = paste("",df_osm_edge_ids[index,2],df_osm_edge_ids[index,1], sep = " " ),
           xaxis = list(title="Average Speed (km/h)"),
           yaxis = list(title="Number of sessions"))
}

lapply(1, FUN = speed_s)

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

## Low speed and Parkings

df_superhotedges_april16pt %>% 
  group_by(way_id, session_id, segment_id) %>% 
  summarise(iqr  = IQR(track), m= mean(speed) ) %>% 
  filter( iqr == 0 & m== 0) %>% 
  group_by(way_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

df_superhotedges_april16pt %>% 
  subset(way_id == 106334851  ) %>% 
  group_by(session_id) %>% 
  summarise(median(track),iqr = IQR(track), mean(speed*18/5), median(speed*18/5),  quantile(speed*18/5, 0.75)) %>% 
  filter(iqr >100)
  
  
df_superhotedges_april16pt %>% 
  subset(way_id == 106334851 &(session_id == 16665 |session_id == 45313 | session_id == 21481) )

df_superhotedges_april16pt %>% 
  group_by(way_id, session_id, segment_id) %>% 
  summarise(iqr  = IQR(track), m= mean(speed) ) %>% 
  filter( iqr > 100 ) %>% 
  group_by(way_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

df_superhotedges_april16pt %>% 
  group_by(way_id, session_id) %>% 
  summarise(med_tr =median(track),iqr = IQR(track), mean= mean(speed*18/5),median =  median(speed*18/5),q75=  quantile(speed*18/5, 0.75)) %>% 
  merge(y= df_osm_edge_ids) %>% 
  ggplot(aes(x=iqr, fill = median)) +
  geom_histogram(binwidth = 20) + 
  facet_wrap(~type~address)

# Theme config 

df_intersession_april16pt %>% 
  filter(intersession_time > 0 ) %>% 
  merge(df_sessions_edges_april16pt, using = way_id) %>% 
  ggplot( aes(x=intersession_time, color = category))+
  scale_x_log10(limits = c(1e0,1e4)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf') +
  scale_color_manual(values = c("grey","blue","red","green"),
                     breaks = c("SHE", "medium", "low","NA"),
                     labels = c("High", "Medium", "Low","NA")) +
  theme(legend.title = element_blank(),
        legend.text = element_text( size = 20, face = "bold"),
        legend.position = c(0.8,0.8),
        axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=20),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  xlab("Intersession time (min)") + 
  ylab("P(X>=x)") +
  geom_vline(xintercept =  c(15,30,45,60), color ="dark grey")


#Travelmode
df_sessions_allmodes_april16pt %>% 
  filter(travelmode != "NA") %>% 
  group_by(hour = hour(time_start), travelmode, total_sessions) %>% 
  summarise(n_sessions = n_distinct(session_id)) %>% 
  mutate(freq = n_sessions/total_sessions *100) %>% 
  ggplot(aes(x = hour,y = freq, color=travelmode)) +
  geom_line() + 
  ylab("Frequency(%)") +
  xlab("Hour") +
  scale_x_continuous(breaks = seq(0,23,1)) + 
  scale_color_discrete(name = "Travelmode") +
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16),
        legend.title = element_text("Street", size=1, face="bold"), 
        legend.text = element_text(size=18, face="bold"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = c(0.2,0.7))

# Nº sessions 
df_st_hotedges_april16pt %>% 
  mutate(class_day = case_when(wday(time_start)!= 1 &  wday(time_start)!= 7 ~ "Weekday",
                               TRUE ~ "Weekend")) %>% 
  ggplot(aes(x=hour(time_start))) + 
  geom_bar(aes(fill = class_trip )) + 
  scale_fill_hue(breaks = c("1", "2", "3","4"),
                 labels = c("From campus", "To campus", "Outside campus","Within campus"))+
  theme(legend.title = element_blank(),
        legend.text = element_text( size = 20, face = "bold"),
        legend.position = c(0.75,0.8),
        axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  xlab("Hour") + 
  ylab("# of sessions") +
  facet_grid(~as.factor(class_day)) + 
  theme(strip.text.x = element_text(size = 20))

