####################   New Speed (Average of all points)   ####################

# New speed per hour. 
  
newspeed_avg <- function(index, aggregator = wday, 
                      data = df_superhotedges_april16pt, data_ids = df_osm_edge_ids ) { 
  data %>%   
    filter( way_id == data_ids[index,1], wday(time) != 1 , wday(time) != 7 ) %>% 
    group_by(way_id, session_id, heading, aggregator = aggregator(time)) %>% 
    summarise(median_speed = median(speed*18/5), avg_speed = mean(speed*18/5)) %>% 
    plot_ly(x= ~factor(aggregator), y= ~avg_speed, color= ~heading , type = 'box', 
            name = "Avg speed (WW)") %>%
    layout(title = paste("Avg speed per ",df_osm_edge_ids[index,2],
                         df_osm_edge_ids[index,1], sep = " " ),
           yaxis = list(title="Speed (km/h)", range(0,100)),
           xaxis = list(title= "without weekends"))
}

lapply(1:2, FUN = speed_avg, aggregator = wday)


#---------------------------------#
#          Function all           #
#---------------------------------#

#Per edge

speed_avg_all <- function(index, 
                          data = df_superhotedges_april16pt, data_ids = df_osm_edge_ids) {
  n1 <- newspeed_avg(index, hour)
  n2 <- newspeed_avg(index, wday) 
  n3 <- newspeed_avg(index, day)
  n1
  n2
  n3
  # subplot(n1,n2,n3,nrows = 3, shareY = T)%>% 
  #   layout(legend= list(orientation= 'h'))
}

#lapply(1, FUN = newspeed_avg)