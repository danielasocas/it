# Daniela S. Gil
# Top edges setting

#######################################################################
#                         Edges info                                  #
#######################################################################
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

df_osm_edge_ids <- df_osm_edge_ids %>% 
  mutate(type = if_else( way_id == 35078500 | way_id == 37141967 | 
                           way_id == 63192525 | way_id == 106334851 , "h", "v"))


#######################################################################
#                            Course                                   #
#######################################################################
# 1: to feup and 2: to out feup. 

df_speed_top$course <- 1

#Intersection Eduardo Silva with H.Sousa to separe it in two.
intersection <- 41.17755

#Vertical
#Rua Roberto Frias 1 & Eduardo Silva 1
df_speed_top[((df_speed_top$way_id == 37141970 & df_speed_top$lat < intersection) | 
                df_speed_top$way_id == 35145521 ) & (ave(df_speed_top$track,df_speed_top$session_id , 
                                                         df_speed_top$segment_id, FUN = median )) > 90 &
               (ave(df_speed_top$track,df_speed_top$session_id , 
                    df_speed_top$segment_id, FUN = median )) < 270,]$course <- 2

#Rua Roberto Frias 2 & Eduardo Silva 2
df_speed_top[((df_speed_top$way_id == 37141970 & df_speed_top$lat > intersection) |
                df_speed_top$way_id == 316459169) & (ave(df_speed_top$track,df_speed_top$session_id , 
                                                         df_speed_top$segment_id, FUN = median ) < 90) | 
               (ave(df_speed_top$track,df_speed_top$session_id , 
                    df_speed_top$segment_id, FUN = median ) > 270),]$course <- 2  


#Horizontals
#Rua H. Sousa 1 & H. Sousa 2 
df_speed_top[(df_speed_top$way_id == 63192525 | df_speed_top$way_id == 106334851) &   
               (ave(df_speed_top$track,df_speed_top$session_id , 
                    df_speed_top$segment_id, FUN = median )) < 180,]$course <- 2

#Rua Manuel Silva. 
df_speed_top[df_speed_top$way_id == 35078500  & (ave(df_speed_top$track,df_speed_top$session_id , 
                                                     df_speed_top$segment_id, FUN = median ) > 180),]$course <- 2
#Rua  Placido da Costa
# df_speed_top[df_speed_top$way_id == 35078500  & (ave(df_speed_top$track,df_speed_top$session_id , 
#                                                      df_speed_top$segment_id, FUN = median ) > 180),]$course <- 2

df_speed_top[df_speed_top$way_id == 35078500,]$course <- 2

# Heading direction. 
df_speed_top <- df_speed_top %>% 
  mutate( heading = case_when((way_id == 37141970 | way_id == 35145521) &  course == 2 ~ "S", 
                              (way_id == 37141970 | way_id == 35145521) &  course == 1 ~ "N",
                              (way_id == 37141970.2 | way_id == 316459169) &  course == 2 ~ "N", 
                              (way_id == 37141970.2 | way_id == 316459169) &  course == 1 ~ "S",
                              (way_id == 63192525 | way_id == 106334851) &  course == 2 ~ "E", 
                              (way_id == 63192525 | way_id == 106334851) &  course == 1 ~ "W",
                              way_id == 35078500  &  course == 2 ~ "W", 
                              way_id == 35078500  &  course == 1 ~ "E",
                              way_id == 36979228  &  course == 2 ~ "W", 
                              way_id == 36979228  &  course == 1 ~ "E",
                              way_id == 37141967  &  (ave(df_speed_top$track,df_speed_top$session_id , 
                                                          df_speed_top$segment_id, FUN = median ) > 180) ~ "W",
                              way_id == 37141967  &  (ave(df_speed_top$track,df_speed_top$session_id , 
                                                          df_speed_top$segment_id, FUN = median ) <= 180) ~ "E")) 


#######################################################################
#                            Top edges                                #
#######################################################################

# Setting dataframe with top edges in April 2016(vehicules).  
# Filters: 
# * No parking edges.
# * Week days.
# * Hour between 8hr and 20h.
# 
# Rua Eduardo Silva is divided inthe intersection for better analysis. 


# Setting the dataframe. No weekends and time between 8-20h.

df_superhotedges_april16pt <- df_speed_top
df_superhotedges_april16pt$time <- as.POSIXct(df_superhotedges_april16pt$seconds, 
                                              origin="1970-01-01")

df_superhotedges_april16pt <- df_superhotedges_april16pt %>% 
  filter(hour(time) >=8, hour(time)<= 20, wday(time) != 1 , wday(time) != 7, day(time) != 25) 



# Rua Eduardo Silva 2 

df_superhotedges_april16pt[df_superhotedges_april16pt$way_id == 37141970 & 
                             df_superhotedges_april16pt$lat >= 41.17755,]$way_id <- 37141970.2

df_superhotedges_april16pt$way_id <- as.factor(df_superhotedges_april16pt$way_id)


# df_osm_edge_ids has the top edges info.
df_superhotedges_april16pt_all <- df_edges_april16pt %>% 
  filter(way_id %in% df_osm_edge_ids$way_id)

df_superhotedges_april16pt_all$time <- as.POSIXct(df_superhotedges_april16pt_all$seconds, 
                                              origin="1970-01-01")


#Creating a df with all the points in the sessions that passes by SHE 
df_superhotedges_april16pt_sessions <- df_edges_april16pt %>% 
  filter(session_id %in% df_superhotsessions_april16pt)

df_superhotedges_april16pt_all[df_superhotedges_april16pt_all$way_id == 37141970 & 
                             df_superhotedges_april16pt_all$lat >= 41.17755,]$way_id <- 37141970.2

df_superhotedges_april16pt_sessions$time <- as.POSIXct(df_superhotedges_april16pt_sessions$seconds, 
                                              origin="1970-01-01")


#######################################################################
#                              Speed                                  #
#######################################################################

# df_superhotedges_april16pt %>% 
#   mutate(speed =  speed*18/5)

#######################################################################
#                              Time                                   #
#######################################################################

df_superhotedges_april16pt_all <- mutate(df_superhotedges_april16pt_all, 
                                     class = ifelse(hour(time) >=7 & 
                                                      hour(time) <= 20, "day", "night"))




