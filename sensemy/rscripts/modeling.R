# Daniela Socas Gil
# Last modified: 25/07/17

#######################################################################
#                     Creating df for the model                       #
#######################################################################

#-----------------------------------------#
#              Meteorological             #
#-----------------------------------------#

wu_metar_April16 <- read.csv("WU_METAR_April_2016.csv")

df_wu_features <- wu_metar_April16 %>% 
  filter(VelocidadeDoVentoKmh != "calm") %>% 
  group_by(day= day(DateUTC), hour= hour(DateUTC)) %>% 
  summarise(wind_speed = mean(as.integer(VelocidadeDoVentoKmh)), temperature = mean(TemperaturaC))



#-----------------------------------------#
#       Basic data, speed and time        #
#-----------------------------------------#

# Spliting st. Eduardo Silva in 2. 

df_osm_edge_ids <- df_osm_edge_ids %>% 
  mutate(middle_lat = case_when( way_id == 35078500 ~ 41.173704,
                                 way_id == 35145521 ~ 41.176411,
                                 way_id == 316459169 ~ 41.180174,
                                 way_id == 37141967 ~ 41.177119, 
                                 way_id == 63192525 ~ 41.177066,
                                 way_id == 106334851 ~ 41.177365, 
                                 way_id == 37141970 ~ 41.176086,
                                 way_id == 37141970.2 ~ 41.179357), 
         middle_lon = case_when(way_id == 35078500 ~ -8.602252, 
                                way_id == 35145521 ~ -8.599999,
                                way_id == 316459169 ~ -8.598134,
                                way_id == 37141967 ~ -8.596396,
                                way_id == 63192525 ~ -8.592392,
                                way_id == 106334851 ~ -8.589774, 
                                way_id == 37141970 ~ -8.587905,
                                way_id == 37141970.2 ~ -8.585850))

#df_osm_edge_ids[nrow(df_osm_edge_ids)+1,] <- c(37141970.2, "Rua D. Eduardo Santos Silva 2","v",41.179357, -8.585850)
#df_superhotedges_april16pt[(df_superhotedges_april16pt$way_id == 37141970 & df_superhotedges_april16pt$lat > intersection) ,]$way_id <- 37141970.2   

#------------------ Top 7 edges -----------------#
##################################################

#Basic merge meteorogical
dfm_base <- df_superhotedges_april16pt %>% 
  group_by(way_id,session_id, day = day(time), hour= hour(time), track = median(track) ) %>% 
  summarise(speed = mean(speed*18/5), course = factor(median(course))) %>%
  filter( course != 1.5) %>% 
  merge(y= df_wu_features, using = c(day,hour))

#Hour discretization

dfm_base <- dfm_base %>% 
  mutate( period = case_when(hour == 8 | 
                               hour == 9 | 
                               hour == 12 |
                               hour == 13 |
                               hour == 14 | 
                               hour == 17 |
                               hour == 18 |
                               hour == 19 |
                               hour == 20  ~ factor(hour, levels = c(8,9,"beforenoon",12,13,14,"afternoon",17,18,19,20)), 
                             hour == 10 |
                               hour == 11  ~ as.factor("beforenoon"), 
                               hour == 15 |
                               hour == 16 ~ as.factor("afternoon")))


# Direction 

dfm_base <- dfm_base %>% 
  mutate( heading = case_when((way_id == 37141970 | way_id == 35145521) &  course == 2 ~ "S", 
                              (way_id == 37141970 | way_id == 35145521) &  course == 1 ~ "N",
                              (way_id == 37141970.2 | way_id == 316459169) &  course == 2 ~ "N", 
                              (way_id == 37141970.2 | way_id == 316459169) &  course == 1 ~ "S",
                              (way_id == 63192525 | way_id == 106334851) &  course == 2 ~ "E", 
                              (way_id == 63192525 | way_id == 106334851) &  course == 1 ~ "W",
                              way_id == 35078500  &  course == 2 ~ "W", 
                              way_id == 35078500  &  course == 1 ~ "E",
                              way_id == 37141967  &  track > 180 ~ "W",
                              way_id == 37141967  &  track <= 180 ~ "E")) 


# State discetrization
dfm_base <- dfm_base %>% 
  mutate(flow = case_when(.$speed <= 18 ~ "congestion", .$speed > 18 & .$speed < 30 ~ "sync", .$speed >= 30 ~"free" )) 

# Edge info
dfm_base <- dfm_base %>% 
  merge(y=df_osm_edge_ids, using = way_id)

#------------------ features -----------------#

features <- c("session_id",
              "day",
              "hour",
              "wind_speed",
              "temperature",
              "period",
              "heading",
              "middle_lat",
              "middle_lon",
              "flow")


features <- c("session_id",
              "speed",
              "day",
              "hour",
              "wind_speed",
              "temperature",
              "period",
              "heading",
              "middle_lat",
              "middle_lon")


#Factors
dfm_base$flow <- as.factor(dfm_base$flow)
dfm_base$heading <- as.factor(dfm_base$heading)
dfm_base$heading <- as.factor(dfm_base$heading)
dfm_base$type <- as.factor(dfm_base$type)
dfm_base$middle_lat <- as.factor(dfm_base$middle_lat)
dfm_base$middle_lon <- as.factor(dfm_base$middle_lon)

dfm_base$middle_lat <- NULL
dfm_base$middle_lon <- NULL 


#######################################################################
#                               Modeling                              #
#######################################################################

dfm_train <- dfm_base %>% 
  filter(day <= 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]
  
dfm_test <- dfm_base %>% 
  filter(day > 23) 
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]

#---------------------------- Multi class classifier -----------------#
#######################################################################

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#

mtree <- rpartXse(flow  ~ ., dfm_train)
predtree <- predict(mtree,dfm_test, type = 'class')
mc <- table(predtree,dfm_test$flow)
err <- 100*(1-sum(diag(mc))/sum(mc))
describe(predtree)

err

err_2 <- err
mtree_2 <- mtree 
desc_tree2 <- describe(predtree)

prp(mtree, type = 4)

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#

mrand <- randomForest(flow ~ ., dfm_train, importance = TRUE)
mrand

predrand <- predict(mrand,dfm_test)
describe(predrand)
mc_rf <- table(predrand,dfm_test$flow)
err <- 100*(1-sum(diag(mc_rf))/sum(mc_rf))

rand_1 <- mrand 
err_rand1 <- err
desc_rand1 <- describe(predrand)

importance(regrand)
multiclass.roc(dfm_test$flow, predrand)

#-----------------------------------------#
#              Hierarchical               #
#-----------------------------------------#


#-------------------------------- Regression -------------------------#
#######################################################################

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#

regtree <- rpartXse(speed  ~ ., dfm_train)
preds <- predict(regtree,dfm_test)
preds
mae <- mean(abs(preds-dfm_test$speed))
mae

cr <- cor(preds,dfm_test$speed)
prp(regtree, type = 4)

regtree_1 <- regtree 
regmae_1 <- mae 
cr1 <- cr


regr.eval(dfm_test$speed,regtree,train.y=dfm_train$speed)

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#

mrand <- randomForest(speed ~ ., dfm_train, importance = TRUE)

predrand <- predict(mrand,dfm_test)
describe(predrand)
table(predrand,dfm_test$speed)
randerr <- mean(abs(dfm_test$speed - predrand))

multiclass.roc(dfm_test$speed, predrand)

mrand_1 <- mrand
predrand_1 <- predrand
randerr_1 <- randerr

#-----------------------------------------#
#                 Mars                    #
#-----------------------------------------#

mars <- earth(speed ~ .,dfm_train)
predmars <- predict(mars,dfm_test)
mae_mars <- mean(abs(dfm_test$speed - predmars))
mae_mars
summary(predmars)
table(predmars,dfm_test$speed)

summary(mars)

mars_1 <- mars
mae_mars_1 <- mae_mars


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
        legend.title = element_text("Street", size=18, face="bold"), 
        legend.text = element_text(size=18, face="bold"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = c(0.2,0.7))
