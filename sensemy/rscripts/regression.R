# Daniela Socas Gil
# Last modified: 25/07/17

source("rscripts/features.R")

#######################################################################
#                     Creating df for the model                       #
#######################################################################

#-----------------------------------------#
#              Meteorological             #
#-----------------------------------------#

wu_metar_April16 <- read.csv("raw_data/WU_METAR_April_2016.csv")

df_wu_features <- wu_metar_April16 %>% 
  group_by(day= day(DateUTC), hour= hour(DateUTC),  DateUTC, conditions = Conditions) %>% 
  summarise(wind_speed = mean(as.integer(VelocidadeDoVentoKmh)), temperature = mean(TemperaturaC))


# ???
# df_wu_features <- wu_metar_April16 %>% 
#   filter(VelocidadeDoVentoKmh != "calm") %>% 
#   group_by(day= day(DateUTC), hour= hour(DateUTC)) %>% 
#   summarise(wind_speed = mean(as.integer(VelocidadeDoVentoKmh)), temperature = mean(TemperaturaC))
# 


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
                                 way_id == 36979228 ~ 41.179386,
                                 way_id == 37141970 ~ 41.176086,
                                 way_id == 37141970.2 ~ 41.179357), 
         middle_lon = case_when(way_id == 35078500 ~ -8.602252, 
                                way_id == 35145521 ~ -8.599999,
                                way_id == 316459169 ~ -8.598134,
                                way_id == 37141967 ~ -8.596396,
                                way_id == 63192525 ~ -8.592392,
                                way_id == 106334851 ~ -8.589774,
                                way_id == 36979228 ~ -8.602060,
                                way_id == 37141970 ~ -8.587905,
                                way_id == 37141970.2 ~ -8.585850))

#------------------ Top edges -----------------#
##################################################



#Speed calculation 
## Method 2 ( Avg of mean speed)
dfm_base <- df_superhotedges_april16pt %>% 
  group_by(way_id, session_id, day = day(time), hour= hour(time), wday = wday(time), heading ) %>% 
  summarise(speed = mean(speed*18/5), time = min(time)) %>% 
  group_by(way_id, day, hour, wday, heading, time) %>% 
  summarise(speed = mean(speed)) 


#Merge meteorogical
dfm_base <- dfm_base %>% 
  merge(y= df_wu_features, using = c(day,hour)) %>% 
  mutate(min = case_when( as.numeric(minute(time)) >= 30 ~ 30, 
                          as.numeric(minute(time)) < 30 ~ 0)) %>% 
  filter(as.numeric(minute(DateUTC)) == min )


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
                               hour == 20  ~ factor(hour, 
                                                    levels = c(8,9,
                                                               "beforenoon",
                                                               12,13,14,
                                                               "afternoon",
                                                               17,18,19,20)), 
                             hour == 10 |
                               hour == 11  ~ as.factor("beforenoon"), 
                             hour == 15 |
                               hour == 16 ~ as.factor("afternoon")))


# Direction 

# dfm_base <- dfm_base %>% 
#   mutate( heading = case_when((way_id == 37141970 | way_id == 35145521) &  course == 2 ~ "S", 
#                               (way_id == 37141970 | way_id == 35145521) &  course == 1 ~ "N",
#                               (way_id == 37141970.2 | way_id == 316459169) &  course == 2 ~ "N", 
#                               (way_id == 37141970.2 | way_id == 316459169) &  course == 1 ~ "S",
#                               (way_id == 63192525 | way_id == 106334851) &  course == 2 ~ "E", 
#                               (way_id == 63192525 | way_id == 106334851) &  course == 1 ~ "W",
#                               way_id == 35078500  &  course == 2 ~ "W", 
#                               way_id == 35078500  &  course == 1 ~ "E",
#                               way_id == 37141967  &  track > 180 ~ "W",
#                               way_id == 37141967  &  track <= 180 ~ "E")) 


# State discetrization
# dfm_base <- dfm_base %>% 
#   mutate(flow = case_when(.$speed <= 18 ~ "congestion", .$speed > 18 & .$speed < 30 ~ "sync", .$speed >= 30 ~"free" )) 

# Edge info
dfm_base <- dfm_base %>% 
  merge(y=df_osm_edge_ids, using = way_id)

#Factors
 dfm_base$heading <- as.factor(dfm_base$heading)
 dfm_base$wday <- as.factor(dfm_base$wday)
# dfm_base$type <- as.factor(dfm_base$type)
# dfm_base$middle_lat <- as.factor(dfm_base$middle_lat)
# dfm_base$middle_lon <- as.factor(dfm_base$middle_lon)
# 
# dfm_base$middle_lat <- NULL
# dfm_base$middle_lon <- NULL 

#Conditions 
# dfm_base <- dfm_base %>% 
#    mutate( weather = case_when(Conditions == "Céu Limpo" | 
#                                 Conditions == "Nuvens Dispersas" ~ factor("limpo", 
#                                                                            levels = c("limpo", "nublado", "chuva")), 
#                                 Conditions == "Aguaceiros Fracos de Chuva" |
#                                 Conditions == "Aguaceiros" |
#                                 Conditions == "Chuva" |
#                                 Conditions == "Chuva Forte" |
#                                 Conditions == "Chuva Fraca" |
#                                 Conditions == "Trovoadas com Chuva"  ~ as.factor("chuva"), 
#                                 Conditions == "Muito Nublado" | 
#                                 Conditions == "Parcialmente Nublado" |
#                                 Conditions == "Céu Encoberto"  ~ as.factor("nublado"))) 
dfm_base <- dfm_base %>% 
   mutate( chuva = case_when(conditions == "Aguaceiros Fracos de Chuva" |
                                 conditions == "Aguaceiros" |
                                 conditions == "Chuva" |
                                 conditions == "Chuva Forte" |
                                 conditions == "Chuva Fraca" |
                                 conditions == "Trovoadas com Chuva"  ~ as.numeric(1), 
                               TRUE  ~ as.numeric(0))) 
 
 
#######################################################################
#                               Modeling                              #
#######################################################################

dfm_train <- dfm_base %>% 
  filter(day <= 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]

dfm_test <- dfm_base %>% 
  filter(day > 23) 
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]

#-------------------------------- Regression -------------------------#
#######################################################################

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#

# Model
regtree <- rpartXse(speed  ~ ., dfm_train, method = "anova")
preds <- predict(regtree,dfm_test)
#preds

# Error 
mae <- mean(abs(preds-dfm_test$speed))
mae

cr <- cor(preds,dfm_test$speed)
cr

#Plotting 
prp(regtree, type = 4)

new.regtree <- prp(regtree,snip=TRUE)$obj

fancyRpartPlot(regtree)
#prettyTree(regtree)

# Saving in other variable 
regtree_0 <- regtree 
regmae_0 <- mae 
cr0 <- cr

regr.eval(dfm_test$speed, regtree,train.y=dfm_train$speed)

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#
set.seed(318)
mrand <- randomForest(speed ~ ., dfm_train, importance = TRUE, method = "anova", ntree = 2000)

predrand <- predict(mrand,dfm_test)
describe(predrand)
table(predrand,dfm_test$speed)
randerr <- mean(abs(dfm_test$speed - predrand))

multiclass.roc(dfm_test$speed, predrand)

mrand_0 <- mrand
predrand_0 <- predrand
randerr_0 <- randerr


#------------- PARTY ---------------------#

mrand.p_0 <- randomForest(speed ~ ., dfm_train, 
                        controls=cforest_unbiased(ntree=2000, mtry=3))

#-----------------------------------------#
#                 Mars                    #
#-----------------------------------------#
set.seed(1234)
mars <- earth(speed ~ .,dfm_train)
predmars <- predict(mars,dfm_test)
mae_mars <- mean(abs(dfm_test$speed - predmars))
mae_mars
summary(predmars)
table(predmars,dfm_test$speed)

summary(mars)

mars_4 <- mars
mae_mars_0 <- mae_mars

