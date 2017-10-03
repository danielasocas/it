title: 'Sense My FEUP - April 2016 Data - Speeds'
author: "Daniela S. Gil"
 
# Speed by way_id and session.
df_speed %>% 
  group_by(way_id, session_id) %>% 
  summarise(avg_speed = mean(speed), n = n() )

# Speed by way_id and session.\
avg_speed_wayid <- df_speed %>% 
  group_by(way_id) %>% 
  summarise(avg_speed = mean(speed), n = n() )

ggplot(avg_speed_wayid, aes(avg_speed)) +
  geom_histogram(binwidth = 2) + 
  scale_x_continuous(name = "Avg_speed(m/s)",  breaks = seq(0, 100, 10) )

## By date 

df_speed$time <- as.POSIXct(df_speed$seconds, origin = "1970-01-01") 
  
df_speed %>% 
group_by(weekday =wday(time)) %>% 
  summarise(avg_speed = mean(speed)) %>% 
  ggplot(aes(weekday, avg_speed)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,7,1))

df_speed %>% 
  group_by(day =day(time)) %>% 
  summarise(avg_speed = mean(speed)) %>% 
  ggplot(aes(day, avg_speed)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,30,1))

df_speed %>% 
  group_by(hour =hour(time)) %>% 
  summarise(avg_speed = mean(speed)) %>% 
  ggplot(aes(hour, avg_speed)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,23,1))
  
prueba <- table(cut(df_speed$time, breaks = "30 mins"))
plot(prueba, xlab = "date", ylab = "frequency")

df_speed %>% 
    ggplot(aes(time, speed))+
    stat_summary(fun.y = mean, geom = "point")
  
df_speed %>% 
    group_by(hour =hour(time)) %>% 
    summarise(sessions = n_distinct(session_id)) %>% 
  ggplot(aes(hour, sessions)) + 
  geom_line()+
  scale_x_continuous(breaks = seq(0,23,1))
  
# ---------------------------------- Boxplots 
install.packages("plotly")
library(plotly)

# --------- Without plotly tryouts  ------
summary(df_intersession_april_gs$intersession_time)

boxplot(df_intersession_april_gs$intersession_time)
boxplot(df_intersession_april_gs$intersession_time, outline = FALSE)
boxplot(filter(df_intersession_april_gs, intersession_time > 0 )$intersession_time, outline = FALSE)

plot_ly(y = df_intersession_april_gs$intersession_time, type = "box")

boxplot(df_intersession_april_gs$intersession_time ~ wday(df_intersession_april_gs$time), outline = FALSE, notch = TRUE)
#-----------------

b1 <- df_intersession_april_gs$intersession_time
b1 <- boxplot(b1,horizontal = TRUE, outline = FALSE)
ggplotly(b1)

plot_ly(y = df_intersession_april_gs$intersession_time, type = "box", name = "Intersession time(mins)") %>% 
  add_boxplot(y= filter(df_intersession_april_gs, intersession_time > 0 )$intersession_time, name = "At least 2 sessions")


p <- ggplot(df_intersession_april_gs, aes(x = wday(time), y = intersession_time, fill = interaction(wday(time)) )) + 
  geom_boxplot(outlier.shape = NA) + 
  xlab("Days of the week" ) +
  ylab("Intersession time (min)") +
  ggtitle("Intersession time(min) in the week without outliers") + 
  theme(legend.position="none") + 
  scale_x_discrete(labels = c("1" = "Sun","2" = "Mon","3" = "Tue",
                              "4" = "Wed", "5" = "Thur","6" = "Fri","7" = "Sat")) 

p <- plotly_build(p)

#Only this to take out outliers
p$x$data <- lapply(p$x$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})
p
scale_fill_discrete(scale_fill_identity(name = "Days", breaks = c("1","2","3","4","5","6","7"),
                                          labels = c( "Sun", "Mon", "Tue","Wed", "Thur", "Fri","Sat")))
