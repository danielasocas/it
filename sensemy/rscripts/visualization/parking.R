#Low speeds and parking analysis. 
#Daniela Socas, Last modified: 01/09/17

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
  summarise(median(track),iqr = IQR(track), mean(speed*18/5), median(speed*18/5),  
            quantile(speed*18/5, 0.75)) %>% 
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
  summarise(med_tr =median(track),iqr = IQR(track), mean= mean(speed*18/5),
            median =  median(speed*18/5),q75=  quantile(speed*18/5, 0.75)) %>% 
  merge(y= df_osm_edge_ids) %>% 
  ggplot(aes(x=iqr, fill = median)) +
  geom_histogram(binwidth = 20) + 
  facet_wrap(~type~address)
