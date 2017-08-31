################################################################################
#                              Number of sessions                              #
################################################################################

map <- function(data, name, all = TRUE) {
  # Shows the map of Porto coloring the edges with more sessions by 3 levels.
  #
  # Args:
  #   data: data with edges to map.
  #   name: name of the new map created.
  #
  # Returns:
  #   A map object m. 
  
## Map points April 2016

library(leaflet)
  
#List before passing to map.
df_osm_edge <- data %>% 
  select(way_id, sessions)

list_osm_edge <- df_osm_edge[, 1]

# Creating the empty map of Porto
superior <- quantile(df_osm_edge$sessions, 0.9)
medio <- quantile(df_osm_edge$sessions, 0.7)
low <- 0 

if(all == FALSE){
low <- quantile(df_osm_edge$sessions, 0.5)
}

m <- leaflet() %>% setView(lng=-8.61419, lat=41.16311, zoom = 13)
m <- addTiles(m) 
m <- addProviderTiles(m, "CartoDB.Positron")


counter <- 1

for(way_id in list_osm_edge) {
  
  
  df_way_id <- dbGetQuery(con_osm, paste0("SELECT st_astext(st_transform(way, 4326)) 
                                          AS line FROM planet_osm_line WHERE planet_osm_line.osm_id = ", way_id))
  
  line <- df_way_id$line
  line <- as.character(line)
  
  line <- unlist(strsplit(line, split='(', fixed=TRUE))[2]
  line <- substr(line, 1, nchar(line) - 1)
  
  parsed_line <- strsplit(line, ",")
  
  lons <- c()
  lats <- c()
  
  if(length(parsed_line) != 0) {
    
    #Defining lons and lats as variables to use later.
    
    for(coord in parsed_line[[1]]) {
      
      lon <- unlist(strsplit(coord, split=' ', fixed=TRUE))[1]
      lat <- unlist(strsplit(coord, split=lon, fixed=TRUE))[2]
      lat <- substr(lat, 2, nchar(lat))
      
      lon <- as.numeric(lon)
      lat <- as.double(lat)
      
      lons <- c(lons, lon)
      lats <- c(lats, lat)
      
    }
    
    if(df_osm_edge[counter, 2] > superior) {
      m <- addPolylines(m, lons, lats, color='red', popup = paste("", way_id, sep = "")) 
      
    } else if (df_osm_edge[counter, 2] >= medio && df_osm_edge[counter, 2] <= superior) {
      m <- addPolylines(m, lons, lats, color='yellow', popup = paste("", way_id, sep = ""))
      
    } else if (df_osm_edge[counter, 2] >= low && df_osm_edge[counter, 2] <= medio) 
      m <- addPolylines(m, lons, lats, color='green', popup = paste("", way_id, sep = ""))
      
    }
    
    counter <- counter + 1 
  }
  m
}

