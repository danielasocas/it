#Filtering and Data Visualization
#Daniela S. Gil, Last modified: 18/05/17

#------------------ POINTS EDGE OSM BY CARS  --------------------------

## GETTING THE DATA BY TRAVEL MODE 

### Segments by car

df_segments_by_car <- data.frame()
df_segments_by_car <- dbGetQuery(con, "SELECT session_id, segment_id, seconds_start, seconds_end, travelmode
                                    FROM sensemyfeup.segments as segments
                                    WHERE session_id > 50000 ")

#segments.travelmode = 'car' OR segments.travelmode =  'bus'

### Table for mapping by cars 

df_points_edge_osm_cars <- data.frame()
df_points_edge_osm_cars <- dbGetQuery(con, "SELECT way_id, COUNT(*) AS points
                                       FROM sensemyfeup_raw.osmlocation as osm 
                                          INNER JOIN sensemyfeup.segments as segments 
                                            USING (session_id)
                                          INNER JOIN sensemyfeup_raw.session_offsets 
                                            USING (session_id)
                                       WHERE (osm.seconds + session_offsets.clock_offset_seconds BETWEEN segments.seconds_start AND seconds_end) AND
                                            session_id > 50000
                                       GROUP BY way_id
                                       ORDER BY points DESC")

save(df_points_edge_osm_cars, file="df_points_edge_osm_cars")

## JOIN WITH OSM LOCATION

##------------------ MAPPING WITH LEAFLET  --------------------------

##------------------LOADING DF ------------------

load("df_points_edge_osm_cars.Rda")

##------------------CONNECTING WITH OPENSTREETMAP DATABASE ------------------

pw <- {
  "y7dWwByZLWso"
}

drv <- dbDriver("PostgreSQL")

con_osm <- dbConnect(drv, dbname = "openstreetmap",
                     host = "localhost", port = 15432,
                     user = "marciofontes", password = pw)


##------------------ MAPPING WITH LEAFLET  --------------------------

list_points_edge_osm_cars <- df_points_edge_osm_cars[1:500, 1]

fun.ecdf <- ecdf(df_points_edge_osm_cars$points)

my.ecdf <- fun.ecdf(sort(df_points_edge_osm_cars$points))

my_ecdf_df <- data.frame(my.ecdf)

my_ecdf_df$points <- sort(df_points_edge_osm_cars$points)

tail(my_ecdf_df, 400)

P <- ecdf(df_points_edge_osm_cars$points)
grid()
plot(P, log="x", xlim=c(1, max(df_points_edge_osm_cars$points)))

m <- leaflet() %>% setView(lng=-8.61419, lat=41.16311, zoom = 13)
m <- addTiles(m) 
m <- addProviderTiles(m, "CartoDB.Positron")

counter <- 1

for(way_id in list_points_edge_osm_cars) {
  
  df_way_id <- dbGetQuery(con_osm, paste0("SELECT st_astext(st_transform(way, 4326)) AS line FROM planet_osm_line WHERE planet_osm_line.osm_id = ", way_id))
  
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
    
    # Deciding the color of the point by the counter.
    
    if(df_points_edge_osm_cars[counter, 2] > 18838) {
      
      m <- addPolylines(m, lons, lats, color='red')
      
    } else if (df_points_edge_osm_cars[counter, 2] >= 4424 && df_points_edge_osm_cars[counter, 2] <= 18838) {
      
      m <- addPolylines(m, lons, lats, color='yellow')
      
    } else {
      
      m <- addPolylines(m, lons, lats, color='green')
      
    }
    
    counter <- counter + 1 
    
  }
  
  #print(line)
  
}

dbDisconnect(con_osm)

m

#------------------ RANDOM FACT LEAFLET  --------------------------
#Leaflet
m = leaflet()
m = addTiles(m)
m = addMarkers(m, lng=-8.805099, lat=39.74451, popup="T")
print(m)
