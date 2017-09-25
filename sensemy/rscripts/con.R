#Connect to the database R-PsotgreSQL
#Daniela Socas, Last modified: 01/09/17

#Install Packages 
#install.packages("DBI")
#install.packages("RPostgreSQL")

#Load Packages
library(DBI)
library(RPostgreSQL)

#--------------- Connecting the database ----------------#

# Connecting Sense My FEUP - DS

drv <- dbDriver("PostgreSQL")
pwd <- "psicopatas"

#Database to connect 
db <- "sensemyfeup"
#db <- sensemycity

con <-dbConnect(drv,host='localhost',
                port='15432',
                dbname=db,
                user='danielasocas',
                password=pwd)

# Connecting OpenStreetMap - Marcio Fontes
pw <- {
  "y7dWwByZLWso"
}

drv <- dbDriver("PostgreSQL")

con_osm <- dbConnect(drv, dbname = "openstreetmap",
                     host = "localhost", port = 15432,
                     user = "marciofontes", password = pw)

