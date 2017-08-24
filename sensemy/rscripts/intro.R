#Connect to the database R-PsotgreSQL
#Daniela Socas, Last modified: 27/04/17

#------------------ INSTALLING PACKAGES --------------------------

if(! "lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate")
}
if(! "ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")
}
if(! "DBI" %in% rownames(installed.packages())){
  install.packages("DBI")
}
if(! "RPostgreSQL" %in% rownames(installed.packages())){
  install.packages("RPostgreSQL")
}

#Data Visualization
if(! "leaflet" %in% rownames(installed.packages())){
  install.packages("leaflet")
}

#Packages from udacity
install.packages("swirl")
install.packages('knitr', dependencies = T) 

#------------------ LOADING PACKAGES --------------------------

library(lubridate)
library(ggplot2)
library(DBI)
library(RPostgreSQL)

library(leaflet)

library(swirl)
library(knitr)

#------------------ DATABASE ACCESS --------------------------

drv <- dbDriver("PostgreSQL")
pwd <- "psicopatas"

#Database to connect 
db <- "sensemyfeup"
#db <- sensemycity

con <-dbConnect(drv,host='localhost',port='15432',dbname=db,user='danielasocas',
password=pwd)

#List all tables in the database
#dbListTables(con )

#Summary of the Connection.
#Summary(con)

#Get list of all the tables in the database.
#dbSendQuery(conn,"select * from table_name")

#Read table in R.
#table_name<- dbReadTable(conn, "tablen_name")

#Close current query.
#dbClearResult(dbListResults(conn)[[1]])

#Disconnect database.
#dbDisconnect(conn)

#------------------ LOADING DATAFRAMES --------------------------

df_prueba <- data.frame()
df_query <-"SELECT * FROM sensemyfeup. limit 10" 
df_prueba <- dbGetQuery(con, df_query)
save(df_prueba, file="prueba.Rda")












