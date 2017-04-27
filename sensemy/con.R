#Connect to the database R-PsotgreSQL
#Daniela Socas, Last modified: 27/04/17

#Install Packages 
#install.packages("DBI")
#install.packages("RPostgreSQL")

#Load Packages
library(DBI)
library(RPostgreSQL)

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