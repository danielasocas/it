#Daniela Socas, Last modified: 01/09/17
#Connect to the database R-PsotgreSQL

#------------------ BASICS --------------------------

if(! "ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2")
}

if(! "dplyr" %in% rownames(installed.packages())){
  install.packages("dplyr")
}

if(! "tidyr" %in% rownames(installed.packages())){
  install.packages("tidyr")
}

#if(! "xlsx" %in% rownames(installed.packages())){
#  install.packages("xlsx")
#}

if(! "magrittr" %in% rownames(installed.packages())){
  install.packages("magrittr")
}
if(! "reshape2" %in% rownames(installed.packages())){
  install.packages("reshape2")
}

if(! "gridExtra" %in% rownames(installed.packages())){
  install.packages("gridExtra")
}

if(! "plotly" %in% rownames(installed.packages())){
  install.packages("plotly")
}

library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(reshape2)

#------------------ CONNECTION --------------------------

if(! "DBI" %in% rownames(installed.packages())){
  install.packages("DBI")
}
if(! "RPostgreSQL" %in% rownames(installed.packages())){
  install.packages("RPostgreSQL")
}

library(DBI)
library(RPostgreSQL)

#------------------ MAPPING -------------------------- 

if(! "leaflet" %in% rownames(installed.packages())){
  install.packages("leaflet",dependencies = TRUE)
}

library(leaflet)

#------------------ TIME MANAGEMENT --------------------------

if(! "lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate")
}

library(lubridate)


#------------------ MODELING --------------------------

if(! "DMwR" %in% rownames(installed.packages())){
  install.packages("DMwR")
}
if(! "rpart" %in% rownames(installed.packages())){
  install.packages("rpart")
}
if(! "rpart.plot" %in% rownames(installed.packages())){
  install.packages("rpart.plot")
}
if(! "rattle" %in% rownames(installed.packages())){
  install.packages("rattle")
}
if(! "RColorBrewer" %in% rownames(installed.packages())){
  install.packages("RColorBrewer")
}
if(! "Hmisc" %in% rownames(installed.packages())){
  install.packages("Hmisc")
}
if(! "earth" %in% rownames(installed.packages())){
  install.packages("earth")
}
if(! "randomForest" %in% rownames(installed.packages())){
  install.packages("randomForest")
}
if(! "pROC" %in% rownames(installed.packages())){
  install.packages("pROC")
}
if(! "party" %in% rownames(installed.packages())){
  install.packages('party')
}
if(! "performanceEstimation" %in% rownames(installed.packages())){
  install.packages('performanceEstimation')
}
if(! "broom" %in% rownames(installed.packages())){
  install.packages('broom')
}


library(DMwR)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(Hmisc)
library(earth)
library(randomForest)
library(pROC)
library(party)
library(performanceEstimation)
library(broom)

#------------------ Timeseries --------------------------

# if(! "prophet" %in% rownames(installed.packages())){
#   install.packages('prophet')
# }
# 
# library(prophet)

#------------------ Correlation --------------------------

if(! "corrgram" %in% rownames(installed.packages())){
  install.packages("corrgram")
}

library(corrgram)


#------------------ Random Forest plot --------------------------
# if(! "ggraph" %in% rownames(installed.packages())){
#   install.packages('ggraph')
# }
# if(! "igraph" %in% rownames(installed.packages())){
#   install.packages('igraph')
# }
# library(ggraph)
# library(igraph)
# 



