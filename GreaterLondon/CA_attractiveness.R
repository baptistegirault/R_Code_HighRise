#title: 'Conservations Areas Attractiveness'
#author: "Baptiste GIRAULT"

rm(list=ls())

library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)

#######################################
#Set Working Directory
setwd(dir = "C:/Baptiste/DB_Finale/LondonPlan/Data")

#######################################
#Load Towers file (include all towers inside Borough with known Conservations Areas) 
  #1883 stamped and planned towers (39.1% of the initial database)
Tours <- readOGR("Emporis/ToursExistantes/LondonEmporis2017WithData.shp",
                 "LondonEmporis2017WithData")
#Change into good projection (here British Grid)
BritishGrid <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
Tours <- spTransform(Tours, BritishGrid)

#Load Greater London Conservations Areas 
CA <- readOGR("ConservationAreasJuly2017/WithDataLondon.shp",
              "WithDataLondon")
#Remmove Borough with missing data and change of projection
CA <- subset(CA_Total, CA_Total@data$CAPTURE_SC != "NO DATA CURRENTLY AVAILABLE FOR THIS DISTRICT")
CA <- spTransform(CA, BritishGrid)


#Load Greater London Conservations Areas Buffer (100m, 200m and 300m)
Zone1 <- readOGR("ConservationAreasJuly2017/Zone/CAbuffer100.shp",
                 "CAbuffer100")
Zone2 <- readOGR("ConservationAreasJuly2017/Zone/CAbuffer200.shp",
                 "CAbuffer200")
Zone3 <- readOGR("ConservationAreasJuly2017/Zone/CAbuffer300.shp",
                 "CAbuffer300")

#######################################
#Counting of towers inside each area
IsOverZone0 <- over(Tours, CA)
IsOverZone0 <- na.omit(IsOverZone0)
NbToursZone0 <- as.numeric(nrow(IsOverZone0))

IsOverZone1 <- over(Tours, Zone1)
IsOverZone1 <- na.omit(IsOverZone1)
NbToursZone1 <- as.numeric(nrow(IsOverZone1)) - as.numeric(nrow(IsOverZone0))

IsOverZone2 <- over(Tours, Zone2)
IsOverZone2 <- na.omit(IsOverZone2)
NbToursZone2 <- as.numeric(nrow(IsOverZone2)) - as.numeric(nrow(IsOverZone1))

IsOverZone3 <- over(Tours, Zone3)
IsOverZone3 <- na.omit(IsOverZone3)
NbToursZone3 <- as.numeric(nrow(IsOverZone3)) - as.numeric(nrow(IsOverZone2))

#######################################
#Number of towers in each area (CA and buffers)
NbTours <- cbind(NbToursZone0, NbToursZone1, NbToursZone2, NbToursZone3)

#######################################
#Extraction of the results
Results <- as.data.frame(NbTours)
write.csv(Results, file = "CA_Attractiveness.csv")
