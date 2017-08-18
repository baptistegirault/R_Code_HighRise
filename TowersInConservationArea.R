#title: 'Towers In Conservations Areas'
#author: "Baptiste GIRAULT"

rm(list=ls())

#Setup Library
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)

#######################################
#Set Working Directory
setwd(dir = "C:/Baptiste/DB_Finale/LondonPlan/Data")

#Load Towers file (include all stamped existing and in project towers inside Borough with known Conservations Areas) 
  #895 stamped and planned towers (39.1% of the initial database)
Tours <- readOGR("Emporis/ToursExistantes/ToursExistantesProjetsWithData.shp", 
                 "ToursExistantesProjetsWithData")

#Load Greater London Conservations Areas
CA <- readOGR("ConservationAreasJuly2017/DataBrut/ConservationsAreas.shp", 
              "ConservationsAreas", 
              encoding = "UTF-8")

#######################################
#Set the projection of both previous shapefile in British Grid
BritishGrid <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +vunits=m +no_defs")
proj4string(CA) <- BritishGrid
proj4string(Tours) <- BritishGrid

#######################################
#1967 to 2015 data (first and last known Conservations Areas)
  #This loop calculate the number of stamped towers built each year, 
  #the number towers built inside existing CA and the number of towers built inside futur CA
  #between 1967 and 2015
for (i in 1967:2015) {
  ToursTotalI <- Tours[Tours@data$YearStart == i,]
  
  if (length(ToursTotalI) == 0) {
    TimeLine <- rbind(TimeLine, c(i, 0, 0, 0, PreviousSum))
  } else if (length(ToursTotalI) == 0 & i == 1967) {
    TimeLine <- c(i, 0, 0, 0, PreviousSum)
  }
  else {
    PreviousCA.i <- CA[CA@data$Year <= i,]
    IsInsidePreviousCA <- over(ToursTotalI, PreviousCA.i)
    IsInsidePreviousCA <- na.omit(IsInsidePreviousCA)
    
    FuturCA.i <- CA[CA@data$Year > i,]
    if (length(FuturCA.i) == 0) {
      IsInsideFuturCA <- data.frame()
      IsInsideFuturCA <- data.frame()
    } else {
      IsInsideFuturCA <- over(ToursTotalI, FuturCA.i)
      IsInsideFuturCA <- na.omit(IsInsideFuturCA)
    }
    
    dfToursTotalI <- as.data.frame(ToursTotalI)
  }

  if (i == 1967) {
    TimeLine <- c(i, nrow(dfToursTotalI), nrow(IsInsidePreviousCA), nrow(IsInsideFuturCA) + nrow(IsInsidePreviousCA),  sum(PreviousCA.i@data$Shape_Area))
  } else {
    TimeLine <- rbind(TimeLine, 
                      c(i, 
                        nrow(dfToursTotalI), 
                        nrow(IsInsidePreviousCA), 
                        nrow(IsInsideFuturCA) + nrow(IsInsidePreviousCA),
                        sum(PreviousCA.i@data$Shape_Area)))
  }
  
  PreviousSum <- sum(PreviousCA.i@data$Shape_Area)
}

#add before 1967 and after 2015 data
ToursBefore1967 <- Tours[Tours@data$YearStart < 1967,]
#selecting all towers built after 2015 OR under project
ToursAfter2015 <- Tours[Tours@data$YearStart > 2015 & Tours@data$Current_St == "planned",]

IsInsideBefore1967 <- over(ToursBefore1967, CA)
IsInsideBefore1967 <- na.omit(IsInsideBefore1967)

IsInsideAfter2015 <- over(ToursAfter2015, CA)
IsInsideAfter2015 <- na.omit(IsInsideAfter2015)

#TimeLine <- rbind(c(1966, nrow(ToursBefore1967), 0, nrow(IsInsideBefore1967)), TimeLine)
TimeLine <- rbind(TimeLine, c(2016, nrow(ToursAfter2015), nrow(IsInsideAfter2015), 0))

#######################################
#Extracting the data into a csv file and transform it into a dataframe for the visualisation
TimeLine <- as.data.frame(TimeLine)
TimeLine <- TimeLine[c(1,2,4,3,5)]
colnames(TimeLine) <- c("Year", "TotalTowers", "InsideFuture", "InsideExisting", "CA_Area")
#write.csv(TimeLine, "TimeLine.csv")

dfCA <- as.data.frame(CA@data)
dfCA$Year <- as.numeric(dfCA$Year)

#######################################
#Visualization using ggplot2
ggplot(TimeLine, aes()) +
  geom_bar(stat = "identity", aes(x = TimeLine$Year, y = TimeLine$TotalTowers, fill="Towers built"),
            alpha = 0.8) +
  geom_bar(stat = "identity", aes(x = TimeLine$Year, y = TimeLine$InsideFuture, fill="Towers built inside future CA"),
            alpha = 0.8) +
  geom_bar(stat = "identity", aes(x = TimeLine$Year, y = TimeLine$InsideExisting, fill="Towers built inside existing CA"),
            alpha = 0.8) +
  geom_point(data=dfCA, 
             aes(x = dfCA$Year, y = 60, color = "CA creation"), 
             position = "jitter",
             alpha = 0.1,
             pch = 20,
             stroke = 2) +
  geom_line(aes(x = TimeLine$Year, y = TimeLine$CA_Area/4000000), 
            alpha = 0.6, size = 1.5, color = "#37426b") +
  #ggtitle("Total towers construction by years and inside Conservations Areas") +
  ylab("Number of built towers per years") +
  scale_x_continuous(limits = c(1967, 2015)) +
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "CA cumulative area (kmÂ²)")) +
  xlab("Year") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(ncol=1)) +
  #guides(color=guide_legend(ncol=1)) +
  scale_fill_manual(name = "",values=c("#ffbf99", "#34495e", "#ff9966"), 
                    breaks = c("Towers built","Towers built inside future CA","Towers built inside existing CA")) +
  scale_color_manual(name = "", 
                     values = "#37426b", 
                     breaks = "CA creation")
