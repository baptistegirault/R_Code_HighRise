#title: 'Towers In Clusters'
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
Zone0 <- readOGR("CouloirsDeVue/Zone/Zone0.shp",
                 "Zone0")
Zone1 <- readOGR("CouloirsDeVue/Zone/Zone1.shp",
                 "Zone1")
Zone2 <- readOGR("CouloirsDeVue/Zone/Zone2.shp",
                 "Zone2")
Zone3 <- readOGR("CouloirsDeVue/Zone/Zone3.shp",
                 "Zone3")

Tours <- readOGR("Clusters/Clusters.shp",
                 "Clusters")

#######################################
#Add periods
Tours@data$Period <- 1
Tours@data$Period[Tours@data$C2 == "NA"] <- 2

#######################################
#Number of tower in each area before 2007
ToursBefore2007 <- Tours[Tours@data$Period == 1,]

IsOverZone0_1 <- over(ToursBefore2007, Zone0)
IsOverZone0_1 <- na.omit(IsOverZone0_1)
NbToursZone0_1 <- as.numeric(nrow(IsOverZone0_1))

IsOverZone1_1 <- over(ToursBefore2007, Zone1)
IsOverZone1_1 <- na.omit(IsOverZone1_1)
NbToursZone1_1 <- as.numeric(nrow(IsOverZone1_1)) - as.numeric(nrow(IsOverZone0_1))

IsOverZone2_1 <- over(ToursBefore2007, Zone2)
IsOverZone2_1 <- na.omit(IsOverZone2_1)
NbToursZone2_1 <- as.numeric(nrow(IsOverZone2_1)) - as.numeric(nrow(IsOverZone1_1))

IsOverZone3_1 <- over(ToursBefore2007, Zone3)
IsOverZone3_1 <- na.omit(IsOverZone3_1)
NbToursZone3_1 <- as.numeric(nrow(IsOverZone3_1)) - as.numeric(nrow(IsOverZone2_1))

Results <- cbind(NbToursZone0_1, NbToursZone1_1, NbToursZone2_1, NbToursZone3_1)
colnames(Results) <- c("Protected View", "Buffer 100m", "Buffer 200m", "Buffer 300m")

#######################################
#Number of tower in each area after 2007
ToursAfter2007 <- Tours[Tours@data$Period == 2,]

IsOverZone0_2 <- over(ToursAfter2007, Zone0)
IsOverZone0_2 <- na.omit(IsOverZone0_2)
NbToursZone0_2 <- as.numeric(nrow(IsOverZone0_2))

IsOverZone1_2 <- over(ToursAfter2007, Zone1)
IsOverZone1_2 <- na.omit(IsOverZone1_2)
NbToursZone1_2 <- as.numeric(nrow(IsOverZone1_2)) - as.numeric(nrow(IsOverZone0_2))

IsOverZone2_2 <- over(ToursAfter2007, Zone2)
IsOverZone2_2 <- na.omit(IsOverZone2_2)
NbToursZone2_2 <- as.numeric(nrow(IsOverZone2_2)) - as.numeric(nrow(IsOverZone1_2))

IsOverZone3_2 <- over(ToursAfter2007, Zone3)
IsOverZone3_2 <- na.omit(IsOverZone3_2)
NbToursZone3_2 <- as.numeric(nrow(IsOverZone3_2)) - as.numeric(nrow(IsOverZone2_2))

Res <- cbind(NbToursZone0_2, NbToursZone1_2, NbToursZone2_2, NbToursZone3_2)

Results <- rbind(Results, Res)
Results <- as.data.frame(Results)
rm(Res)

#######################################
#DEFCON level BEFORE 2007
for (i in 1:nrow(ToursBefore2007@data)) { #
  TowersI <- ToursBefore2007[ToursBefore2007[i,],]
  
  #Over Zones 0,1,2 et 3
  IsIn0 <- over(TowersI, Zone0)
  IsIn1 <- over(TowersI, Zone1)
  IsIn2 <- over(TowersI, Zone2)
  IsIn3 <- over(TowersI, Zone3)
  
  if (nrow(na.omit(IsIn3)) == 0) {
    ToursBefore2007@data$Zone3[i] <- 0
  } else {
    ToursBefore2007@data$Zone3[i] <- 1
  }
  
  if (nrow(na.omit(IsIn2)) == 0) {
    ToursBefore2007@data$Zone2[i] <- 0
  } else {
    ToursBefore2007@data$Zone2[i] <- 1
    ToursBefore2007@data$Zone3[i] <- 0
  }
  
  if (nrow(na.omit(IsIn1)) == 0) {
    ToursBefore2007@data$Zone1[i] <- 0
  } else {
    ToursBefore2007@data$Zone1[i] <- 1
    ToursBefore2007@data$Zone2[i] <- 0
  }
  
  if (nrow(na.omit(IsIn0)) == 0) {
    ToursBefore2007@data$Zone0[i] <- 0
  } else {
    ToursBefore2007@data$Zone0[i] <- 1
    ToursBefore2007@data$Zone1[i] <- 0
  }

  #Is inside cluster
  if (ToursBefore2007@data$C2[i] == 0) {
    ToursBefore2007@data$InCluster[i] <- 0
  } else {
    ToursBefore2007@data$InCluster[i] <- 1
  }
  
  #DEFCON
  if (ToursBefore2007@data$Zone0[i] == 1 & ToursBefore2007@data$InCluster[i] == 0) {
    ToursBefore2007@data$Defcon[i] <- 1
  } else if (ToursBefore2007@data$Zone0[i] == 1 & ToursBefore2007@data$InCluster[i] == 1) {
    ToursBefore2007@data$Defcon[i] <- 2
  } else if (ToursBefore2007@data$Zone1[i] == 1) {
    ToursBefore2007@data$Defcon[i] <- 5
  } else if (ToursBefore2007@data$Zone2[i] == 1) {
    ToursBefore2007@data$Defcon[i] <- 6
  } else if (ToursBefore2007@data$Zone3[i] == 1) {
    ToursBefore2007@data$Defcon[i] <- 7
  } else {
    ToursBefore2007@data$Defcon[i] <- 8
  }
  
    
}

dfBefore <- as.data.frame(ToursBefore2007)

#######################################
#DEFCON level AFTER 2007
for (j in 1:nrow(ToursAfter2007@data)) { #
  TowersI <- ToursAfter2007[ToursAfter2007[j,],]
  
  #Over Zones 0,1,2 et 3
  IsIn0 <- over(TowersI, Zone0)
  IsIn1 <- over(TowersI, Zone1)
  IsIn2 <- over(TowersI, Zone2)
  IsIn3 <- over(TowersI, Zone3)
  
  if (nrow(na.omit(IsIn3)) == 0) {
    ToursAfter2007@data$Zone3[j] <- 0
  } else {
    ToursAfter2007@data$Zone3[j] <- 1
  }
  
  if (nrow(na.omit(IsIn2)) == 0) {
    ToursAfter2007@data$Zone2[j] <- 0
  } else {
    ToursAfter2007@data$Zone2[j] <- 1
    ToursAfter2007@data$Zone3[j] <- 0
  }
  
  if (nrow(na.omit(IsIn1)) == 0) {
    ToursAfter2007@data$Zone1[j] <- 0
  } else {
    ToursAfter2007@data$Zone1[j] <- 1
    ToursAfter2007@data$Zone2[j] <- 0
  }
  
  if (nrow(na.omit(IsIn0)) == 0) {
    ToursAfter2007@data$Zone0[j] <- 0
  } else {
    ToursAfter2007@data$Zone0[j] <- 1
    ToursAfter2007@data$Zone1[j] <- 0
  }
  
  #Is inside cluster
  if (ToursAfter2007@data$C2[j] == 0) {
    ToursAfter2007@data$InCluster[j] <- 0
  } else {
    ToursAfter2007@data$InCluster[j] <- 1
  }
  
  #DEFCON
  if (ToursAfter2007@data$Zone0[j] == 1 & ToursAfter2007@data$InCluster[j] == 0) {
    ToursAfter2007@data$Defcon[j] <- 1
  } else if (ToursAfter2007@data$Zone0[j] == 1 & ToursAfter2007@data$InCluster[j] == 1 & ToursAfter2007@data$Cas[j] == 1) {
    ToursAfter2007@data$Defcon[j] <- 2
  } else if (ToursAfter2007@data$Zone0[j] == 1 & ToursAfter2007@data$InCluster[j] == 1 & ToursAfter2007@data$Cas[j] == 2) {
    ToursAfter2007@data$Defcon[j] <- 3
  } else if (ToursAfter2007@data$Zone0[j] == 1 & ToursAfter2007@data$InCluster[j] == 1 & ToursAfter2007@data$Cas[j] == 3) {
    ToursAfter2007@data$Defcon[j] <- 4
  } else if (ToursAfter2007@data$Zone1[j] == 1) {
    ToursAfter2007@data$Defcon[j] <- 5
  } else if (ToursAfter2007@data$Zone2[j] == 1) {
    ToursAfter2007@data$Defcon[j] <- 6
  } else if (ToursAfter2007@data$Zone3[j] == 1) {
    ToursAfter2007@data$Defcon[j] <- 7
  } else {
    ToursAfter2007@data$Defcon[j] <- 8
  }
  
  
}

dfAfter <- as.data.frame(ToursAfter2007)

#######################################
#Data extraction of DEFCON level
write.csv(dfBefore, file = "ConservationAreasJuly2017/DefconBef2007.csv",row.names = FALSE) 
write.csv(dfAfter, file = "ConservationAreasJuly2017/DefconAfter2007.csv",row.names = FALSE)


#dfAfter$Defcon <- as.factor(as.character(dfAfter$Defcon))
#dfBefore$Defcon <- as.factor(as.character(dfBefore$Defcon))

After <- filter(dfAfter, Defcon != 8)
Before <- filter(dfBefore, Defcon != 8)
Total <- rbind(Before, After)



#VISUALIZATION
ggplot(Before) +
  geom_bar(data = Before, aes(x = Before$Defcon-0.3, fill="Before 2007"),
           alpha = 0, position = "dodge", binwidth=0.3) +
  geom_bar(data = After, aes(x = After$Defcon, fill="After 2007"),
           alpha = 0.6) +
  scale_fill_manual(name = "",values=c("#34495e", "#ffbf99"), 
                    breaks = c("After 2007","Before 2007")) +
  scale_color_manual(name = "", values = "#34495e", breaks = "Protected View") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(ncol=1)) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) +
  xlab("DEFCON level") + ylab("Number of towers")
