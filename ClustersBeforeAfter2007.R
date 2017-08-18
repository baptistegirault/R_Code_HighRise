#title: 'Clusters Before and After 2007'
#author: "Baptiste GIRAULT"

rm(list=ls())

library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)
library(igraph)
library(dbscan)

#######################################
#Set Working Directory
setwd(dir = "C:/Baptiste/DB_Finale/LondonPlan/Data")

#######################################
#Load Emporis towers
  #895 stamped and planned towers (39.1% of the initial database)
Tours <- readOGR("Emporis/ToursExistantes/ToursExistantes.shp", 
                 "ToursExistantes")

#Select stamped towersbuilt before 2007
ToursAvt2007 <- Tours[Tours@data$YearStart < 2007,]

#######################################
#Distance calculation between towers as a matrix
MatDistTours <- as.matrix(spDists(Tours))
MatDistTours[MatDistTours] <- NA

#plot histogram of distance
hist(MatDistTours, main="Distance between 2 towers",xlab="Distances (m)")

#Same for towers before 2007
MatDistToursAvt2007 <- as.matrix(spDists(ToursAvt2007))
MatDistToursAvt2007[MatDistToursAvt2007 == 0.0000000] <- NA
hist(MatDistToursAvt2007, main="Distance between 2 towers (Before 2007)",xlab="Distances (m)")

#######################################
#Clusters parameters setting
minPtsvalue <- 4
leps <- list(100, 125, 150, 175, 200, 225, 250)

### PROCESS CLUSTERING ###
for (i in 1:length(leps)){
  eps0 <- leps[i]  
  #Tours (total)
  ClusterToursEps <- dbscan(Tours@coords, eps = eps0, MinPts = minPtsvalue)
  print(paste("Tours eps=",
              eps0,
              "nbclasses=",
              nrow(table(ClusterToursEps$cluster)),
              sep=","))
  ResultsTours <- cbind(Tours@data[,2],Tours@coords, ClusterToursEps$cluster)
  colnames(ResultsTours) <- c("EBN","X","Y", paste("Eps",eps0,sep = ""))
  
  #Towers before 2007
  ClusterToursAvt2007Eps <- dbscan(ToursAvt2007@coords, eps = eps0, MinPts = minPtsvalue)
  print(paste("Tours eps=",
              eps0,
              "nbclasses=",
              nrow(table(ClusterToursAvt2007Eps$cluster)),
              sep=","))
  ResultsToursAvt2007 <- cbind(ToursAvt2007@data[,2],ToursAvt2007@coords, ClusterToursAvt2007Eps$cluster)
  colnames(ResultsToursAvt2007) <- c("EBN","X","Y", paste("Eps",eps0,sep = ""))
  
  if (i==1){
    ResTours <- ResultsTours
    ResToursAvt2007 <- ResultsToursAvt2007
  }else{
    ResTours <- merge(ResTours, ResultsTours[,c(1,4)],by = "EBN")
    ResToursAvt2007 <- merge(ResToursAvt2007, ResultsToursAvt2007[,c(1,4)],by = "EBN")
  }
} 

rm(ResultsTours, ResultsToursAvt2007, MatDistToursAvt2007, MatDistTours)
rm(i, leps, minPtsvalue, eps0, ClusterToursEps, ClusterToursAvt2007Eps)

#######################################
# Exportation des fichiers
#write.csv(ResTours, file = "Clusters/ToursClustersTotal.csv",row.names = FALSE)
#write.csv(ResToursAvt2007, file = "Clusters/ToursClustersAvt2007.csv",row.names = FALSE) 

ResTours <- as.data.frame(ResTours)
ResToursAvt2007 <- as.data.frame(ResToursAvt2007)

#######################################
#Cluster evolution analysis
  #set different case possible using 250m as a definitive distance
dfFinal <- ResTours
dfFinal$Eps250 <- dfFinal$Eps250 + 100
dfFinal[dfFinal == 100] <- 0

Clusters <- merge(dfFinal, ResToursAvt2007, by = "EBN", all.x = TRUE)
Clusters <- Clusters[-c(4:9,11:18)]
colnames(Clusters) <- c("EBN", "X","Y","C1","C2")
rm(dfFinal)

#rm NA
#Clusters[is.na(Clusters)] <- 0

#Id clusters
Clusters$C[Clusters$C1 == 0 & is.na(Clusters$C2)] <- 0
Clusters$C[Clusters$C1 == 0 & Clusters$C2 == 0] <- 0
for (i in 1:nrow(Clusters)) {
  if (Clusters$C1[i] != 0) {
    Clusters$C[i] <- Clusters$C1[i]
  }
}

#New clusters
Clusters$Cas[is.na(Clusters$C2) & Clusters$C1 != 0] <- 1
Clusters$Cas[Clusters$C2 == 0 & Clusters$C1 != 0] <- 2
#old clusters
Clusters$Cas[Clusters$C2 != 0 & Clusters$C1 != 0] <- 3
#Hors clusters
Clusters$Cas[Clusters$C == 0] <- 0


#write.csv(Clusters, file = "Clusters/Clusters.csv",row.names = FALSE) 

for (i in 101:max(Clusters$C)) {
  test <- filter(Clusters, C == i)
  for (j in 1:nrow(test)) {
    test$Cas <- max(test$Cas)
  }
  if (i == 101) {
    ClustersFinal <- test
  } else {
    ClustersFinal <- rbind(ClustersFinal,test)
  }
  
}

HorsClusters <- filter(Clusters,C == 0)
ClustersFinal <- rbind(ClustersFinal, HorsClusters)

#######################################
#Extract clusters evolution data
write.csv(ClustersFinal, file = "Clusters/Clusters.csv",row.names = FALSE)
