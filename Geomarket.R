library(dplyr)
setwd("C:/Users/Baptiste/Desktop")
options(stringsAsFactors = FALSE)

MyData <- read.csv2("TestDataFrame.csv")

Zone80_CA <- function (ID_STORE, ID_CLIENT, CA_CLIENT, X_STORE, Y_STORE, X_CLIENT, Y_CLIENT) 
{
  distEucli <- vector()
  for(i in 1:length(ID_CLIENT)) {
    distEucli[i] <- sqrt((Y_STORE[i] - Y_CLIENT[i])^2 + (X_STORE[i] - X_CLIENT[i])^2)
  }
  df <- data.frame(cbind(ID_STORE, 
                         ID_CLIENT, 
                         CA_CLIENT, 
                         X_STORE, 
                         Y_STORE, 
                         X_CLIENT, 
                         Y_CLIENT, 
                         distEucli))
  
  df <- df[order(df$ID_STORE, df$distEucli),]
  return(df)
}


test <- Zone80_CA(MyData$ID_STORE, 
                  MyData$ID_CLIENT, 
                  MyData$CA_CLIENT, 
                  MyData$X_STORE, 
                  MyData$Y_STORE,
                  MyData$X_CLIENT,
                  MyData$Y_CLIENT)
