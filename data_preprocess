library(dplyr)
#data
clev <- read.csv2("/Users/abhinavsharma/Desktop/datasets/processed.cleveland.csv", sep = "")
swiss <- read.csv2("/Users/abhinavsharma/Desktop/datasets/processed.switzerland.csv", sep = ",")
va <- read.csv2("/Users/abhinavsharma/Desktop/datasets/processed.va.csv", sep = ",")



#removing the last three features
clev <- clev[,-c(11,12,13)]
va <- va[,-c(11,12,13)]
va[va == "?"] <- NA
#replacing missing values from data va to median
for (i in 1:ncol(va)){ 
  va[,i] <- as.numeric(va[,i])
  va[is.na(va[,i]),i]<- median(va[,i], na.rm =TRUE)
  } 
#combining va and clev
for (i in 1:ncol(clev)){ 
  clev[,i] <- as.numeric(clev[,i])
  }
clev <- clev %>% rename(class = num)
comb_data <- bind_rows(clev, va)
swiss <- swiss[,-c(11,12,13)]
for (i in 1:ncol(swiss)){ 
  swiss[,i] <- as.numeric(swiss[,i])
  swiss[is.na(swiss[,i]),i]<- median(swiss[,i], na.rm =TRUE)
} 
#exporting combined va and clev dataset
write.csv(comb_data,"/Users/abhinavsharma/Desktop/datasets/training_data.csv", row.names = FALSE)
write.csv(swiss,"/Users/abhinavsharma/Desktop/datasets/testing_data.csv", row.names = FALSE)
