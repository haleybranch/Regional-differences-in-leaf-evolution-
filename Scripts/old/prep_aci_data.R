library(dplyr)
library(tidyverse)
#ACi Parameters 
aci_data1 <- read.csv("Data/aciparameters1.csv")
#aci_data1[1:51,11] <- "wet" 
#aci_data1[52:112,11] <- "dry"
#aci_data1 <- aci_data1 %>% unite(ID, c("Timeline", "Treatment"), sep = "-", remove = FALSE)
#aci_data1$Name <- NULL
#aci_data1$Timeline <- NULL
#aci_data1$Unique <- NULL
#aci_data1$PrePeak <- NULL

aci_data2 <- read.csv("Data/aciparameters2.csv")
#aci_data2 <- aci_data2 %>% unite(ID, c("ID", "Treatment"), sep = "-", remove = FALSE)
aci_data3 <- read.csv("Data/aciparameters3.csv")

aci_data <- rbind(aci_data1, aci_data2, aci_data3, by="ID")
aci_data$X <- NULL

#write.csv(aci_data, "paper2_leaves/Data/aci_data_all.csv")
