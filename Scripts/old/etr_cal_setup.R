
library(tidyverse)


#### Data wrangling 
licor_data <-list.files(path = "Data/LICOR", recursive=T,pattern="*.csv", full.names = T) #finds the files in the "LICOR" Folder
licor_data_Names <-gsub(".csv","", licor_data) %>% gsub("Data/LICOR/","",.) #grabs file names

for (i in 1:length(licor_data)) {
  assign(licor_data_Names[i], read.csv(licor_data[i], 
                                       skip = 13,header = T, as.is=T))
}



# make a list of all the dataframes
l.df <- lapply(ls(), function(x) if (class(get(x)) == "data.frame") get(x))

#remove row 2
for(i in 1:length(licor_data)){
  l.df[[i]] <- l.df[[i]][-1,]
}

#Make file names into dataframe
file_names <- as.data.frame(licor_data_Names) %>% separate(licor_data_Names, into =paste("ID", 1:6, sep="-"))
file_names <- file_names %>% slice(rep(1:n(), each=13))

#subset of columns obs, A, Ca, Ci, gsw, Tleaf, Q, file_name
for(i in 1:length(licor_data)){
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","PhiCO2"))
}

#merge l.df into one dataframe, ensuring columns stay together 
for(i in 1:length(licor_data)){
  df<- bind_rows(l.df[[i]])
}
library(data.table)
df<-rbindlist(l.df)

#create factor with all the names
ID <- file_names

#Remove row obs with number 14 = extra data point at 1500 CO2 ppm  
df <- df[df$obs !=14, ]

#add new column and merge names into dataframe
df$ID <- ID$`ID-1`
df$Treatment <- ID$`ID-2`
#df$Year <- ID$`ID-3`
#df$Site <- ID$`ID-4`

# Remove file name and replace with ID
#df <- df %>% separate(file_names, 
#          c("ID", "Treatment", "Year"))

#add rep # column
df <- mutate(df, Rep = 1)

etr <- df %>% filter(obs == 7)
etr$Treatment <- NULL


###### Add IDs to Rep 1 
Rand_ID <- read.csv("Data/RandomID.csv")
Rand_ID$ID <- as.character(Rand_ID$ID)
rep1 <- merge(etr,Rand_ID, by="ID")
rep1$Rep <- 1
rep1$ID <- NULL
rep1$obs <- NULL

#write.csv(rep1, file = "Data/Rep_1_etr.csv")


#### Data wrangling rep 2
licor_data2 <-list.files(path = "Data/LICOR_rep2", recursive=T,pattern="*.csv", full.names = T) #finds the files in the "LICOR" Folder
licor_data_Names2 <-gsub(".csv","", licor_data2) %>% gsub("Data/LICOR_rep2/","",.) #grabs file names

for (i in 1:length(licor_data2)) {
  assign(licor_data_Names2[i], read.csv(licor_data2[i], 
                                        skip = 13,header = T, as.is=T))
}


# make a list of all the dataframes
l.df <- lapply(ls(), function(x) if (class(get(x)) == "data.frame") get(x))

#remove row 2
for(i in 1:length(licor_data2)){
  l.df[[i]] <- l.df[[i]][-1,]
}

#Make file names into dataframe
file_names <- as.data.frame(licor_data_Names2) %>% separate(licor_data_Names2, into =paste("ID", 1:6, sep="-"))
file_names <- file_names %>% slice(rep(1:n(), each=13))

#subset of columns obs, A, Ca, Ci, gsw, Tleaf, Q, file_name
for(i in 1:length(licor_data2)){
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","PhiCO2"))
}

#merge l.df into one dataframe, ensuring columns stay together 
for(i in 1:length(licor_data2)){
  df<- bind_rows(l.df[[i]])
}
library(data.table)
df<-rbindlist(l.df)

#create factor with all the names
ID <- file_names

#Remove row obs with number 14 = extra data point at 1500 CO2 ppm  
df <- df[df$obs !=14, ]

#add new column and merge names into dataframe
df$ID <- ID$`ID-1`
df$Treatment <- ID$`ID-2`
#df$Year <- ID$`ID-3`
#df$Site <- ID$`ID-4`

# Remove file name and replace with ID
#df <- df %>% separate(file_names, 
#          c("ID", "Treatment", "Year"))

#add rep # column
df <- mutate(df, Rep = 2)

etr <- df %>% filter(obs == 7)

###### Add IDs to Rep 1 
Rand_ID <- read.csv("Data/RandomID.csv")
Rand_ID$ID <- NULL
Rand_ID$ID <- Rand_ID$Timeline
Rand_ID <- Rand_ID %>% filter(Treatment=="Wet")
Rand_ID$Treatment <- NULL
rep2 <- merge(etr,Rand_ID, by="ID")
rep2$Timeline <- NULL
rep2$obs <- NULL

#write.csv(rep2, file = "Data/Rep_2_etr.csv")

############################################################
#### Data wrangling rep 3###################################
licor_data3 <-list.files(path = "Data/LICOR_rep3", recursive=T,pattern="*.csv", full.names = T) #finds the files in the "LICOR" Folder
licor_data_Names3 <-gsub(".csv","", licor_data3) %>% gsub("Data/LICOR_rep3/","",.) #grabs file names

for (i in 1:length(licor_data3)) {
  assign(licor_data_Names3[i], read.csv(licor_data3[i], 
                                        skip = 13,header = T, as.is=T))
}


# make a list of all the dataframes
l.df <- lapply(ls(), function(x) if (class(get(x)) == "data.frame") get(x))

#remove row 2
for(i in 1:length(licor_data3)){
  l.df[[i]] <- l.df[[i]][-1,]
}

#Make file names into dataframe
file_names <- as.data.frame(licor_data_Names3) %>% separate(licor_data_Names3, into =paste("ID", 1:6, sep="-"))
file_names <- file_names %>% slice(rep(1:n(), each=13))

#subset of columns obs, A, Ca, Ci, gsw, Tleaf, Q, file_name
for(i in 1:length(licor_data3)){
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","PhiCO2"))
}

#merge l.df into one dataframe, ensuring columns stay together 
for(i in 1:length(licor_data3)){
  df<- bind_rows(l.df[[i]])
}
library(data.table)
df<-rbindlist(l.df)

#create factor with all the names
ID <- file_names

#Remove row obs with number 14 = extra data point at 1500 CO2 ppm  
df <- df[df$obs !=14, ]

#add new column and merge names into dataframe
df$ID <- ID$`ID-1`
df$Treatment <- ID$`ID-2`

#add rep # column
df <- mutate(df, Rep = 3)


etr <- df %>% filter(obs == 7)

###### Add IDs to Rep 1 
Rand_ID <- read.csv("Data/RandomID.csv")
Rand_ID$ID <- NULL
Rand_ID$ID <- Rand_ID$Timeline
Rand_ID <- Rand_ID %>% filter(Treatment=="Wet")
Rand_ID$Treatment <- NULL
rep3 <- merge(etr,Rand_ID, by="ID")
rep3$Timeline <- NULL
rep3$obs <- NULL

#write.csv(rep3, file = "Data/Rep_3_etr.csv")

