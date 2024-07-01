
library(tidyverse)


#### Data wrangling rep 1
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
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","gsw","Tleaf","Q"))
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

#add rep # column
df <- mutate(df, Rep = 1)


#write.csv(df, file = "paper2_leaves/Data/Rep_1_Aci.csv")



############################################################
#### Data wrangling rep 2##############################
licor_data2 <-list.files(path = "paper2_leaves/Data/LICOR_rep2", recursive=T,pattern="*.csv", full.names = T) #finds the files in the "LICOR" Folder
licor_data_Names2 <-gsub(".csv","", licor_data2) %>% gsub("paper2_leaves/Data/LICOR_rep2/","",.) #grabs file names

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
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","gsw","Tleaf","Q"))
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

#add rep # column
df <- mutate(df, Rep = 2)


############################################################
#### Data wrangling rep 3###################################
licor_data3 <-list.files(path = "paper2_leaves/Data/LICOR_rep3", recursive=T,pattern="*.csv", full.names = T) #finds the files in the "LICOR" Folder
licor_data_Names3 <-gsub(".csv","", licor_data3) %>% gsub("paper2_leaves/Data/LICOR_rep3/","",.) #grabs file names

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
  l.df[[i]] <- subset(l.df[[i]], select = c("obs","A","Ca","Ci","gsw","Tleaf","Q"))
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

#write.csv(df, file = "paper2_leaves/Data/Rep_3_Aci.csv")
