########### Data wrangling for stomata and trichomes ###########
rep1 <- read.csv("Data/Rep1_stomata_trich.csv")
rep2 <- read.csv("Data/Rep2_stomata_trich.csv")
rep3 <- read.csv("Data/Rep3_stomata_trich.csv")


###### Add IDs to Rep 1 
Rand_ID <- read.csv("Data/RandomID.csv")
names(Rand_ID)[names(Rand_ID) == 'ID'] <- 'Plant_Number'
rep1 <- merge(rep1,Rand_ID, by="Plant_Number")
rep1 <- subset(rep1, Growth_time != "be")

# grab only columns we want
rep1 <- rep1[,-c(1,3,5:10,18)]
names(rep1)[names(rep1)== 'Timeline'] <- 'ID'
rep1_all <-rep1


### rep 2
ID_info <- Rand_ID
names(ID_info)[names(ID_info) == 'Timeline'] <- 'ID'
ID_info$Plant_Number <-NULL
ID_info <- subset(ID_info, Treatment != "Dry")
ID_info$Treatment <- NULL
ID_info$Unique <- NULL

names(rep2)[names(rep2) == 'Plant_ID'] <- 'ID'
rep2 <- merge(rep2,ID_info, by="ID")

rep2_all <-rep2

#### rep 3
rep3 <- rep3[,-c(5:7,9:11, 13:29)]
rep3 <- merge(rep3,ID_info, by="ID")
rep3$Treatment <- gsub('w', 'Wet', rep3$Treatment)
rep3$Treatment <- gsub('d', 'Dry', rep3$Treatment)
rep3$Rep <- 3
rep3$Growth_time <- NULL

## merge all
all <- rbind(rep1, rep2, rep3)
all$unique <- paste(all$ID, all$Treatment, all$Rep, sep= "-")

#write.csv(all, file = "Data/all_stom_trich_mar30.csv")