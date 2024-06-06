
rep1 <- read.csv("Data/sla_rep1.csv")
rep2 <- read.csv("Data/sla.csv")

###### Add IDs to Rep 1 
Rand_ID <- read.csv("Data/RandomID.csv")
#names(Rand_ID)[names(Rand_ID) == 'ID'] <- 'Plant_Number'
rep1 <- inner_join(rep1,Rand_ID, by="ID")
rep1$Unique <- NULL
rep1$Treatment <- gsub('Wet', 'wet', rep1$Treatment)
rep1$Treatment <- gsub('Dry', 'dry', rep1$Treatment)
rep1$ID <- NULL
rep1$ID <- rep1$Timeline
rep1$Timeline <- NULL
rep1$ww <- rep1$Leaf.Wet.Weight
rep1$dw <- rep1$Leaf.Dry.Weight
rep1$Leaf.Wet.Weight <- NULL
rep1$Leaf.Dry.Weight <- NULL
rep1$sa <- rep1$SA
rep1$SA <- NULL

## remove before
rep2 <- subset(rep2, Timing != "Be")
rep2 <- na.omit(rep2)

ID_info <- read.csv("Data/ID_rep2_rep3.csv")
rep2 <- merge(rep2,ID_info, by="ID")
rep2$Timing <- NULL
rep2$Rep <- 2

# bind
dat <- rbind(rep1, rep2)


# SLA column
dat$SLA <- dat$sa/dat$dw

#write.csv(dat, "Data/sla_all.csv")
## call data