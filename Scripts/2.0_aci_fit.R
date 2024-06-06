############# Plantecophys ###################
#Rep 1 ###
mimulus_aci <- data.frame(read.csv("Data/Rep_1_Aci.csv"))
mimulus_aci <- as.data.frame(mimulus_aci)
library(plantecophys)
library(dplyr)
library(tidyr)
library(tidyverse) # for data manipulation
library(lme4) # for mixed models
library(lmtest) # for likelihood ratio tests
library(visreg) # for visualizing effects
library(lmerTest)
library(MuMIn)
df2 <- mimulus_aci

# new dataframe with Curve group, Ci, A, Tleaf, PARi
#  select(Curve, Ci, A, Tleaf, PARi)
mimulus_aci = mimulus_aci %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)

# change name of A to Photo
mimulus_aci$Photo <- mimulus_aci$A
#mimulus_aci$A = NULL
aci.fit <- fitacis(mimulus_aci, "ID_curve")

# All ACi outputs into a table
#Creating a loop for Vcmax, Jmax, Rd, Ci.transition dataframe from ACI Curves

ID_list <- matrix(unique(mimulus_aci$ID_curve)) 
datalist = list()

for (i in 1:length(ID_list)) {
  dat <- data.frame(ID_list[[i]], aci.fit[[i]]$pars[1,-2],aci.fit[[i]]$pars[2,-2],aci.fit[[i]]$pars[3,-2], aci.fit[[i]]$Ci(0))#add here for column
  dat$Ci.transition <- aci.fit[[i]]$Ci_transition
  dat$i <- i
  datalist[[i]] <- dat # add it to your list
}

aci_output = do.call(rbind, datalist)
names(aci_output)= c("ID","Vcmax","Jmax","Rd","CompPoint", "Ci.transition", "i")#add names here before "i" 
aci_output$i <- NULL

#merge IDs together
library(tidyverse)
ID_dat <- mimulus_aci %>% select(c("ID","Treatment","Year","Site","Rep"))
ID_dat$ID2 <- ID_dat$ID
ID_dat = ID_dat %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)
ID_dat$ID <- NULL

ID_dat <- unique(ID_dat)
ID_dat$ID <- ID_dat$ID_curve

aci_output <- merge(aci_output, ID_dat, by = "ID")
aci_output$ID_curve <- NULL
aci_output$ID<-NULL
aci_output$ID = aci_output$ID2
aci_output$ID2 <- NULL
aci_output <- mutate(aci_output, Rep = 2)

#write.csv(aci_output, file="paper2_leaves/Data/aciparameters2.csv")



#Rep 2 ###
mimulus_aci <- data.frame(read.csv("paper2_leaves/Data/Rep_2_Aci.csv"))

df2 <- mimulus_aci

# new dataframe with Curve group, Ci, A, Tleaf, PARi
#  select(Curve, Ci, A, Tleaf, PARi)
mimulus_aci = mimulus_aci %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)

# change name of A to Photo
mimulus_aci$Photo <- mimulus_aci$A
aci.fit <- fitacis(mimulus_aci, "ID_curve")

# plotting individuals
plot(aci.fit, "oneplot")
plot(aci.fit[["1-dry"]])
plot(aci.fit[["4-dry"]])


# All ACi outputs into a table
#Creating a loop for Vcmax, Jmax, Rd, Ci.transition dataframe from ACI Curves

ID_list <- matrix(unique(mimulus_aci$ID_curve)) 
datalist = list()

for (i in 1:length(ID_list)) {
  dat <- data.frame(ID_list[[i]], aci.fit[[i]]$pars[1,-2],aci.fit[[i]]$pars[2,-2],aci.fit[[i]]$pars[3,-2], aci.fit[[i]]$Ci(0))#add here for column
  dat$Ci.transition <- aci.fit[[i]]$Ci_transition
  dat$i <- i
  datalist[[i]] <- dat # add it to your list
}

aci_output = do.call(rbind, datalist)
names(aci_output)= c("ID","Vcmax","Jmax","Rd","CompPoint", "Ci.transition", "i")#add names here before "i" 
aci_output$i <- NULL

#merge IDs together
library(tidyverse)
ID_dat <- mimulus_aci %>% select(c("ID","Treatment","Year","Site","Rep"))
ID_dat$ID2 <- ID_dat$ID
ID_dat = ID_dat %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)
ID_dat$ID <- NULL
#aci_output$i <- NULL

ID_dat <- unique(ID_dat)
ID_dat$ID <- ID_dat$ID_curve

aci_output <- merge(aci_output, ID_dat, by = "ID")
aci_output$ID_curve <- NULL
aci_output$ID<-NULL
aci_output$ID = aci_output$ID2
aci_output$ID2 <- NULL
aci_output <- mutate(aci_output, Rep = 2)

#write.csv(aci_output, file="paper2_leaves/Data/aciparameters2.csv")


#Rep 3 ###
mimulus_aci <- data.frame(read.csv("paper2_leaves/Data/Rep_3_Aci.csv"))

df2 <- mimulus_aci
# combining columns into single string and replaces
#aci.data <- unite_(mimulus_aci, "Curve", c("Site", "Plant", "Year", "Treatment"))

# new dataframe with Curve group, Ci, A, Tleaf, PARi
#aci.sub <- aci.data %>%
#  select(Curve, Ci, A, Tleaf, PARi)
mimulus_aci = mimulus_aci %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)

# change name of A to Photo
mimulus_aci$Photo <- mimulus_aci$A
#mimulus_aci$A = NULL
aci.fit <- fitacis(mimulus_aci, "ID_curve")

# plotting individuals
plot(aci.fit, "oneplot")
plot(aci.fit[["1-dry"]])
plot(aci.fit[["4-dry"]])


# All ACi outputs into a table
#Creating a loop for Vcmax, Jmax, Rd, Ci.transition dataframe from ACI Curves

ID_list <- matrix(unique(mimulus_aci$ID_curve)) 
datalist = list()

for (i in 1:length(ID_list)) {
  dat <- data.frame(ID_list[[i]], aci.fit[[i]]$pars[1,-2],aci.fit[[i]]$pars[2,-2],aci.fit[[i]]$pars[3,-2], aci.fit[[i]]$Ci(0))#add here for column
  dat$Ci.transition <- aci.fit[[i]]$Ci_transition
  dat$i <- i
  datalist[[i]] <- dat # add it to your list
}

aci_output = do.call(rbind, datalist)
names(aci_output)= c("ID","Vcmax","Jmax","Rd","CompPoint", "Ci.transition", "i")#add names here before "i" 
aci_output$i <- NULL

#merge IDs together
library(tidyverse)
ID_dat <- mimulus_aci %>% select(c("ID","Treatment","Year","Site","Rep"))
ID_dat$ID2 <- ID_dat$ID
ID_dat = ID_dat %>% unite(ID_curve, c("ID", "Treatment"), sep = "-", remove = FALSE)
ID_dat$ID <- NULL
#aci_output$i <- NULL

ID_dat <- unique(ID_dat)
ID_dat$ID <- ID_dat$ID_curve

aci_output <- merge(aci_output, ID_dat, by = "ID")
aci_output$ID_curve <- NULL
aci_output$ID<-NULL
aci_output$ID = aci_output$ID2
aci_output$ID2 <- NULL
aci_output <- mutate(aci_output, Rep = 3)

#write.csv(aci_output, file="paper2_leaves/Data/aciparameters3.csv")

