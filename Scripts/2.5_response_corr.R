### Variable Corrleations ###
library("Hmisc")
library(tidyverse)

#read in physiology data 
A_fl <- read.csv("Data/Chl_fl_all.csv")
A_fl$NPQ <- NULL
A_fl <- na.omit(A_fl)
A_fl$unique <- paste(A_fl$Timeline,A_fl$Treatment,A_fl$Rep, sep="_")
A_fl <- A_fl %>% select("A","unique")

aci_data <- read.csv("Data/aci_data_all.csv")
aci_data$Vcmax <- as.numeric(aci_data$Vcmax)
aci_data$Jmax <- as.numeric(aci_data$Jmax)
aci_data$Ci.transition <- as.numeric(aci_data$Ci.transition)
aci_data <- na.omit(aci_data)
aci_data <- aci_data %>% filter(Vcmax < 200)
aci_data <- na.omit(aci_data)

aci_data$unique <- paste(aci_data$ID,aci_data$Treatment,aci_data$Rep, sep="_")
aci_data2 <- aci_data %>% select("Vcmax", "Jmax", "Ci.transition")

gsw_50 <- data.frame(read.csv("Data/gsw_50.csv"))
gsw_50$unique <- paste(gsw_50$ID,gsw_50$Treatment,gsw_50$Rep, sep="_")
gsw_50 <- gsw_50 %>% select("gsw", "unique")

a_gs <- inner_join(A_fl,gsw_50, by="unique")

#Assimilation vs stomatal conductance vs ACi data
phys <- inner_join(a_gs,aci_data, by="unique")
phys$unique <- NULL
phys_cor <- rcorr(as.matrix(phys)) 
phys_corr=data.frame(phys_cor$r)
#write.csv(phys_corr,'phys_corr.csv')



######## Anatomy #### 
leaves <- read.csv("Data/Mesophyll_proportions_all.csv")


leaves$Average_thick <- as.numeric(leaves$Average_thick)

#omit NA data -- all data that has no values 
# csv goes from 261 to 
leaves <- na.omit(leaves)

#mixed effects model
#lmer(response ~ effect1*effect2*effect3 + (1|random), data)
# effect1: treatment = dry or wet 
# effect2: year = pre or peak (which corresponds to climatic drought timing) 
# effect 3: site = S02, S07, S11, S15, S16, S36 (these are 3 southern sites and 3 northern)
# random1 = replicate
# random2 = PlantID (random sampling of individuals from a given site, not specifically chosen)

#change treatment, prepeak, and site to characters
#column you want to change $ data <- 
leaves$Treatment <- as.character(leaves$Treatment)
leaves$Site <- as.character(leaves$Site)
leaves$PrePeak <- as.character(leaves$PrePeak)
leaves$PlantID <- as.character(leaves$ID)
leaves$Rep <- as.character(leaves$Rep)

#remove S11 and S16
leaves_subset <- subset(leaves,Site != "S11")
leaves_subset2 <- subset(leaves_subset, Site != "S16")
leaves_all <- leaves
leaves <- leaves_subset2   
leaves<- subset(leaves, Rep!="3")
leaves <- na.omit(leaves)
leaves$Treatment<-c("W" = "wet", "D" = "dry")

leaves$unique <- paste(leaves$ID,leaves$Treatment,leaves$Rep, sep="_")
leaves <- leaves %>% select("Average_prop", "Average_thick","unique")

all<-read.csv("Data/all_stom_trich.csv")
all$Treatment<-c("Wet" = "wet", "Dry" = "dry")
all<- subset(all, Rep!="3")

below <- subset(all, Placement == "be")
below$be_s <- below$Stomata
below$be_t <- below$Trichome
below$unique <- paste(below$ID,below$Treatment,below$Rep, sep="_")
above <- subset(all, Placement == "ab")
above$ab_s <- above$Stomata
above$ab_t <- above$Trichome
above$unique <- paste(above$ID,above$Treatment,above$Rep, sep="_")
s_t <- inner_join(above,below, by="unique")
s_t <- s_t %>% select("be_s", "be_t","ab_s", "ab_t", "unique")
s_t_only <- s_t
s_t_only$unique <- NULL
rcorr(as.matrix(s_t_only)) 

anatomy <- inner_join(leaves,s_t, by="unique") 
anatomy$unique <- NULL
ana_cor <- rcorr(as.matrix(anatomy)) 

df.rcx.r=data.frame(ana_cor$r)
write.csv(df.rcx.r,'anatomy_corr.csv')


#### all variables 
phys2 <- inner_join(a_gs,aci_data, by="unique")
s_t2 <- inner_join(above,below, by="unique")
phy_ana <- inner_join(phys2,s_t2, by="unique")
all_dat <- inner_join(phy_ana,leaves, by="unique")
all_dat <- all_dat %>% select("A", "Vcmax", "Jmax", "Ci.transition","gsw","Average_prop","Average_thick", "be_s", "be_t","ab_s", "ab_t", "unique")
all_dat$unique <- NULL
all_var <- rcorr(as.matrix(all_dat)) 
all_var=data.frame(all_var$r)
write.csv(all_var,'all_correlation.csv')
