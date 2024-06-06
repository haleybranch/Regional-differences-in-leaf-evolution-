########### mesophyll conductance ####################
### Photocompensation point

library(plantecophys)

dry1 <- read.csv("Data/Li-photocomp_point/373_w.csv")

dry1$Wavelength <- as.character(dry1$Wavelength)
dry1$Photo <- dry1$A
aci.fit <- fitacis(dry1, "Wavelength")
plot(aci.fit, "oneplot")

func=function(x,m1,m2,m3){
  val1=m1$Photosyn(Ci=x)$ALEAF
  val2=m2$Photosyn(Ci=x)$ALEAF
  val3=m3$Photosyn(Ci=x)$ALEAF
  (val1-val2)^2+(val2-val3)^2+(val3-val1)^2
}

optimize(func,range(x),aci.fit$`100`,aci.fit$`200`,aci.fit$`500`)

x_value <- optimize(func,range(x),aci.fit$`100`,aci.fit$`200`,aci.fit$`500`)$minimum
y_value_1 <- aci.fit$`100`$Photosyn(Ci=x_value)$ALEAF
y_value_2 <- aci.fit$`200`$Photosyn(Ci=x_value)$ALEAF
y_value_3 <- aci.fit$`500`$Photosyn(Ci=x_value)$ALEAF
y_intercept <- (y_value_1+y_value_2+y_value_3)/3
y_intercept
#numerically find value where the three curves are closest, sum of squared distances and then minimized this with the optimze function in R
# to find y value, took average of the three curves at the point where they were closest 


##### ETR #######
#ETR = PhiPS2*1200*0.93*0.5
chl_fl <- read.csv("Data/Chl_fl_all.csv")

ETR=function(x){
  x*1200*0.93*0.5
}

chl_fl <- mutate(chl_fl, ETR_cal = ETR(PhiPS2))


chl_fl$Rd <- -0.5046585
chl_fl$comp_point <- 49.052501


#write.csv(chl_fl, file = "Data/ETR.csv")



##### Gm #######

### grab Ci and A for each rep
rep1 <- data.frame(read.csv("Data/Rep_1_Aci.csv"))
random <- data.frame(read.csv("Data/RandomID.csv"))

rep1$X <- NULL

filtered <- rep1 %>% group_by(ID) %>% filter(Ci==max(Ci)) %>% distinct()

dat <- merge(random,filtered, by="ID")
dat$Treatment.y <- NULL
dat$Treatment <- dat$Treatment.x
dat$Treatment.x <- NULL
dat$ID <- dat$Timeline
dat$Timeline <- NULL
dat$Year <- as.character(dat$Year)
dat$Treatment <- ifelse(dat$Treatment == "Wet", "wet", "dry")
dat$Unique <- NULL


rep2 <- data.frame(read.csv("Data/Rep_2_Aci.csv"))
rep2$X <- NULL
IDs_rep23 <- data.frame(read.csv("Data/ID_rep2_rep3.csv"))

dat2 <- rep2 %>% group_by(ID, Treatment) %>% filter(Ci==max(Ci))
dat2$Year <- as.character(dat2$Year)
dat2 <- na.omit(dat2)
IDs_rep23<- distinct(IDs_rep23)
IDs_rep23$Year <- as.character(IDs_rep23$Year)
dat2 <- inner_join(dat2, IDs_rep23, by="ID")
dat2$Site.y <- NULL
dat2$Year.y <- NULL
names(dat2)[names(dat2) == 'Site.x'] <- 'Site'
names(dat2)[names(dat2) == 'Year.x'] <- 'Year'
dat2 <- na.omit(dat2)

rep3 <- data.frame(read.csv("Data/Rep_3_Aci.csv"))
rep3$X <- NULL

dat3 <- rep3 %>% group_by(ID, Treatment) %>% filter(Ci==max(Ci))
dat3 <- inner_join(dat3, IDs_rep23, by="ID")
dat3$Site.y <- NULL
dat3$Year.y <- NULL
names(dat3)[names(dat3) == 'Site.x'] <- 'Site'
names(dat3)[names(dat3) == 'Year.x'] <- 'Year'

data <- rbind(dat, dat2, dat3)
data$Unique <- paste(data$ID, data$Treatment, data$Rep, sep= "_")

chl_fl$Unique <- paste(chl_fl$Timeline, chl_fl$Treatment, chl_fl$Rep, sep= "_")
chl_fl$A <- NULL

data <- inner_join(data,chl_fl, by="Unique")

#Gm = An / [Ci - (ro*(ETR + 8*(An + Rd))/ETR - 4*(An + Rd))]

data$ARd <- data$A + data$Rd #An+Rd
data$ARd8 <- data$ARd*8 #8(An+Rd)
data$ARd8ETR <- data$ARd8 + data$ETR_cal #ETR + 8(An+Rd)
data$ETR49 <- data$ARd8ETR*49.0525 # ro*
data$ARd4 <- data$ARd*4 #4(An+Rd)
data$ETRd4 <- data$ETR_cal - data$ARd4
data$numdom <- data$ETR49/data$ETRd4
data$Cinumdom <- data$Ci - data$numdom
data$gm <- data$A/data$Cinumdom


# select variables v1, v2, v3
myvars <- c("gm", "ID", "Site.x", "Year.x", "PrePeak.x", "Treatment", "Rep", "Region.x")
meso_cond <- data[myvars]

#write.csv(meso_cond, file = "Data/meso_cond.csv")
