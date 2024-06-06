# Graph 1: 30 year annual precipitation seasonality average across 12 populations
timeSer3 <- read.csv("Data/ClimateWNA/climateWNA_input3_62km_1979-2009M.csv")

# Change ID2
timeSer3$ID2 <- rep(1:12000, 31)

# calculate biovars and export

tmax3 <- as.matrix(timeSer3[, paste("Tmax", paste(ifelse(1:12 < 10, "0", ""), 
                                                  as.character(1:12), sep = ""), sep = "")], ncol = 12)

tmin3 <- as.matrix(timeSer3[, paste("Tmin", paste(ifelse(1:12 < 10, "0", ""), 
                                                  as.character(1:12), sep = ""), sep = "")], ncol = 12)

prec3 <- as.matrix(timeSer3[, paste("PPT", paste(ifelse(1:12 < 10, "0", ""), 
                                                 as.character(1:12), sep = ""), sep = "")], ncol = 12)
library(dismo)
bio3 <- biovars(prec3, tmin3, tmax3)

# Add biovars to data.frame

timeSer3 <- cbind(timeSer3, bio3)

# Replace values of -9999 with NA

timeSer3[timeSer3 == -9999] <- NA

# Average by site for 30 year normal (1979 - 2010)

yearmin <- 1979
yearmax <- 2009

# Change all temperatures to Kelvin
tempColumns <- c(grep("T[max|min|ave]", colnames(timeSer3)), which(colnames(timeSer3) 
                                                                   %in% c("bio1", "bio5", "bio6", "bio8", "bio9", "bio10", "bio11")))
timeSer3[, colnames(timeSer3)[tempColumns]] <- timeSer3[, 
                                                        colnames(timeSer3)[tempColumns]] + 273.16
timeSer3 <- subset(timeSer3, timeSer3$Year >= yearmin & timeSer3$Year <= yearmax)

tmp <- read.csv("../Climate/hbPopulations.csv")
timeSer3$PopID <- tmp$ID[match(timeSer3$ID1, tmp$Site)]
#write.csv(timeSer3, "timeSer3.csv")
#timeSer3 <- read.csv("timeSer3.csv")

library(tibble)
foc12.avg30 <- as.data.frame(apply(timeSer3[, 4:(ncol(timeSer3) - 1)], 2, function(X) tapply(X,
                                                                                             timeSer3$PopID, mean, na.rm = TRUE)))
d <- foc12.avg30
Site <- rownames(d)
rownames(d) <- NULL
foc12.avg30 <- cbind(Site,d)

#write_csv(foc12.avg30,"ForHaley/foc12.avg30.csv")
foc12.avg30 <- read.csv("Data/foc12.avg30.csv")
plot(foc12.avg30$Latitude, foc12.avg30$bio15)
foc12.avg30$Pop <- foc12.avg30$Pop

foc12.avg30.reg <- lm(bio15~Latitude,data=foc12.avg30)
summary(foc12.avg30.reg )
#R2: 0.7952
#p-value (for slope): 0.74e-05

library(ggplot2)
avg30_bio15 <- ggplot(foc12.avg30, aes(Latitude, bio15))+
  geom_point(aes(group=Region, colour=Region), size = 5)+
  scale_color_manual(values= c("South"="#FFA100", "Center"="yellow2", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100",  "Center"="yellow2","North"="skyblue3"))+
  ylab("Average Precipitation Seasonality")+
  ylim(NA,140)+
  xlab("Latitude")+
  theme_classic()
avg30_bio15 <-avg30_bio15 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
avg30_bio15 <- avg30_bio15+
  geom_smooth(method=lm,color='black',alpha=0.2)#+
  #annotate("text",x=34,y=100,label=bquote(~R^{2}==0.795), hjust = 0)+
  #annotate("text",x=34,y=92,label=bquote(p-value<0.0001), hjust= 0)
avg30_bio15

#GRAPH 2: 30 year average covariation of variance of precipitation variability
#in 12 populations

foc12.cva30 <- as.data.frame(apply(timeSer3[, 4:(ncol(timeSer3) - 1)], 2, function(X) tapply(X,
                                                                                             timeSer3$PopID, function(X) sd(X, na.rm = TRUE) / mean(X, na.rm = TRUE))))

foc12.cva30[, c("Latitude", "Longitude", "Elevation")] <- foc12.avg30[, c("Latitude",
                                                                          "Longitude", "Elevation")]

for (i in 4:ncol(foc12.cva30)) {
  
  # Make Inf's NA
  foc12.cva30[is.infinite(foc12.cva30[, i]), i] <- NA
  
  #Replace NA's with 0's for cases where all values are 0
  if (any(is.na(foc12.cva30[, i]), na.rm = T) & any(foc12.avg30[, i] == 0, na.rm = T)) {
    foc12.cva30[which(foc12.avg30[, i] == 0), i] <- 0
    
  }
  
}


foc12.cva30[, c("Latitude", "Longitude", "Elevation")] <- foc12.avg30[, c("Latitude",
                                                                          "Longitude", "Elevation")]


a <- foc12.cva30
Site <- rownames(a)
rownames(a) <- NULL
foc12.cva30 <- cbind(Site,a)

#For sanity check
cva12 <- tibble(foc12.cva30$Site, foc12.cva30$bio15)
colnames(cva12)<- c("Site","BIO15")

#write_csv(foc12.cva30,"ForHaley/foc12.cva30.csv")
foc12.cva30 <- read_csv("Data/foc12.cva30.csv")
plot(foc12.cva30$Latitude, foc12.cva30$bio15)

foc12.cva30.reg <- lm(bio15~Latitude,data=foc12.cva30)
summary(foc12.cva30.reg)
#P-value: 0.017889
#R-quared: 0.4445

cva30_bio15 <- ggplot(foc12.cva30, aes(Latitude, bio15))+
  geom_point(aes(group=Region, colour=Region), size = 5)+
  scale_color_manual(values= c("South"="#FFA100", "Center"="yellow2", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100",  "Center"="yellow2","North"="skyblue3"))+
  ylab("Interannaul Precipitation Seasonality")+
  ylim(NA,0.35)+
  xlab("Latitude")+
  theme_classic()
cva30_bio15 <-cva30_bio15 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
cva30_bio15 <- cva30_bio15+
  geom_smooth(method=lm,color='black',alpha=0.2)#+
  #annotate("text",x=34,y=0.16,label=bquote(~R^{2}==0.4445), hjust=0)+
  #annotate("text",x=34,y=0.15,label=bquote(p-value<0.02), hjust= 0)
cva30_bio15

#to obtain just the averages of the regions
regional_avg <- foc12.cva30 %>% group_by(Region) %>%
  summarize(mean=mean(bio15))

#Graph 3: 4 year average precipitation seasonality during drought in 12 populations 

timeSer4 <- read.csv("climateWNA_input3_62km_2010-2016M.csv")

timeSer4 <- timeSer4 %>%
  filter(Year %in% 2011:2015)

# Change ID2 
timeSer4$ID2 <- rep(1:12000, 5)

# calculate biovars and export

tmax4 <- as.matrix(timeSer4[, paste("Tmax", paste(ifelse(1:12 < 10, "0", ""), 
                                                  as.character(1:12), sep = ""), sep = "")], ncol = 12)

tmin4 <- as.matrix(timeSer4[, paste("Tmin", paste(ifelse(1:12 < 10, "0", ""), 
                                                  as.character(1:12), sep = ""), sep = "")], ncol = 12)

prec4 <- as.matrix(timeSer4[, paste("PPT", paste(ifelse(1:12 < 10, "0", ""), 
                                                 as.character(1:12), sep = ""), sep = "")], ncol = 12)

bio4 <- biovars(prec4, tmin4, tmax4)

# Add biovars to data.frame

timeSer4 <- cbind(timeSer4, bio4)

# Replace values of -9999 with NA

timeSer4[timeSer4 == -9999] <- NA

yearmin <- 2012
yearmax <- 2015

# Change all temperatures to Kelvin
tempColumns <- c(grep("T[max|min|ave]", colnames(timeSer4)), which(colnames(timeSer4) 
                                                                   %in% c("bio1", "bio5", "bio6", "bio8", "bio9", "bio10", "bio11")))
timeSer4[, colnames(timeSer4)[tempColumns]] <- timeSer4[, 
                                                        colnames(timeSer4)[tempColumns]] + 273.16
timeSer4 <- subset(timeSer4, timeSer4$Year >= yearmin & timeSer4$Year <= yearmax)

tmp <- read.csv("../Climate/hbPopulations.csv")
timeSer4$PopID <- tmp$ID[match(timeSer4$ID1, tmp$Site)]

to_plot <- timeSer4%>%
  group_by(PopID)%>%
  summarise(mean_bio_15=mean(bio15))%>%
  rename(ID=PopID)%>%
  inner_join(tmp)

#write_csv(to_plot,"ForHaley/graph3data.csv")
foc12.avg4 <- read_csv("Data/graph3data.csv")
plot(foc12.avg4$Lat,to_plot$mean_bio_15)

foc12.avg4.reg <- lm(mean_bio_15~poly(Lat,2),data=foc12.avg4)
summary(foc12.avg4.reg)
#R-squared: 0.5258
#p-values: 0.709 (linear), 0.012 (quadratic)

avg4_bio15 <- ggplot(foc12.avg4, aes(Lat, mean_bio_15))+
  geom_point(aes(group=Region, colour=Region), size = 5)+
  scale_color_manual(values= c("South"="#FFA100", "Center"="yellow2", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100",  "Center"="yellow2","North"="skyblue3"))+
  ylab("Average Precipitation Seasonality")+
  ylim(NA,140)+
  xlab("Latitude")+
  theme_classic()
avg4_bio15 <-avg4_bio15 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
avg4_bio15 <- avg4_bio15+
  geom_smooth(method=lm,formula=y~poly(x,2),color='black',alpha=0.2)#+
  #annotate("text",x=38,y=87,label=bquote(~R^{2}==0.5258))+
  #annotate("text",x=38,y=85,label=bquote(p-value==0.012 (quadratic)))
avg4_bio15

#Graph 4: 4 year coefficient of variation in precipitation seasonality during
#a drought in 12 populations

focSA.avg7 <- as.data.frame(apply(timeSer4[,4:(ncol(timeSer4) - 1)], 2, function(X) tapply(X,
                                                                                           timeSer4$ID2, mean, na.rm = TRUE)))
focSA.cva7 <- as.data.frame(apply(timeSer4[, 4:(ncol(timeSer4) - 1)], 2, function(X) tapply(X, 
                                                                                            timeSer4$ID2, function(X) sd(X, na.rm = TRUE) / mean(X, na.rm = TRUE))))

for (i in 4:ncol(focSA.cva7)) {
  
  # Make Inf's NA
  focSA.cva7[is.infinite(focSA.cva7[, i]), i] <- NA
  
  # Replace NA's with 0's for cases where all values are 0
  if (any(is.na(focSA.cva7[, i]), na.rm = T) & any(focSA.avg7[, i] == 0, na.rm = T)) {
    focSA.cva7[which(focSA.avg7[, i] == 0), i] <- 0
    
  }
  
}
focSA.cva7[, c("Latitude", "Longitude", "Elevation")] <- focSA.avg7[, c("Latitude", 
                                                                        "Longitude", "Elevation")]

focSA.avg7$PopID <- timeSer4$PopID[1:12000]
# focSA.var30$PopID <- timeSer3$PopID[1:16000]
focSA.cva7$PopID <- timeSer4$PopID[1:12000]

library(tidyverse)
to_plot_cva <- focSA.cva7 %>% group_by(PopID) %>% summarise_each(funs(mean))

#write.csv(to_plot_cva,"ForHaley/to_plot_cva.csv")
foc12.cva4 <- read_csv("Data/to_plot_cva.csv")


foc12.cva4.reg <- lm(bio15~Latitude,data=foc12.cva4)
summary(foc12.cva4.reg)
#R^2 = 0.5894
#p-value=0.00355

plot(foc12.cva4$Latitude,foc12.cva4$bio15)

cva4_bio15 <- ggplot(foc12.cva4, aes(Latitude, bio15))+
  geom_point(aes(group=Region, colour=Region), size = 5)+
  scale_color_manual(values= c("South"="#FFA100", "Center"="yellow2", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100",  "Center"="yellow2","North"="skyblue3"))+
  ylab("Interannual Precipitation Seasonality")+
  ylim(NA, 0.35)+
  xlab("Latitude")+
  theme_classic()
cva4_bio15 <- cva4_bio15 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
cva4_bio15<- cva4_bio15+
  geom_smooth(method=lm,formula=y~x,color='black',alpha=0.2)#+
  #annotate("text",x=41,y=0.2,label=bquote(~R^{2}==0.5894))+
  #annotate("text",x=41,y=0.18,label=bquote(p-value==0.00355))
cva4_bio15

#to obtain just the averages of the regions
regional_avg4 <- foc12.cva4 %>% group_by(Region) %>%
  summarize(mean=mean(bio15))

library(cowplot)

#save 10x8
plot_grid(avg30_bio15,cva30_bio15,avg4_bio15,cva4_bio15)


