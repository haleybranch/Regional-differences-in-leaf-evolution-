###### graph of regional differences ######
library(tidyverse)
library(ggplot2)

clim <- read.csv("Data/climate.csv")

plot_MAP <- clim %>% 
  group_by(Region) %>% 
  summarize(se=sd(MAP)/3,MAP=mean(MAP)) %>%  
            ggplot( aes(x=Region, y=MAP, colour = Region))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=MAP-se, ymax=MAP+se),size=1.5,width=0)+
  scale_y_continuous(name="Mean Annual \n Preciptation (mm)")+
  scale_x_discrete(name= "")+
  scale_color_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  theme_classic() +
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
  

plot_MAT <- clim %>% 
  group_by(Region) %>% 
  summarize(se=sd(MAT)/3,MAT=mean(MAT)) %>%  
  ggplot( aes(x=Region, y=MAT, colour = Region))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=MAT-se, ymax=MAT+se),size=1.5,width=0)+
  scale_y_continuous(name=expression("Mean Annual\n Temperature ("*~degree*"C)")) +  
  scale_x_discrete(name= "")+
  scale_color_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  theme_classic() +
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
  
  

plot_bio <- clim %>% 
  group_by(Region) %>% 
  summarize(se=sd(CVI)/3,CVI=mean(CVI)) %>%  
  ggplot( aes(x=Region, y=CVI, colour = Region))+
  geom_point(size=3.5)+
  geom_errorbar(aes(ymin=CVI-se, ymax=CVI+se),size=1.5, width=0)+
  scale_y_continuous(name="Precipitation Seasonality \n Covariance of Variation")+
  scale_x_discrete(name= "Region")+
  scale_color_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  scale_fill_manual(values= c("South"="#FFA100", "North"="skyblue3"))+
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")
  
library(cowplot)

plot_grid(plot_MAT, plot_MAP, plot_bio, ncol=1, axis = "l", align = "v")

