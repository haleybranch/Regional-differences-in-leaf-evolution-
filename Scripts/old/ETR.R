# ETR
library(tidyverse)
rep1 <- read.csv("Data/Rep_1_etr.csv")
rep1$ID <- rep1$Timeline
rep1$Timeline <-NULL
rep2 <- read.csv("Data/Rep_2_etr.csv")
rep3 <- read.csv("Data/Rep_3_etr.csv")

dat <- rbind(rep1, rep2, rep3)

#calculate J = 4*(A+Rd)
#Rd = -0.5
dat$J <- 4*(dat$A - 0.5)

#calculate phiPSII = (alpha * beta * phiCO2)/4
#alpha = 93
#beta = 0.5
dat$PhiPSII <- (93*0.5*dat$PhiCO2)/4

#caluclate ETR = PPFD * alpha * 0.5 * PhiPSII
dat$ETR <- 1200*0.93*0.5*dat$PhiPSII

#write.csv(dat, "Data/ETR_calc_apr24.csv")

dat$Region <- gsub('S02', 'South', dat$Site)
dat$Region <- gsub('S07', 'South', dat$Region)
dat$Region <- gsub('S11', 'South', dat$Region)
dat$Region <- gsub('S15', 'North', dat$Region)
dat$Region <- gsub('S16', 'North', dat$Region)
dat$Region <- gsub('S36', 'North', dat$Region)

dat$Treatment <- gsub('dry', 'Dry', dat$Treatment)
dat$Treatment <- gsub('wet', 'Wet', dat$Treatment)
dat$Treatment <- gsub('Wet2020', 'Wet', dat$Treatment)


dat$Unique<-NULL
dat$Name <- NULL
dat$X <- NULL
dat <- dat %>% filter(J>0)



library(lmerTest)
library(lme4)
library(lmtest)

mod1 <- lmer(ETR ~ Treatment*Region*PrePeak + (1|Site/ID) + (1|Rep),data=dat)


emmeans(mod1, list(pairwise ~ Treatment*Region*PrePeak), adjust = "tukey")#notta



library(visreg)
vis_etr_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))

vis_etr_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_etr_D<-vis_etr_dry$res ; Res_etr_W<-vis_etr_wet$res # Extract residuals
Res_etr_all<-rbind(Res_etr_D, Res_etr_W) #Row bind wet and dry residuals into one data frame


library(ggplot2)
level_order <- factor(Res_etr_all$PrePeak, level = c('Pre', 'Peak'))
Res_etr_all_plot <-ggplot(Res_etr_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_boxplot(aes(colour=Treatment))+
  facet_wrap(.~Region)+
  scale_y_continuous(name="Electron Transport Rate")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_etr_all_plot <-Res_etr_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_etr_all_plot <- Res_etr_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_etr_all_plot

#save 8x6