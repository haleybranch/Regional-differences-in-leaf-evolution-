#read in csv
chl_fl <- read.csv("Data/Chl_fl_all.csv")
chl_fl <- na.omit(chl_fl)

library(lmerTest)
library(lme4)
mod1 <- lmer(Fv.Fm ~ Treatment*Site*PrePeak + (1|Timeline) + (1|Rep),
             control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

# drop 3way
no3way.mod1 <- lmer(Fv.Fm ~ Treatment*PrePeak + Treatment*Site +
                      PrePeak*Site + (1|Timeline), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

lrtest(mod1, no3way.mod1) # accept no3way model

#drop Site
no2way.mod1 <- lmer(Fv.Fm ~Treatment*PrePeak + (1|Timeline), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

lrtest(no2way.mod1, no3way.mod1) # accept no2way model
#drop Prepeak
no2way.mod2 <- lmer(Fv.Fm ~Treatment*Site + (1|Timeline), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

lrtest(no2way.mod2, no3way.mod1) # accept no2way
lrtest(no2way.mod1, no2way.mod2) #accept no2way with site dropped, include prepeak

#Main effects
no1way.mod1 <- lmer(Fv.Fm ~Treatment + (1|Timeline), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

lrtest(no2way.mod1, no1way.mod1) # accept main effect Average~Treatment

no1way.mod2 <- lmer(Fv.Fm ~PrePeak + (1|Timeline), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)
lrtest(no1way.mod2, no1way.mod1) # accept main effect Average~Treatment 


anova(no1way.mod1) #not significant p = 0.735
summary(no1way.mod1)



library(visreg)
vis_chl<-visreg(no1way.mod1, xvar="Treatment")
Res_chl<-vis_chl$res  # Extract residuals


library(ggplot2)
Res_thickness_all_plot <-ggplot(Res_chl, aes(Treatment, y=visregRes))+
  geom_boxplot()+
  ylab("Fv/Fm")+
  ylim(0.7,1)+
  xlab("Treatment")+
  theme_classic()
Res_thickness_all_plot <-Res_thickness_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_thickness_all_plot +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

Amax.all = lmer(A~ Treatment*Site*PrePeak + (1|Timeline), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), chl_fl)
summary(Amax.all)
anova(Amax.all)

vis_Amax_D<-visreg(Amax.all, xvar="PrePeak", by="Site", cond=list(Treatment="D")) #set up visreg for Drought
vis_Amax_W<-visreg(Amax.all, xvar="PrePeak", by="Site", cond=list(Treatment="W")) #set up visreg for Wet
Res_Amax_D<-vis_Amax_D$res ; Res_Amax_W<-vis_Amax_W$res # Extract residuals
Res_Amax_all<-rbind(Res_Amax_D, Res_Amax_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("S11"="South3", "S07"="South2", "S02"="South1", "S36"="North3","S15"="North1", "S16" = "North2")
Res_Amax_all_plot<-ggplot(Res_Amax_all, aes(Treatment, y=visregRes, colour=PrePeak))+
  geom_boxplot(aes(colour=Treatment), size=0.2)+
  #  geom_text()+
  geom_smooth(method="lm",aes(colour=Treatment,fill=Treatment))+
  facet_wrap(.~Site, labeller = labeller(Site=Site_Labs))+
  scale_x_discrete(name= "Treatment") +
  scale_y_continuous(name="Max Photosynthetic Rate")+
  theme_classic()
Res_Amax_all_plot <- Res_Amax_all_plot + theme(legend.position = "none",
                                                 axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                                 axis.text.y = element_text(size=12,face="bold"),
                                                 axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_Amax_all_plot + facet_wrap(.~Site, labeller = labeller(Site=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#####

vis_ETR_D<-visreg(ETR.all, xvar="PrePeak", by="Site", cond=list(Treatment="D")) #set up visreg for Drought
vis_ETR_W<-visreg(ETR.all, xvar="PrePeak", by="Site", cond=list(Treatment="W")) #set up visreg for Wet
Res_ETR_D<-vis_ETR_D$res ; Res_ETR_W<-vis_ETR_W$res # Extract residuals
Res_ETR_all<-rbind(Res_ETR_D, Res_ETR_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("S11"="South3", "S07"="South2", "S02"="South1", "S36"="North3","S15"="North1", "S16" = "North2")
Res_ETR_all_plot<-ggplot(Res_ETR_all, aes(Treatment, y=visregRes, colour=PrePeak))+
  geom_boxplot(aes(colour=Treatment), size=0.2)+
  #  geom_text()+
  geom_smooth(method="lm",aes(colour=Treatment,fill=Treatment))+
  facet_wrap(.~Site, labeller = labeller(Site=Site_Labs))+
  scale_x_discrete(name= "Treatment") +
  scale_y_continuous(lim=c(0,300), name="Electron Transport Rate")+
  theme_classic()
Res_ETR_all_plot <- Res_ETR_all_plot + theme(legend.position = "none",
                                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                             axis.text.y = element_text(size=12,face="bold"),
                                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_ETR_all_plot + facet_wrap(.~Site, labeller = labeller(Site=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))



Amax.all = lmer(A~ Treatment*Site*PrePeak + (1|Timeline), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), chl_fl)
summary(Amax.all)
anova(Amax.all)

vis_Amax_D<-visreg(Amax.all, xvar="Treatment", by=c("Site","PrePeak"), cond=list(Treatment="D")) #set up visreg for Drought
vis_Amax_W<-visreg(Amax.all, xvar="Treatment", by="Site", cond=list(Treatment="W")) #set up visreg for Wet
Res_Amax_D<-vis_Amax_D$res ; Res_Amax_W<-vis_Amax_W$res # Extract residuals
Res_Amax_all<-rbind(Res_Amax_D, Res_Amax_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("S11"="South3", "S07"="South2", "S02"="South1", "S36"="North3","S15"="North1", "S16" = "North2")
Res_Amax_all_plot<-ggplot(Res_Amax_all, aes(Treatment, y=visregRes, colour=Treatment))+
  geom_boxplot(aes(colour=Treatment), size=0.2)+
  #  geom_text()+
  geom_smooth(method="lm",aes(colour=PrePeak,fill=PrePeak))+
  facet_wrap(.~Site, labeller = labeller(Site=Site_Labs))+
  scale_x_discrete(name= "Treatment") +
  scale_y_continuous(name="Max Photosynthetic Rate")+
  theme_classic()
Res_Amax_all_plot <- Res_Amax_all_plot + theme(legend.position = "none",
                                               axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                               axis.text.y = element_text(size=12,face="bold"),
                                               axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                               axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_Amax_all_plot + facet_wrap(.~Site, labeller = labeller(Site=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))





