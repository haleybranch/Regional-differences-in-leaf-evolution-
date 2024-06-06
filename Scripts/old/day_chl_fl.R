## daytime chl fl
library(lmerTest)
library(lme4)
library(car)
library(see)
library(patchwork)
library(performance)
library(MuMIn)
library(emmeans)


chl_fl <- read.csv("Data/Chl_fl_all.csv")

mod1 <- lmer(Fv.Fm. ~ Treatment*Region*PrePeak + (1|Site) + (1|Timeline) + (1|Rep), data=chl_fl)
check_model(mod1) #violates colinearity   

# drop 3way
mod2 <- lmer(Fv.Fm. ~ Treatment*PrePeak + Treatment*Region +
                      PrePeak*Region + (1|Site) + (1|Timeline), data=chl_fl)

anova(mod2)
emmeans(mod2, list(pairwise ~ Treatment*Region*PrePeak), adjust = "tukey") #Nothing significant

library(visreg)

vis_chl_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))

vis_chl_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 

Res_chl_D<-vis_chl_dry$res ; Res_chl_W<-vis_chl_wet$res # Extract residuals
Res_chl_all<-rbind(Res_chl_D, Res_chl_W) #Row bind wet and dry residuals into one data frame


library(ggplot2)
level_order <- factor(Res_etr_all$PrePeak, level = c('Pre', 'Peak'))
Res_chl_all_plot <-ggplot(Res_chl_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_boxplot(aes(colour=Treatment))+
  facet_wrap(.~Region)+
  scale_y_continuous(name="Leaf Chl Fl (Fv'/Fm')")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_chl_all_plot <-Res_chl_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_chl_all_plot <- Res_chl_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_chl_all_plot

library(emmeans)
emmeans(mod1, pairwise ~ Site)
emmeans(mod1, pairwise ~ Treatment*PrePeak)

#plot without wet and dry 

vis_chl_wet<-visreg(mod1, xvar="PrePeak", by="Region")

vis_chl_dry <- visreg(mod1, xvar="PrePeak", by="Region") 

Res_chl_D<-vis_chl_dry$res ; Res_chl_W<-vis_chl_wet$res # Extract residuals
Res_chl_all<-rbind(Res_chl_D, Res_chl_W) #Row bind wet and dry residuals into one data frame


library(ggplot2)
Res_chl_all_plot <-ggplot(Res_chl_all, aes(PrePeak, y=visregRes))+
  geom_boxplot()+
  facet_wrap(.~Region)+
  scale_y_continuous(name="Leaf chl")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_chl_all_plot <-Res_chl_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_chl_all_plot <- Res_chl_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_chl_all_plot




## plot without prepeak 
vis_chl_wet<-visreg(mod1, xvar="Treatment", by="Region")

vis_chl_dry <- visreg(mod1, xvar="PrePeak", by="Region") 

Res_chl_D<-vis_chl_dry$res ; Res_chl_W<-vis_chl_wet$res # Extract residuals
Res_chl_all<-rbind(Res_chl_D, Res_chl_W) #Row bind wet and dry residuals into one data frame


library(ggplot2)
Res_chl_all_plot <-ggplot(Res_chl_all, aes(Treatment, y=visregRes))+
  geom_boxplot(aes(colour=Treatment))+
  facet_wrap(.~Region)+
  scale_y_continuous(name="Leaf chl")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_chl_all_plot <-Res_chl_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_chl_all_plot <- Res_chl_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_chl_all_plot


# No region
vis_chl_wet<-visreg(mod1, xvar="PrePeak", by="Treatment")

vis_chl_dry <- visreg(mod1, xvar="PrePeak", by="Treatment") 

Res_chl_D<-vis_chl_dry$res ; Res_chl_W<-vis_chl_wet$res # Extract residuals
Res_chl_all<-rbind(Res_chl_D, Res_chl_W) #Row bind wet and dry residuals into one data frame


library(ggplot2)
Res_chl_all_plot <-ggplot(Res_chl_all, aes(PrePeak, y=visregRes))+
  geom_boxplot(aes(colour=Treatment))+
  facet_wrap(.~PrePeak)+
  scale_y_continuous(name="Leaf chl")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_chl_all_plot <-Res_chl_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_chl_all_plot <- Res_chl_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_chl_all_plot

###### ETR


ETR.all = lmer(ETR_computed~ Treatment*Region*PrePeak + (1|Site) + (1|Timeline) + (1|Rep), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), chl_fl)

# drop 3way
no3way.mod1 <- lmer(ETR_computed ~ Treatment*PrePeak + Treatment*Region +
                      PrePeak*Region + (1|Site) + (1|Timeline) + (1|Rep), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=chl_fl)

lrtest(ETR.all, no3way.mod1) # accept full model

anova(ETR.all)# Region*prepeak = 0.004
summary(ETR.all)

library(visreg)
vis_ETR_D<-visreg(ETR.all, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) #set up visreg for Drought
vis_ETR_W<-visreg(ETR.all, xvar="PrePeak", by="Region", cond=list(Treatment="wet")) #set up visreg for Wet
Res_ETR_D<-vis_ETR_D$res ; Res_ETR_W<-vis_ETR_W$res # Extract residuals
Res_ETR_all<-rbind(Res_ETR_D, Res_ETR_W) #Row bind wet and dry residuals into one data frame

Res_ETR_all_plot<-ggplot(Res_ETR_all, aes(PrePeak, y=visregRes))+
  geom_boxplot()+
  facet_wrap(.~PrePeak)+
  scale_y_continuous(name="ETR")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_ETR_all_plot <-Res_ETR_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_ETR_all_plot <- Res_ETR_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_ETR_all_plot


