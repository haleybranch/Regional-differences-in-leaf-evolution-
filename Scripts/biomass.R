####### Biomass ################
library(lmerTest)
library(lme4)
library(car)
library(emmeans)
library(visreg)
library(ggplot2)
library(tidyverse)
library(multtest)
library(effectsize)

all<-read.csv("biomass.csv")
all$Region <- all$Site
all$Region <- gsub('S02', 'South', all$Region)
all$Region <- gsub('S07', 'South', all$Region)
all$Region <- gsub('S11', 'South', all$Region)

all$Region <- gsub('S15', 'North', all$Region)
all$Region <- gsub('S16', 'North', all$Region)
all$Region <- gsub('S36', 'North', all$Region)

# remove ID 68 and 130, because the R:S ratio was skewed with these two outliers, very small plants
all<-all[!(all$Random_Id=="68" | all$Random_Id=="130"),]


above <- all %>% select(above_dw, Treatment, PrePeak, Year, Site, Region,Timeline)

mod1 <- lmer(above_dw ~ Treatment*PrePeak*Region + (1|Site/Timeline), data=above)

#check for model violations
plot(mod1, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod1,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod1) # qqplot

hist(resid(mod1)) #histogram

plot(mod1, rstudent(.) ~ hatvalues(.)) #residuals vs leverage


#grab the coefficents 
dm <- summary(mod1)
stom_ad_aov <- anova(mod1)  #Treatment < 0.001
#write.csv(stom_ad_aov, "Results/shoot_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/shoot_effectsize.csv")

#determine which of the contrasts are driving the differences

emm1 = emmeans(mod1, specs = ~ Treatment*Region*PrePeak)
emm1

# select what row corresponds to which group 
DNPe = c(1, 0, 0, 0, 0, 0, 0, 0)
WNPe = c(0, 1, 0, 0, 0, 0, 0, 0)
DSPe = c(0, 0, 1, 0, 0, 0, 0, 0)
WSPe = c(0, 0, 0, 1, 0, 0, 0, 0)
DNPr = c(0, 0, 0, 0, 1, 0, 0, 0)
WNPr = c(0, 0, 0, 0, 0, 1, 0, 0)
DSPr = c(0, 0, 0, 0, 0, 0, 1, 0)
WSPr = c(0, 0, 0, 0, 0, 0, 0, 1)

contrasts <- list("DNPe-WNPe" = DNPe - WNPe, "DSPe-WSPe" = DSPe - WSPe,
                  "DNPr-WNPr" = DNPr - WNPr, "DSPr-WSPr" = DSPr - WSPr,
                  "DNPe-DNPr" = DNPe - DNPr, "WNPe-WNPr" = WNPe - WNPr,
                  "DSPe-DSPr" = DSPe - DSPr, "WSPe-WSPr" = WSPe - WSPr,
                  "DNPe-DSPe" = DNPe - DSPe, "WNPe-WSPe" = WNPe - WSPe,
                  "DNPr-DSPr" = DNPr - DSPr, "WNPr-WSPr" = WNPr - WSPr)


#grab the raw pvalues without a posthoc adjustment
contrast.stomad <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.stomad)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
#write.csv(adjust.p.df, "Results/shoot_correctedp.csv")


vis_stom_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_stom_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 
Res_stom_D<-vis_stom_dry$res ; Res_stom_W<-vis_stom_wet$res # Extract residuals
Res_stom_all<-rbind(Res_stom_D, Res_stom_W) #Row bind wet and dry residuals into one data frame

level_order <- factor(Res_stom_all$PrePeak, level = c('pre', 'peak'))
Res_stomad_plot <-ggplot(Res_stom_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_violin(aes(fill=Treatment),trim=FALSE, position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment), fun=mean, colour="black", position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment),fun.data=mean_se, geom="errorbar", colour="black", position = position_dodge(width = 1), width=0.5)+
  facet_wrap(.~Region)+
  scale_y_continuous(name=expression(paste("Shoot Biomass (g)")))+
  scale_x_discrete(name= "Year in Relation to Drought")+
  scale_color_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  scale_fill_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  theme_classic() 
Res_stomad_plot <-Res_stomad_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_stomad_plot <- Res_stomad_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_stomad_plot


### roots ###
roots <- all %>% select(Root.Dry.Weight, Treatment, PrePeak, Year, Site, Region,Timeline)


mod2 <- lmer(Root.Dry.Weight ~ Treatment*Region*PrePeak + (1|Site/Timeline), data=roots)

#check for model violations
plot(mod2, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod2,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod2) # qqplot

hist(resid(mod2)) #histogram

plot(mod2, rstudent(.) ~ hatvalues(.)) #residuals vs leverage


#grab the coefficents 
dm <- summary(mod2)
stom_above_aov <- anova(mod2)  #Treatment = 0.05, Reg*Year = 0.07
#write.csv(stom_above_aov, "Results/root_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod2)
#write.csv(effsize, "Results/Root_effectsize.csv")

#determine which of the contrasts are driving the differences

emm1 = emmeans(mod2, specs = ~ Treatment*Region*PrePeak)
emm1


DNPe = c(1, 0, 0, 0, 0, 0, 0, 0)
WNPe = c(0, 1, 0, 0, 0, 0, 0, 0)
DSPe = c(0, 0, 1, 0, 0, 0, 0, 0)
WSPe = c(0, 0, 0, 1, 0, 0, 0, 0)
DNPr = c(0, 0, 0, 0, 1, 0, 0, 0)
WNPr = c(0, 0, 0, 0, 0, 1, 0, 0)
DSPr = c(0, 0, 0, 0, 0, 0, 1, 0)
WSPr = c(0, 0, 0, 0, 0, 0, 0, 1)

contrasts <- list("DNPe-WNPe" = DNPe - WNPe, "DSPe-WSPe" = DSPe - WSPe,
                  "DNPr-WNPr" = DNPr - WNPr, "DSPr-WSPr" = DSPr - WSPr,
                  "DNPe-DNPr" = DNPe - DNPr, "WNPe-WNPr" = WNPe - WNPr,
                  "DSPe-DSPr" = DSPe - DSPr, "WSPe-WSPr" = WSPe - WSPr,
                  "DNPe-DSPe" = DNPe - DSPe, "WNPe-WSPe" = WNPe - WSPe,
                  "DNPr-DSPr" = DNPr - DSPr, "WNPr-WSPr" = WNPr - WSPr)


#grab the raw pvalues without a posthoc adjustment
contrast.stomad <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.stomad)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
#write.csv(adjust.p.df, "Results/Root_correctedp.csv")


vis_stom_wet<-visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_stom_dry <- visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 
Res_stom_D<-vis_stom_dry$res ; Res_stom_W<-vis_stom_wet$res # Extract residuals
Res_stom_all<-rbind(Res_stom_D, Res_stom_W) #Row bind wet and dry residuals into one data frame


level_order <- factor(Res_stom_all$PrePeak, level = c('pre', 'peak'))

Res_stom_mod3_plot <-ggplot(Res_stom_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_violin(aes(fill=Treatment), trim=FALSE, position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment), fun=mean, colour="black", position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment),fun.data=mean_se, geom="errorbar", colour="black", width=0.5, position = position_dodge(width = 1))+
  facet_wrap(.~Region)+
  scale_y_continuous(name=expression(paste("Root Biomass (g)")))+
  scale_x_discrete(name= "Year in Relation to Drought")+
  scale_color_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  scale_fill_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  theme_classic()
Res_stom_mod3_plot <-Res_stom_mod3_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_stom_mod3_plot <- Res_stom_mod3_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_stom_mod3_plot



############# ab vs ad ###################################
all$rs <- all$Root.Dry.Weight/all$above_dw

all <- all %>% filter(!is.na(all$rs))


rs_all <- subset(all, rs != Inf)



mod3 <- lmer(rs ~ Treatment*Region*PrePeak + (1|Site/Timeline), data=rs_all)

#check for model violations
plot(mod3, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod3,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod3) # qqplot

hist(resid(mod3)) #histogram

plot(mod2, rstudent(.) ~ hatvalues(.)) #residuals vs leverage


#grab the coefficents 
dm <- summary(mod3)
stom_above_aov <- anova(mod3)  #Nothing
#write.csv(stom_ad_aov, "Results/root_shoots_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod3)
#write.csv(effsize, "Results/root_shoots_effsize.csv")

emm1 = emmeans(mod3, specs = ~ Treatment*Region*PrePeak)
emm1


DNPe = c(1, 0, 0, 0, 0, 0, 0, 0)
WNPe = c(0, 1, 0, 0, 0, 0, 0, 0)
DSPe = c(0, 0, 1, 0, 0, 0, 0, 0)
WSPe = c(0, 0, 0, 1, 0, 0, 0, 0)
DNPr = c(0, 0, 0, 0, 1, 0, 0, 0)
WNPr = c(0, 0, 0, 0, 0, 1, 0, 0)
DSPr = c(0, 0, 0, 0, 0, 0, 1, 0)
WSPr = c(0, 0, 0, 0, 0, 0, 0, 1)

contrasts <- list("DNPe-WNPe" = DNPe - WNPe, "DSPe-WSPe" = DSPe - WSPe,
                  "DNPr-WNPr" = DNPr - WNPr, "DSPr-WSPr" = DSPr - WSPr,
                  "DNPe-DNPr" = DNPe - DNPr, "WNPe-WNPr" = WNPe - WNPr,
                  "DSPe-DSPr" = DSPe - DSPr, "WSPe-WSPr" = WSPe - WSPr,
                  "DNPe-DSPe" = DNPe - DSPe, "WNPe-WSPe" = WNPe - WSPe,
                  "DNPr-DSPr" = DNPr - DSPr, "WNPr-WSPr" = WNPr - WSPr)


#grab the raw pvalues without a posthoc adjustment
contrast.stomad <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.stomad)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
#write.csv(adjust.p.df, "Results/roots_shoots_correctedp.csv")


vis_stom_wet<-visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_stom_dry <- visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 
Res_stom_D<-vis_stom_dry$res ; Res_stom_W<-vis_stom_wet$res # Extract residuals
Res_stom_all<-rbind(Res_stom_D, Res_stom_W) #Row bind wet and dry residuals into one data frame


level_order <- factor(Res_stom_all$PrePeak, level = c('pre', 'peak'))

Res_stom_mod3_plot <-ggplot(Res_stom_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_violin(aes(fill=Treatment), trim=FALSE, position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment), fun=mean, colour="black", position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment),fun.data=mean_se, geom="errorbar", colour="black", width=0.5, position = position_dodge(width = 1))+
  facet_wrap(.~Region)+
  scale_y_continuous(name=expression(paste("Root:Shoot (g)")))+
  scale_x_discrete(name= "Year in Relation to Drought")+
  scale_color_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  scale_fill_manual(values= c("Dry"="#FFA100", "Wet"="skyblue3"))+
  theme_classic()
Res_stom_mod3_plot <-Res_stom_mod3_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_stom_mod3_plot <- Res_stom_mod3_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_stom_mod3_plot


