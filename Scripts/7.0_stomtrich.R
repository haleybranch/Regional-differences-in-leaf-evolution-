####### Stomata and Trichomes ################
library(tidyverse)
library(lmerTest)
library(lme4)
library(car)
library(emmeans)
library(visreg)
library(ggplot2)
library(effectsize)
library(multtest)

all<-read_csv("Data/all_stom_trich.csv")
all<- subset(all, Rep!="3")
all$Trich_mm <- as.numeric(all$Trich_mm)
all$Stom_mm <- as.numeric(all$Stom_mm)
below <- subset(all, Placement == "be")
above <- subset(all, Placement == "ab")

#set up type 3 ANOVA
type3 <- list(Treatment = contr.sum, Region = contr.sum, PrePeak = contr.sum)

### below leaf stomatal density ########
# make model
mod1 <- lmer(Stom_mm ~ Rep + Treatment*PrePeak*Region + (1|Site/ID),contrasts=type3, data=below)

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
stom_ad_aov <- Anova(mod1,type=3)  
#write.csv(stom_ad_aov, "Results/stom_below_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/stom_ad_effectsize.csv")

#determine which of the contrasts are driving the differences

emm1 = emmeans(mod1, specs = ~ Treatment*Region*PrePeak)
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
#write.csv(adjust.p.df, "Results/stom_ad_correctedp.csv")


####### above leaf stomatal density ########

mod2 <- lmer(Stom_mm ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrasts = type3, data=above)

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
stom_above_aov <- Anova(mod2, type=3)  
#write.csv(stom_ad_aov, "Results/stom_above_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod2)
#write.csv(effsize, "Results/stom_above_effectsize.csv")

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
#write.csv(adjust.p.df, "Results/stom_above_correctedp.csv")

####### graphs ##############
######## Lower stomatal density plot
vis_stbe_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_stbe_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_stbe_all <- rbind(vis_stbe_wet$res,vis_stbe_dry$res)


stbe_res_se <- Res_stbe_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(stbe_res_se$Treatment, level = c('Wet', 'Dry'))

Res_stbe_all_plot <-ggplot(stbe_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Lower Stomatal Density (", mm^2, ")")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_stbe_all_plot <-Res_stbe_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_stbe_all_plot <- Res_stbe_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_stbe_all_plot
# save 8x6

############# Upper stomatal density plot 
vis_stab_wet<-visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_stab_dry <- visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_stab_all <- rbind(vis_stab_wet$res,vis_stab_dry$res)


stab_res_se <- Res_stab_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(stab_res_se$Treatment, level = c('Wet', 'Dry'))

Res_stab_all_plot <-ggplot(stab_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Upper Stomatal Density (", mm^2, ")")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_stab_all_plot <-Res_stab_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_stab_all_plot <- Res_stab_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_stab_all_plot
# save 8x6




############### trichomes ########################

#### below leaf trichome density ############

mod1 <- lmer(Trich_mm ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrasts = type3, data=below)


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
trichbe_aov <- Anova(mod1, type=3) 
#write.csv(trichbe_aov, "Results/trich_below_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/trich_below_effectsize.csv")

#determine which of the contrasts are driving the differences

emm1 = emmeans(mod1, specs = ~ Treatment*Region*PrePeak)
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
contrast.trich <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.trich)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/trich_below_correctedp.csv")


### above leaf trichome density ###
mod3 <- lmer(Trich_mm ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrast = type3, data=above)


#check for model violations
plot(mod3, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod3,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod3) # qqplot

hist(resid(mod3)) #histogram

plot(mod3, rstudent(.) ~ hatvalues(.)) #residuals vs leverage

#grab the coefficents 
dm <- summary(mod3)
trich_above_aov <- Anova(mod3, type=3) 
#write.csv(trich_above_aov, "Results/trich_above_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod3)
#write.csv(effsize, "Results/trich_above_effectsize.csv")

#determine which of the contrasts are driving the differences

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
contrast.trichab <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.trichab)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/trich_above_correctedp.csv")

######### graphs #######################
######### Lower trichome density plot
vis_trbe_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_trbe_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_trbe_all <- rbind(vis_trbe_wet$res,vis_trbe_dry$res)

trbe_res_se <- Res_trbe_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(trbe_res_se$Treatment, level = c('Wet', 'Dry'))

Res_trbe_all_plot <-ggplot(trbe_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Lower Trichome Density (", mm^2, ")")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_trbe_all_plot <-Res_trbe_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_trbe_all_plot <- Res_trbe_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_trbe_all_plot
# save 8x6

####### Upper trichome density plot 
vis_trab_wet<-visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_trab_dry <- visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_trab_all <- rbind(vis_trab_wet$res,vis_trab_dry$res)


trab_res_se <- Res_trab_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(trab_res_se$Treatment, level = c('Wet', 'Dry'))

Res_trab_all_plot <-ggplot(trab_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Upper Trichome Density (", mm^2, ")")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_trab_all_plot <-Res_trab_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_trab_all_plot <- Res_trab_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_trab_all_plot
# save 8x6


library(cowplot)

plot_grid(Res_thick_all_plot,Res_meso_all_plot,Res_stbe_all_plot,Res_trbe_all_plot, ncol=2)
#save 8x8
