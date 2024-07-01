########## stomatal conductance ###########################################
#load packages
library(tidyverse)
library(lmerTest)
library(lme4)
library(car)
library(emmeans)
library(multtest)
library(visreg)
library(ggplot2)

#load in data for each ACi replicate
#Rep 1
#rep1 <- data.frame(read.csv("Data/Rep_1_Aci.csv"))

#Rep 2
#rep2 <- data.frame(read.csv("Data/Rep_2_Aci.csv"))

#Rep 3
#rep3 <- data.frame(read.csv("Data/Rep_3_Aci.csv"))

############# need to grab gsw for 50ppm, Ci.transition, and 400ppm  ##########
############################# 50ppm ##############################
#rep1
#rep1_50_gsw <- rep1 %>% filter(Ca < 60)
#write.csv(rep1_ci_gsw, file="Data/rep1_ci_gsw.csv")
#rep1_50_gsw <- data.frame(read.csv("Data/rep1_ca_gsw.csv"))
#rep1_50_gsw$X.1 <- NULL

#rep2
#rep2_50_gsw <- rep2 %>% filter(Ca < 60)
#rep3
#rep3_50_gsw <- rep3 %>% filter(Ca < 60)

#gsw_50 <- rbind(rep1_50_gsw, rep2_50_gsw, rep3_50_gsw)
#write.csv(gsw_50, file="Data/gsw_50.csv")
gsw_50 <- data.frame(read.csv("Data/gsw_50.csv"))

#make new column with  PrePeak
pre <- filter(gsw_50, Year=="2010" | Year=="2011") 
#Add pre column
pre[,14] <- "Pre"
colnames(pre)[14]<-"PrePeak"

peak <- filter(gsw_50, Year=="2014" | Year=="2015" | Year=="2016") 
#Add pre column
peak[,14] <- "Peak"
colnames(peak)[14]<-"PrePeak"

#gsw_50 <- rbind(pre,peak)
#write.csv(gsw_50, file="Data/gsw_50.csv")
gsw_50 <- data.frame(read.csv("Data/gsw_50.csv"))

mod1 <- lmer(gsw ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrasts = type3, data=gsw_50)

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
gsw_aov <- Anova(mod1, type = 3) 
#write.csv(gsw_aov, "Results/gsw_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/gsw_effectsize.csv")

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
contrast.gsw <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.gsw)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/gsw_correctedp.csv")

## point plot
vis_gs_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))
vis_gs_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 
Res_gs_all<- rbind(vis_gs_wet$res,vis_gs_dry$res)


gs_res_se <- Res_gs_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(gs_res_se$Treatment, level = c('wet', 'dry'))

Res_gs_all_plot <-ggplot(gs_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("gs (50 mol", m^-2, s^-1, ")")))+
  scale_color_manual(breaks=c("Pre","Peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_gs_all_plot <-Res_gs_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_gs_all_plot <- Res_gs_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_gs_all_plot


#### gsw at 400ppm

gsw_400 <- data.frame(read.csv("Data/gsw_400.csv"))


#build 3-way interaction mixed effects model
mod3 <- lmer(gsw ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrasts = type3, data=gsw_400)

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
Amax_aov <- Anova(mod3, type =3) 
#write.csv(Amax_aov, "Results/gsw400_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod3)
#write.csv(effsize, "Results/gsw400_effectsize.csv")

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
contrast.amax <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.amax)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/gsw400_correctedp.csv")


vis_gs400_wet<-visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))
vis_gs400_dry <- visreg(mod3, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 
Res_gs400_all<- rbind(vis_gs400_wet$res,vis_gs400_dry$res)


gs_res400_se <- Res_gs400_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(gs_res400_se$Treatment, level = c('wet', 'dry'))

Res_gs400_all_plot <-ggplot(gs_res400_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("gs (400 mol", m^-2, s^-1, ")")))+
  scale_color_manual(breaks=c("Pre","Peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "Treatment")+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_gs400_all_plot <-Res_gs400_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_gs400_all_plot <- Res_gs400_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_gs400_all_plot


