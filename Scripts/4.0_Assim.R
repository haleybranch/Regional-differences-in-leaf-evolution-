## Assimilation ####
#load libraries
library(lmerTest)
library(lme4)
library(car)
library(emmeans)
library(visreg)
library(ggplot2)
library(tidyverse)
library(multtest)
library(effectsize)

#read in data
A_fl <- read_csv("Data/Chl_fl_all.csv")
A_fl$NPQ <- NULL
A_fl$Rep <- as.factor(A_fl$Rep)

#build 3-way interaction mixed effects model
type3 <- list(Treatment = contr.sum, Region = contr.sum, PrePeak = contr.sum)
mod1 <- lmer(A ~ Rep + Treatment*Region*PrePeak + (1|Site/Timeline), data=A_fl, contrasts = type3)

#check for model violations
plot(mod1, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod1,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod1) # qqplot

hist(resid(mod1)) #histogram

plot(mod1, rstudent(.) ~ hatvalues(.)) #residuals vs leverage

#grab the coefficents and run ANOVA
dm <- summary(mod1)
Amax_aov <- Anova(mod1, type = 3) 
#write.csv(Amax_aov, "Results/Amax_aov_revised.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/Amax_effectsize.csv")

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
contrast.amax <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.amax)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/amax_correctedp.csv")


### make a point plot

vis_A_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))
vis_A_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 

Res_A_all <- rbind(vis_A_wet$res,vis_A_dry$res)


A_res_se <- Res_A_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(A_res_se$Treatment, level = c('wet', 'dry'))

Res_A_all_plot_1 <- ggplot(A_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Max Assimilation (", mu, molm^-2, s^-1, ")")))+
  scale_color_manual(breaks=c("Pre","Peak"),values=c("navyblue", "magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_A_all_plot_1 <-Res_A_all_plot_1 + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_A_all_plot_1 <- Res_A_all_plot_1 + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_A_all_plot_1



#### Net assimilation at 400ppm ##########

#read in data
all <- read_csv("Data/gsw_400.csv")
type3 <- list(Treatment = contr.sum, Region = contr.sum, PrePeak = contr.sum)

#build 3-way interaction mixed effects model
mod2 <- lmer(A ~ Rep + Treatment*Region*PrePeak + (1|Site/ID), contrasts = type3, data=all)

#check for model violations
plot(mod2, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod2,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod2) # qqplot
#residuals have light tails 

hist(resid(mod2)) #histogram

plot(mod2, rstudent(.) ~ hatvalues(.)) #residuals vs leverage

#grab the coefficents and run ANOVA
dm <- summary(mod2)
Amax_aov <- Anova(mod2) 
#write.csv(Amax_aov, "Results/Anet_aov_revised.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod2)
#write.csv(effsize, "Results/Anet_effectsize.csv")

#determine which of the contrasts are driving the differences
emm2 = emmeans(mod2, specs = ~ Treatment*Region*PrePeak)
emm2


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
contrast.amax <- contrast(emm2, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.amax)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/anet_correctedp.csv")


#point plot 

vis_A_wet<-visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))
vis_A_dry <- visreg(mod2, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 

Res_A_all <- rbind(vis_A_wet$res,vis_A_dry$res)


A_res_se <- Res_A_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(A_res_se$Treatment, level = c('wet', 'dry'))

Res_A_all_plot_2 <- ggplot(A_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Net Assimilation (400", mu, molm^-2, s^-1, ")")))+
  scale_color_manual(breaks=c("Pre","Peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name = "Treatment")+
  theme_bw()+
  theme_classic()+
  theme(legend.position = "none")
Res_A_all_plot_2 <-Res_A_all_plot_2 + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_A_all_plot_2 <- Res_A_all_plot_2 + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_A_all_plot_2
