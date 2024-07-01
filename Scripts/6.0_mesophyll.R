####### Mesophyll cell proportions ########
library(tidyverse)
library(lme4)
library(emmeans)
library(multtest)
library(visreg)
library(ggplot2)
library(effectsize)

# read in data
meso <- read_csv("Data/Mesophyll_proportions_all.csv")
meso$Average_prop <- as.numeric(meso$Average_prop)

#set up type 3 ANOVA
type3 <- list(Treatment = contr.sum, Region = contr.sum, PrePeak = contr.sum)

#make model
mod1 <- lmer(Average_prop ~ Rep + Treatment*PrePeak*Region + (1|Site/ID), contrasts =type3, data=meso)

#check for model violations
plot(mod1, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(mod1,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(mod1) # qqplot

hist(resid(mod1)) #histogram

plot(mod1, rstudent(.) ~ hatvalues(.)) #residuals vs leverage


#grab the coefficients and run ANOVA
dm <- summary(mod1)
meso_aov <- Anova(mod1,type=3) 
#write.csv(meso_aov, "Results/meso_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/meso_effectsize.csv")

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
contrast.meso <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.meso)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#write.csv(adjust.p.df, "Results/meso_correctedp.csv")


## graph

vis_meso_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_meso_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_meso_all <- rbind(vis_meso_wet$res,vis_meso_dry$res)

meso_res_se <- Res_meso_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(meso_res_se$Treatment, level = c('Wet', 'Dry'))

Res_meso_all_plot <-ggplot(meso_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Palisade Proportion (%)")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_meso_all_plot <-Res_meso_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_meso_all_plot <- Res_meso_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_meso_all_plot
#save 8x6
