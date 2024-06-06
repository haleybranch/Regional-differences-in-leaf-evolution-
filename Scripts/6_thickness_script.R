#read in leaf thickness data
library(tidyverse)
library(lmerTest)
library(lme4)
library(car)
library(emmeans)
library(multtest)
library(visreg)
library(ggplot2)

#Read in the data
thickness <- read_csv("Data/Mesophyll_proportions_all.csv")
thickness$Average_thick <- as.numeric(thickness$Average_thick)
thickness <- thickness %>% filter(!is.na(Average_thick))


#change treatment, prepeak, and site to characters
thickness$Treatment <- as.character(thickness$Treatment)
thickness$Site <- as.character(thickness$Site)
thickness$PrePeak <- as.character(thickness$PrePeak)
thickness$PlantID <- as.character(thickness$ID)
thickness$Rep <- as.character(thickness$Rep)


#remove S11 and S16, since they were not supposed to be included in the first place
thickness_subset <- subset(thickness,Site != "S11")
thickness_subset2 <- subset(thickness_subset, Site != "S16")
thickness_all <- thickness
thickness <- thickness_subset2   

type3 <- list(Treatment = contr.sum, Region = contr.sum, PrePeak = contr.sum)

mod1 <- lmer(Average_thick ~ Rep + Treatment*PrePeak*Region + (1|Site/ID),contrasts =type3, data=thickness)

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
thick_aov <- Anova(mod1, type = 3) #treatment = 0.004, Trt*Reg = 0.002, Year*Reg = 0.05
#write.csv(thick_aov, "Results/thickness_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/thickness_effectsize.csv")

#determine which of the contrasts are driving the differences
emm1 = emmeans(mod1, specs = ~ Treatment*Region*PrePeak)
emm1

#organize the contrasts of interest
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
contrast.thick <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.thick)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
# South Pre W-D = 0.0007
#write.csv(adjust.p.df, "Results/thickness_correctedp.csv")

vis_thick_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Wet"))
vis_thick_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="Dry")) 

Res_thick_all <- rbind(vis_thick_wet$res,vis_thick_dry$res)


thick_res_se <- Res_thick_all%>%
  group_by(Treatment,Region,PrePeak)%>%
  summarise(m=mean(visregRes),upper=mean_se(visregRes)$ymax,lower=mean_se(visregRes)$ymin)

level_order <- factor(thick_res_se$Treatment, level = c('Wet', 'Dry'))

Res_thick_all_plot <-ggplot(thick_res_se,(aes(x=level_order,y=m,ymax=upper,ymin=lower)))+
  geom_point(aes(color=PrePeak))+geom_errorbar(width=0.25)+
  facet_grid(.~Region)+
  geom_line(aes(group=PrePeak, linetype = PrePeak, color=PrePeak))+
  scale_linetype_manual(values=c("solid", "dotted"))+
  scale_y_continuous(name=expression(paste("Leaf Thickness (", mu,m, ")")))+
  scale_color_manual(breaks=c("pre","peak"),values=c("navyblue","magenta2"))+
  scale_x_discrete(name= "")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
Res_thick_all_plot <-Res_thick_all_plot + theme(
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_thick_all_plot <- Res_thick_all_plot + facet_wrap(.~Region) +
  theme(strip.background = element_blank(), strip.text.x=element_blank())
Res_thick_all_plot
# save 8x6

