################# SLA ##############################
library(lmerTest)
library(lme4)
library(car)
library(see)
library(patchwork)
library(performance)
library(MuMIn)
library(emmeans)

dat <- read.csv("Data/sla_all.csv")

### remove any sa values below 0.01 not enough resolution on the scale
dat <- dat %>% filter(dw > 0.009)
dat <- subset(dat, Site != "S11")

mod1 <- lmer(SLA ~ Treatment*Region*PrePeak + (1|Site/ID) + (1|Rep), data=dat)

anova(mod1)


emmeans(mod1, pairwise~Treatment*Region*PrePeak, adjust = "tukey")


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
stom_ad_aov <- anova(mod1)  #treatment = 0.05, Region = 0.03, Trt*Year = 0.04, Year*Reg = 0.0008
#write.csv(stom_ad_aov, "Results/sla_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(mod1)
#write.csv(effsize, "Results/sla_effectsize.csv")

#determine which of the contrasts are driving the differences

emm1 = emmeans(mod1, specs = ~ Treatment*Region*PrePeak, type= "none")
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
#write.csv(adjust.p.df, "Results/sla_correctedp.csv")


vis_stom_wet<-visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))
vis_stom_dry <- visreg(mod1, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 
Res_stom_D<-vis_stom_dry$res ; Res_stom_W<-vis_stom_wet$res # Extract residuals
Res_stom_all<-rbind(Res_stom_D, Res_stom_W) #Row bind wet and dry residuals into one data frame

level_order <- factor(Res_stom_all$PrePeak, level = c('pre', 'peak'))
Res_stomad_plot <-ggplot(Res_stom_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_violin(aes(fill=Treatment),trim=FALSE, position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment), fun=mean, colour="black", position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment),fun.data=mean_se, geom="errorbar", colour="black", position = position_dodge(width = 1), width=0.5)+
  facet_wrap(.~Region)+
  scale_y_continuous(name=expression(paste("SLA (", cm^2,"/g)")), lim=c(100,700))+
  scale_x_discrete(name= "Year in Relation to Drought")+
  scale_color_manual(values= c("dry"="#FFA100", "wet"="skyblue3"))+
  scale_fill_manual(values= c("dry"="#FFA100", "wet"="skyblue3"))+
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

