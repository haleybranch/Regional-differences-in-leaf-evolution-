
##### Amax plasticity models #####
# subset by region 
south.dat <- A_fl %>% filter(Region == "South")
north.dat <- A_fl %>% filter(Region == "North")


#build 3-way interaction mixed effects model
south.mod <- lmer(A ~ Rep + Treatment*PrePeak + (1|Site/Timeline), data=south.dat)

#check for model violations
plot(south.mod, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(south.mod,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(south.mod) # qqplot

hist(resid(south.mod)) #histogram

plot(south.mod, rstudent(.) ~ hatvalues(.)) #residuals vs leverage

#grab the coefficents 
dm <- summary(south.mod)
Amax_aov <- anova(south.mod) # treatment:region 0.006, Region:PrePeak 0.004, 3way = 0.1
#write.csv(Amax_aov, "Results/Amax_south_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(south.mod)
#write.csv(effsize, "Results/Amax_south_effectsize.csv")

#determine which of the contrasts are driving the differences
emm1 = emmeans(south.mod, specs = ~ Treatment*PrePeak)
emm1


DPe = c(1, 0, 0, 0)
WPe = c(0, 1, 0, 0)
DPre = c(0, 0, 1, 0)
WPre = c(0, 0, 0, 1)


contrasts <- list("DPe-WPe" = DPe - WPe, "DPre-WPre" = DPre - WPre,
                  "DPe-DPre" = DPe - DPre, "WPe-WPre" = WPe - WPre)



#grab the raw pvalues without a posthoc adjustment
contrast.amax <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.amax)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
# Marginal significance (p=0.1) for WSpe-WSPr, DNPe-WNPe, DSPe-WSpe, WNPe-WNPr)
#write.csv(adjust.p.df, "Results/amax_south_correctedp.csv")

#South has reduced Amax in wet treatments following drought 

########## North #############3

#build 3-way interaction mixed effects model
north.mod <- lmer(A ~ Rep + Treatment*PrePeak + (1|Site/Timeline), data=north.dat)

#check for model violations
plot(north.mod, type=c("p","smooth"), col.line=1) #fitted vs residuals

plot(north.mod,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) #scale-location plot

lattice::qqmath(north.mod) # qqplot

hist(resid(south.mod)) #histogram

plot(north.mod, rstudent(.) ~ hatvalues(.)) #residuals vs leverage

#grab the coefficents 
dm <- summary(north.mod)
Amax_aov <- anova(north.mod) # treatment:region 0.006, Region:PrePeak 0.004, 3way = 0.1
#write.csv(Amax_aov, "Results/Amax_north_aov.csv")

#grab effect sizes of the fixed factors
#es = difference between groups/standard deviation of the variance 
effsize <- omega_squared(north.mod)
#write.csv(effsize, "Results/Amax_north_effectsize.csv")

#determine which of the contrasts are driving the differences
emm1 = emmeans(north.mod, specs = ~ Treatment*PrePeak)
emm1


DPe = c(1, 0, 0, 0)
WPe = c(0, 1, 0, 0)
DPre = c(0, 0, 1, 0)
WPre = c(0, 0, 0, 1)


contrasts <- list("DPe-WPe" = DPe - WPe, "DPre-WPre" = DPre - WPre,
                  "DPe-DPre" = DPe - DPre, "WPe-WPre" = WPe - WPre)



#grab the raw pvalues without a posthoc adjustment
contrast.amax <- contrast(emm1, method = contrasts, adjust="none") 
summ.contrast <- summary(contrast.amax)

#use a pvalue correction for the multiple tests 
adjust.p <- mt.rawp2adjp(summ.contrast$p.value, proc=c("BH"), alpha = 0.05, na.rm = FALSE)

#realign the adjusted pvalues with the contrast names
adjust.p.df<- as.data.frame(adjust.p$adjp) %>%
  rename(p.value=rawp)%>%
  inner_join(summ.contrast)
#Results:
# Marginal significance (p=0.1) for WSpe-WSPr, DNPe-WNPe, DSPe-WSpe, WNPe-WNPr)
#write.csv(adjust.p.df, "Results/amax_north_correctedp.csv")

# plot 

vis_A<-visreg(north.mod, xvar="PrePeak", by="Treatment")
Res_A_all<-vis_A$res

level_order <- factor(Res_A_all$PrePeak, level = c('Pre', 'Peak'))

Res_A_all_plot <-ggplot(Res_A_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_violin(aes(fill=Treatment),trim=FALSE, position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment), fun=mean, colour="black", position = position_dodge(width = 1))+
  stat_summary(aes(group=Treatment, colour=Treatment),fun.data=mean_se, geom="errorbar", colour="black", width=0.5, position = position_dodge(width = 1))+
  scale_y_continuous(name=expression(paste("Max Assimilation (", mu, molm^-2, s^-1, ")")), limits = c(10,40))+
  scale_x_discrete(name= "")+
  scale_color_manual(values= c("dry"="#FFA100", "wet"="skyblue3"))+
  scale_fill_manual(values= c("dry"="#FFA100", "wet"="skyblue3"))+
  theme_classic() +
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")


