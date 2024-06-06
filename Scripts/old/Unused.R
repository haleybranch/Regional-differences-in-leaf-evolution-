
#jmax
library(visreg)
vis_j_wet<-visreg(Jmax1.all, xvar="PrePeak", by="Region", cond=list(Treatment="wet"))

vis_j_dry <- visreg(Jmax1.all, xvar="PrePeak", by="Region", cond=list(Treatment="dry")) 

Res_j_D<-vis_j_dry$res ; Res_j_W<-vis_j_wet$res # Extract residuals
Res_j_all<-rbind(Res_j_D, Res_j_W) #Row bind wet and dry residuals into one data frame


level_order <- factor(Res_j_all$PrePeak, level = c('pre', 'peak'))
Res_j_all_plot <-ggplot(Res_j_all, aes(x=level_order, y=visregRes, colour=Treatment))+
  geom_boxplot(aes(colour=Treatment))+
  facet_wrap(.~Region)+
  scale_y_continuous(name="Max RuBP Regeneration Rate (Jmax)")+
  scale_x_discrete(name= "Year in Relation to Drought")+
  theme_classic()
Res_j_all_plot <-Res_j_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_j_all_plot <- Res_j_all_plot + facet_wrap(.~Region) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
Res_j_all_plot
