######### photo compensation point
####### S2_flowering time and germination 
library(tidyverse)
library(ggplot2)
library(visreg)


dry1 <- read.csv("Data/Li-photocomp_point/154_w.csv")

dry1$Wavelength <- as.character(dry1$Wavelength)

mod1 <- loess(A~Ci, data=dry1[dry1$Wavelength==100,])
mod2 <- loess(A~Ci, data=dry1[dry1$Wavelength==200,])
mod3 <- loess(A~Ci, data=dry1[dry1$Wavelength==500,])

x=seq(40,80)
func=function(x,m1,m2,m3){
  val1=predict(m1,x)
  val2=predict(m2,x)
  val3=predict(m3,x)
  (val1-val2)^2+(val2-val3)^2+(val3-val1)^2
}
optimize(func,range(x),mod1,mod2,mod3)



y=seq(-1,0)
func=function(y,m1,m2,m3){
  val1=predict(m1,y)
  val2=predict(m2,y)
  val3=predict(m3,y)
  (val1-val2)^2+(val2-val3)^2+(val3-val1)^2
}
optimize(func,range(y),mod1,mod2,mod3)

ggplot(dry1, aes(Ci, y=A, colour=Wavelength))+
  geom_smooth(aes(colour=Wavelength))+
  scale_y_continuous(name="A")+
  theme_classic()
