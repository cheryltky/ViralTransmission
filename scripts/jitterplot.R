setwd("~/gav pond data")
dat = read.csv("cherylponddata.csv")



head(dat)
names(dat)
str(dat)

#make family.ID factor
dat$Family.ID <-as.factor(dat$Family.ID)

summary(dat)

hist(dat$GAV)

#log transform 
dat$logGAV <- 1+log10(dat$GAV)
hist(dat$logGAV)

library(ggplot2)

##Boxplots
ggplot(dat, aes(Family.ID, logGAV))+
  geom_boxplot()+
  theme_classic()

ggplot(dat, aes(Family.ID, logGAV))+
  geom_boxplot()+
  geom_jitter(aes(colour = Class, shape= Class))+
  theme_classic()

ggplot(dat, aes(Family.ID, logGAV))+
  geom_jitter(position=position_jitter(width = 0.1, height= 0),
              aes(colour = Class, shape= Class))+
  theme_classic()
#to calculate mean of each family
newdat<-subset(dat,Class=="Offspring")