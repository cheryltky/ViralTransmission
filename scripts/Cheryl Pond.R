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




summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


summarySE(newdat, measurevar = "logGAV", groupvars="Family.ID", na.rm = TRUE)
