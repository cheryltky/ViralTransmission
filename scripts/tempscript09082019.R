# Load data file
allfams <- read.csv("https://raw.githubusercontent.com/cheryltky/ViralTransmission/f8664a876f8883d16b855116c39a0e845345fa9b/Working%20Files/allcohorts_1plate_class.csv")
head(allfams)
summary(allfams)
str(allfams)

#Make IntFamID as factor
allfams$IntFamID <- as.factor(allfams$IntFamID)
#Load packages

library(tidyverse)
library(ggplot2)

#ggplot of 89 families
ggplot(data = allfams, aes(x = IntFamID, y = GAV)) +
  geom_point()

#
means <- by(allfams$GAV, allfams$IntFamID, function(x){mean(x, na.rm=T)})
means

maxs <- by(allfams$GAV, allfams$IntFamID, function(x){mean(x, na.rm=T)})
maxs

mins <- by(allfams$GAV, allfams$IntFamID, function(x) {mean(x, na.rm=T)})
mins


