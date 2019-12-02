library(tidyverse)
library(ggplot2)

Pond141 <- read.csv("https://raw.githubusercontent.com/cheryltky/ViralTransmission/master/Working%20Files/201602_by_pond_1plate.csv")

head(Pond141)
summary(Pond141)
str(Pond141)

#make cohort, pond , sample name all factors

Pond141 <- Pond141 %>% mutate_if(is.integer, as.factor)
str(Pond141)
summary(Pond141)

#Leaving log10GAV as factor instead of numeric
##scatterplot

Pond141 <- ggplot(Pond141, aes(GAV, PrawnID)) + 
  geom_point()+
  theme_classic()

##Boxplot
ggplot(Pond141, aes(GAV, PrawnID)) +
  geom_jitter(width = 0.2) + 
  labs( title = "Pond 141 GAV Loading")

ggsave("Pond141.png", Pond141, width = 15, height = 10 , dpi = 300)


