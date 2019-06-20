# set working directory
setwd("~/Documents/PROJECTS/COLLABORATORS/cheryl")
# read data in (you have numerous missing value codes make sure all are included in the list when reading data in)
d<-read.csv("Cohort Summary Stats For Analyses.csv", stringsAsFactors=F, na.strings=c("NA", " "))

# makes sure the values are numeric and not strings
d$OffspringlgGAV<-as.numeric(d$OffspringlgGAV)
d$FatherLog10GAV <-as.numeric(d$FatherLog10GAV)
d$MotherlgGAV <-as.numeric(d$MotherlgGAV)

# 
means<-by(d$OffspringlgGAV, d$FamilyID, function(x){mean(x, na.rm=T)})
maxs<-by(d$OffspringlgGAV, d$FamilyID, function(x){max(x, na.rm=T)})
mins<-by(d$OffspringlgGAV, d$FamilyID, function(x){min(x, na.rm=T)})

res<-cbind(means,maxs,mins)
# because some families have all values missing
# we get NaNs and Inf values, replace with missing
res[is.nan(res[,"means"]), "means"]<-NA
res[is.infinite(res[,"maxs"]), "maxs"]<-NA
res[is.infinite(res[,"mins"]), "mins"]<-NA

write.table(res, "descriptives_by_family.csv", sep=",")

# ========================
# you can also do exactly the same thing using dplyr package
# it is popular with R community and useful to know 
library(dplyr)

d %>%
  group_by(FamilyID) %>%
  summarise_at(vars(OffspringlgGAV), funs(mean(., na.rm=TRUE)))
 
 
 # =================================
 # first remove missing values 
 
# identify values that are missing
# by using '!' we select those that are NOT missing
tmp=!is.na(d$OffspringlgGAV)
d1=d[tmp,]

dim(d1)  # 1156
# check if we removed all the missing values
length(na.omit(d1$OffspringlgGAV))
  
 # recode family ID to make it easier to read.
 fams <- sort(unique(d1$FamilyID))  
 length(fams)     # we have 85 families left

# change family ID. Make it simple integer
 d1$IntFamID<-NA
 for(i in 1:length(fams)){
 	tmp=which(d1$FamilyID == fams[i])
 	d1$IntFamID[tmp] = paste("FAM", i, sep="_")
 }
 
 
# make offspring count for each family
off.cnt <- table(d1$IntFamID)
# we calculate how many offsprings there are per family
# count gives you total number of individuals in the dataset
# so the count has to be divided by the total number to know how many families have that number of offspring
 d1$offspring.cnt<-NA
 for(i in 1:length(off.cnt)){
 	tmp=which(d1$IntFamID == names(off.cnt)[i])
 	d1$offspring.cnt[tmp] = off.cnt[i]
 }

 
 
 # reshape data from wide to long (collapse across offspring, father, mother)
 # first reduce number of columns for easier work
 library(tidyr)
 d2 <- d1[,c("Cohort", "IntFamID", "FamilyID", "Offspring.ID", "FatherID", "MotherID","OffspringlgGAV", "FatherLog10GAV", "MotherlgGAV")] 
 names(d2)[4] <- "OffspringID"
 names(d2)[7:9] <- c("Offspring", "Father", "Mother")
 dlong <- gather(d2, class, logGAV, Offspring:Mother)
 
 tmp=!is.na(dlong$logGAV)
 dlong=dlong[tmp,]
 
 # put IDs into one column
 dlong$ID <- ifelse(dlong$class == "Offspring", dlong$OffspringID, ifelse(dlong$class == "Mother", dlong$MotherID, dlong$Father))
 
 # now you do not need separate IDs so can remove
 dlong <- dlong[, c("Cohort", "IntFamID", "FamilyID","ID", "class", "logGAV")]
 
  
  library(ggplot2)
  png("boxplots_by_cohort.png", unit="in", width=18, height=7, res=350)
  ggplot(dlong, aes(IntFamID, logGAV)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(width=0.1, height=0), aes(color=class, shape=class)) +
  theme(axis.text.x = element_text(size = rel(0.4), angle = 90))+
  facet_wrap(~Cohort, nrow=1)  
  dev.off()
  

write.table(dlong, "all_families_data_summary.csv", row.names=F, sep=",")

# ============================================
 # removing families with fewer than 5
 d5 <- d1[which(d1$offspring.cnt >=5), ]
 dim(d5)   # we are left with 1077  individuals 

 # reshape data from wide to long (collapse across offspring, father, mother)
 # first reduce number of columns for easier work
 library(tidyr)
 d5 <- d5[,c("Cohort", "IntFamID", "FamilyID", "Offspring.ID", "FatherID", "MotherID","OffspringlgGAV", "FatherLog10GAV", "MotherlgGAV")] 
 names(d5)[4] <- "OffspringID"
 names(d5)[7:9] <- c("Offspring", "Father", "Mother")
 dlong5 <- gather(d5, class, logGAV, Offspring:Mother)
 
 tmp=!is.na(dlong5$logGAV)
 dlong5=dlong5[tmp,]
 
 # put IDs into one column
 dlong5$ID <- ifelse(dlong5$class == "Offspring", dlong5$OffspringID, ifelse(dlong5$class == "Mother", dlong5$MotherID, dlong5$Father))
 
 # now you do not need separate IDs so can remove
 dlong5 <- dlong5[, c("Cohort", "IntFamID", "FamilyID","ID", "class", "logGAV")]
  
  library(ggplot2)
  png("boxplots_by_cohort_5offspring.png", unit="in", width=18, height=7, res=350)
  ggplot(dlong5, aes(IntFamID, logGAV)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(width=0.1, height=0), aes(color=class, shape=class)) +
  theme(axis.text.x = element_text(size = rel(0.4), angle = 90))+
  facet_wrap(~Cohort, nrow=1)  
  dev.off()
  

write.table(dlong5, "families_5offspring_data_summary.csv", row.names=F, sep=",")
 