# To get estimates of correlation between offspring and fathers within each of the three cohorts. 

cor.est.off.fath<-by(d[,c('OffspringlgGAV', 'FatherLog10GAV')], d$Cohort, function(x){cor.test(x$OffspringlgGAV, x$FatherLog10GAV)$est})


# This command line does the same but saves p-values instead of the estimate. 
#You can than tabulate them to see how strong the correlation is and how significant.


cor.pval.off.fath<-by(d[,c('OffspringlgGAV', 'FatherLog10GAV')], d$Cohort, function(x){cor.test(x$OffspringlgGAV, x$FatherLog10GAV)$p.value})
