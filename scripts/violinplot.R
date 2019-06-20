ggplot(dat, aes(Cohort, Ct.Mean))+
  geom_boxplot()+
  geom_violin()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  theme_classic()