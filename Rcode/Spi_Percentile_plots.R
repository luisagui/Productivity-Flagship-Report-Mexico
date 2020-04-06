# Script to plot Betas and clustered standard error for each Spillover variable for each percentile.

require(dplyr)
require(tidyr)
require(ggplot2)

Betas_vaw<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Betas_se_quant_spi_reg_Vaw.csv")

Betas_vaw %>% filter(I.Var!="h001a") %>% mutate(up=1.96*cse,down=-1.96*cse) %>%
  gather(key,value,-c(D.Var,I.Var,Percentile)) %>%
  ggplot(aes(factor(Percentile),value,color=key))+
  geom_point()+
  facet_wrap(.~I.Var,scales="free")+labs(x="Percentile",color="")+theme(legend.position="bottom")

Betas_TFP<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Betas_se_quant_spi_reg_TFP.csv")

Betas_TFP %>% filter(I.Var!="h001a") %>% mutate(up=1.96*cse,down=-1.96*cse) %>%
  gather(key,value,-c(D.Var,I.Var,Percentile)) %>%
  ggplot(aes(factor(Percentile),value,color=key))+
  geom_point()+facet_wrap(.~I.Var,scales="free")+labs(x="Percentile",color="")+theme(legend.position="bottom")
