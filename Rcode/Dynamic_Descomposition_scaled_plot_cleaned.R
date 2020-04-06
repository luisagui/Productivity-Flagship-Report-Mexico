# Dynamic_Decomposition graphs / cleaned data

Dynamic_Descomposition<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Dynamic_Descomposition_clean (2).csv")

Descomposition<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_clean (2).csv")
Descomposition %>% select(anio,Vaw)->Des
Des %>% mutate(anio=as.numeric(anio)+5) %>% rename(Vaw_lag=Vaw)->Des_5
Dynamic_Descomposition %>% left_join(Des, by=("anio")) %>% left_join(Des_5,by=("anio"))->Dynamic_Descomposition

# It needs a correction that is explained in the appendix
Dynamic_Descomposition %>% mutate(phi_s=(Vaw.x+Vaw_lag.x)/2,phi=(Vaw.y+Vaw_lag.y)/2,Vaw_Cov_scale=Vaw_Cov/Vaw.x,Vaw_Cov_scale_lag=Vaw_Cov_lag/Vaw_lag.x,delta_Vaw_Cov_s=Vaw_Cov_scale-Vaw_Cov_scale_lag,Cov_s=(Vaw_Cov_scale+Vaw_Cov_scale_lag)/2)->Dynamic_Descomposition
Dynamic_Descomposition %>% mutate(Ent=w_firm_Ent*(Vaw_Ent-Vaw.x)/phi,Exit=w_firm_Exit*(Vaw_lag.x-Vaw_Exit)/phi,delta_Vaw_Avg_s=delta_Vaw_Avg/(phi*(1-Cov_s)),delta_Vaw_Cov_ss=delta_Vaw_Cov_s*phi_s/(phi*(1-Cov_s)))->Dynamic_Descomposition

Dynamic_Descomposition %>% mutate(DELTA_s=(Vaw.y-Vaw_lag.y)/phi) %>% select(anio,Ent,Exit,delta_Vaw_Avg_s,delta_Vaw_Cov_ss,DELTA_s)->Dynamic_Descomposition_correction

my.plots<-list()
Dynamic_Descomposition_correction %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Dynamic_Decomposition_updated.jpg")
my.plots[[1]]<-recordPlot()

Dynamic_Descomposition_s<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Dynamic_Descomposition_Sector_clean (2).csv")

Descomposition_s<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Sector_clean (2).csv")
Descomposition_s %>% select(anio,Sector,Vaw)->Des_s
Des_s %>% mutate(anio=as.numeric(anio)+5) %>% rename(Vaw_lag=Vaw)->Des_5_s
Dynamic_Descomposition_s %>% left_join(Des_s, by=c("Sector","anio")) %>% left_join(Des_5_s,by=c("Sector","anio"))->Dynamic_Descomposition_s

# It needs a correction that is explained in the appendix
Dynamic_Descomposition_s %>% mutate(phi_s=(Vaw.x+Vaw_lag.x)/2,phi=(Vaw.y+Vaw_lag.y)/2,Vaw_Cov_scale=Vaw_Cov/Vaw.x,Vaw_Cov_scale_lag=Vaw_Cov_lag/Vaw_lag.x,delta_Vaw_Cov_s=Vaw_Cov_scale-Vaw_Cov_scale_lag,Cov_s=(Vaw_Cov_scale+Vaw_Cov_scale_lag)/2)->Dynamic_Descomposition_s
Dynamic_Descomposition_s %>% mutate(Ent=w_firm_Ent*(Vaw_Ent-Vaw.x)/phi,Exit=w_firm_Exit*(Vaw_lag.x-Vaw_Exit)/phi,delta_Vaw_Avg_s=delta_Vaw_Avg/(phi*(1-Cov_s)),delta_Vaw_Cov_ss=delta_Vaw_Cov_s*phi_s/(phi*(1-Cov_s)))->Dynamic_Descomposition_s

Dynamic_Descomposition_s %>% mutate(DELTA_s=(Vaw.y-Vaw_lag.y)/phi) %>% select(anio,Sector,Ent,Exit,delta_Vaw_Avg_s,delta_Vaw_Cov_ss,DELTA_s)->Dynamic_Descomposition_correction_s

Dynamic_Descomposition_correction_s %>% filter(Sector==1) %>% select(-Sector) %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition, Manufacturing")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
my.plots[[2]]<-recordPlot()

Dynamic_Descomposition_correction_s %>% filter(Sector==0) %>% select(-Sector) %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition, Commerce & Services")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
my.plots[[3]]<-recordPlot()

#Dynamic_Descomposition_correction_s %>% filter(Sector==4) %>% select(-Sector) %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition, Tradable Services")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[3]]<-recordPlot()

#Dynamic_Descomposition_correction_s %>% filter(Sector==5) %>% select(-Sector) %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition, Non-Tradable Services")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[4]]<-recordPlot()

# Second Split

Dynamic_Descomposition_s_2split<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Dynamic_Descomposition_Sector_clean_2split.csv")

Descomposition_s_2split<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Sector_clean_2split.csv")
Descomposition_s_2split %>% select(anio,Manuf,Vaw)->Des_s_2split
Des_s_2split %>% mutate(anio=as.numeric(anio)+5) %>% rename(Vaw_lag=Vaw)->Des_5_s_2split
Dynamic_Descomposition_s_2split %>% left_join(Des_s_2split, by=c("Manuf","anio")) %>% left_join(Des_5_s_2split,by=c("Manuf","anio"))->Dynamic_Descomposition_s_2split

# It needs a correction that is explained in the appendix
Dynamic_Descomposition_s_2split %>% mutate(phi_s=(Vaw.x+Vaw_lag.x)/2,phi=(Vaw.y+Vaw_lag.y)/2,Vaw_Cov_scale=Vaw_Cov/Vaw.x,Vaw_Cov_scale_lag=Vaw_Cov_lag/Vaw_lag.x,delta_Vaw_Cov_s=Vaw_Cov_scale-Vaw_Cov_scale_lag,Cov_s=(Vaw_Cov_scale+Vaw_Cov_scale_lag)/2)->Dynamic_Descomposition_s_2split
Dynamic_Descomposition_s_2split %>% mutate(Ent=w_firm_Ent*(Vaw_Ent-Vaw.x)/phi,Exit=w_firm_Exit*(Vaw_lag.x-Vaw_Exit)/phi,delta_Vaw_Avg_s=delta_Vaw_Avg/(phi*(1-Cov_s)),delta_Vaw_Cov_ss=delta_Vaw_Cov_s*phi_s/(phi*(1-Cov_s)))->Dynamic_Descomposition_s_2split

Dynamic_Descomposition_s_2split %>% mutate(DELTA_s=(Vaw.y-Vaw_lag.y)/phi) %>% select(anio,Manuf,Ent,Exit,delta_Vaw_Avg_s,delta_Vaw_Cov_ss,DELTA_s)->Dynamic_Descomposition_correction_s_2split

Dynamic_Descomposition_correction_s_2split %>% filter(Manuf==0) %>% select(-Manuf) %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Decomposition, Commerce & Services")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
my.plots[[5]]<-recordPlot()


pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Dynamic_Descomposition_scaled_clean_1.pdf", onefile = TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

# Decomposition relative to 1994.