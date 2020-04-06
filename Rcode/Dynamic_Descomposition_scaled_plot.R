# Dynamic_Descomposition graphs

Dynamic_Descomposition<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Dynamic_Descomposition.csv")

Dynamic_Descomposition %>% mutate(Ent=w_firm_Ent*(Vaw_Ent-Vaw),Exit=w_firm_Exit*(Vaw_lag-Vaw_Exit)) %>% mutate(delta_Vaw_Avg=delta_Vaw_Avg/Vaw_lag,delta_Vaw_Cov=delta_Vaw_Cov/Vaw_lag,Ent=Ent/Vaw_lag,Exit=Exit/Vaw_lag,DELTA=DELTA/Vaw_lag) %>%  select(anio,delta_Vaw_Avg,delta_Vaw_Cov,Ent,Exit,DELTA)->D_plot
pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Dynamic_Descomposition.pdf")
D_plot %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Covariance","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Descomposition")+theme(legend.position = "bottom")
graphics.off()

Descomposition<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition.csv")
Descomposition %>% select(anio,Vaw)->Des
Des %>% mutate(anio=as.numeric(anio)+5) %>% rename(Vaw_lag=Vaw)->Des_5
Dynamic_Descomposition %>% left_join(Des, by=("anio")) %>% left_join(Des_5,by=("anio"))->Dynamic_Descomposition

# It needs a correction that is explained in the appendix
Dynamic_Descomposition %>% mutate(phi_s=(Vaw.x+Vaw_lag.x)/2,phi=(Vaw.y+Vaw_lag.y)/2,Vaw_Cov_scale=Vaw_Cov/Vaw.x,Vaw_Cov_scale_lag=Vaw_Cov_lag/Vaw_lag.x,delta_Vaw_Cov_s=Vaw_Cov_scale-Vaw_Cov_scale_lag,Cov_s=(Vaw_Cov_scale+Vaw_Cov_scale_lag)/2)->Dynamic_Descomposition
Dynamic_Descomposition %>% mutate(Ent=w_firm_Ent*(Vaw_Ent-Vaw.x)/phi,Exit=w_firm_Exit*(Vaw_lag.x-Vaw_Exit)/phi,delta_Vaw_Avg_s=delta_Vaw_Avg/(phi*(1-Cov_s)),delta_Vaw_Cov_ss=delta_Vaw_Cov_s*phi_s/(phi*(1-Cov_s)))->Dynamic_Descomposition

Dynamic_Descomposition %>% mutate(DELTA_s=(Vaw.y-Vaw_lag.y)/phi) %>% select(anio,Ent,Exit,delta_Vaw_Avg_s,delta_Vaw_Cov_ss,DELTA_s)->Dynamic_Descomposition_correction
pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Dynamic_Descomposition_scaled.pdf")
Dynamic_Descomposition_correction %>% gather(Variable,Value,-anio) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(x=anio,y=Value,fill=Variable,label=paste0(round(Value*100,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels = c("Total", "Within", "Between","Entry", "Exit"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Dynamic Descomposition")+theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")
graphics.off()