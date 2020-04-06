# Script to plot Growth Decomposition.

my.plots<-list()
Des<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_clean (2).csv")
Des %>% select(anio, Vaw_Avg, Vaw_Cov, Vaw) %>% gather(value = "Value", key = "Variable",-anio)->Des
Des %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Labor Productivity, Mean vs Covariance by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[4]]<-recordPlot()

Des_s<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Sector_clean (2).csv")

Des_s %>% select(anio,Sector, Vaw_Avg, Vaw_Cov, Vaw) %>% filter(Sector==1) %>% gather(value = "Value", key = "Variable",-c(anio,Sector))->Des_s_m
Des_s_m %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Manufacturing Labor Productivity, Mean vs Covariance by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[5]]<-recordPlot()

Des_s %>% select(anio,Sector, Vaw_Avg, Vaw_Cov, Vaw) %>% filter(Sector==0) %>% gather(value = "Value", key = "Variable",-c(anio,Sector))->Des_s_c
Des_s_c %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Commerce & Services Labor Productivity by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[6]]<-recordPlot()

#Des_s %>% select(anio,Sector, Vaw_Avg, Vaw_Cov, Vaw) %>% filter(Sector==4) %>% gather(value = "Value", key = "Variable",-c(anio,Sector))->Des_s_ts
#Des_s_ts %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Tradable Services Labor Productivity, Mean vs Covariance by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
#my.plots[[3]]<-recordPlot()

#Des_s %>% select(anio,Sector, Vaw_Avg, Vaw_Cov, Vaw) %>% filter(Sector==5) %>% gather(value = "Value", key = "Variable",-c(anio,Sector))->Des_s_nts
#Des_s_nts %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Non-Tradable Services Labor Productivity, Mean vs Covariance by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
#my.plots[[4]]<-recordPlot()

# Second split

Des_s_2split<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Sector_clean_2split.csv")

Des_s_2split %>% select(anio,Manuf, Vaw_Avg, Vaw_Cov, Vaw) %>% filter(Manuf==0) %>% gather(value = "Value", key = "Variable",-c(anio,Manuf))->Des_s_s
Des_s_s %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Within","Between"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Commerce & Services Labor Productivity, Mean vs Covariance by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[5]]<-recordPlot()


pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Mean_Cov_by_year_clean_1.pdf")
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

my.plots1<-list()

G_Des<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Growth_Decomposition_clean (2).csv")
G_Des %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm) %>% gather(value = "Value", key = "Variable",-anio)->G_Des
G_Des %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[7]]<-recordPlot()

G_Des_s<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Growth_Decomposition_Sectors_clean (2).csv")

G_Des_s %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm)  %>%  filter(Sector==1) %>% select(-Sector) %>% gather(value = "Value", key = "Variable",-anio)->G_Des_s_m
G_Des_s_m %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Manufacturing Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[8]]<-recordPlot()

G_Des_s %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm)  %>%  filter(Sector==0) %>% select(-Sector) %>% gather(value = "Value", key = "Variable",-anio)->G_Des_s_c
G_Des_s_c %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Commerce & Services Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[9]]<-recordPlot()

G_Des_s %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm)  %>%  filter(Sector==4) %>% select(-Sector) %>% gather(value = "Value", key = "Variable",-anio)->G_Des_s_ts
G_Des_s_ts %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Tradable Services Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots1[[3]]<-recordPlot()

G_Des_s %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm)  %>%  filter(Sector==5) %>% select(-Sector) %>% gather(value = "Value", key = "Variable",-anio)->G_Des_s_nts
G_Des_s_nts %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Non Tradable Services Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots1[[4]]<-recordPlot()

# Second split

G_Des_s_2split<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Growth_Decomposition_Sectors_clean_2split.csv")

G_Des_s_2split %>% rename(a_Vaw=Vaw,b_Prod_Mun=Prod_Mun,c_Prod_State=Prod_State,d_Prod_firm=Prod_firm)  %>%  filter(Sector==0) %>% select(-Sector) %>% gather(value = "Value", key = "Variable",-anio)->G_Des_s_s
G_Des_s_s %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Commerce & Services Labor Productivity, Decomposition by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots1[[5]]<-recordPlot()


pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decomposition_Vaw_by_year_clean_updated.pdf",onefile=TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

# Now, we want to see plot the absolute changes from 1994.

my.plots2<-list()
G_Des %>% group_by(Variable) %>% mutate(Value_a=if_else(anio==2014,Value-lag(Value,4),if_else(anio==2009,Value-lag(Value,3),if_else(anio==2004,Value-lag(Value,2),Value-lag(Value))))) %>% select(-Value) %>% rename(Value=Value_a)->G_Des_94
G_Des_94 %>% filter(anio!=1994) %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Labor Productivity Decomposition (changes from 1994)")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots2[[1]]<-recordPlot()

G_Des_s_m %>% group_by(Variable) %>% mutate(Value_a=if_else(anio==2014,Value-lag(Value,4),if_else(anio==2009,Value-lag(Value,3),if_else(anio==2004,Value-lag(Value,2),Value-lag(Value))))) %>% select(-Value) %>% rename(Value=Value_a)->G_Des_s_m_94
G_Des_s_m_94 %>% filter(anio!=1994) %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Manufacturing Labor Productivity Decomposition (changes from 1994)")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots2[[2]]<-recordPlot()

G_Des_s_s %>% group_by(Variable) %>% mutate(Value_a=if_else(anio==2014,Value-lag(Value,4),if_else(anio==2009,Value-lag(Value,3),if_else(anio==2004,Value-lag(Value,2),Value-lag(Value))))) %>% select(-Value) %>% rename(Value=Value_a)->G_Des_s_s_94
G_Des_s_s_94 %>% filter(anio!=1994) %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Commerce & Services Labor Productivity Decomposition (changes from 1994)")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots2[[3]]<-recordPlot()

G_Des_s_ts %>% group_by(Variable) %>% mutate(Value_a=if_else(anio==2014,Value-lag(Value,4),if_else(anio==2009,Value-lag(Value,3),if_else(anio==2004,Value-lag(Value,2),Value-lag(Value))))) %>% select(-Value) %>% rename(Value=Value_a)->G_Des_s_ts_94
G_Des_s_ts_94 %>% filter(anio!=1994) %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Tradable Services Labor Productivity Decomposition (changes from 1994)")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots2[[4]]<-recordPlot()

G_Des_s_nts %>% group_by(Variable) %>% mutate(Value_a=if_else(anio==2014,Value-lag(Value,4),if_else(anio==2009,Value-lag(Value,3),if_else(anio==2004,Value-lag(Value,2),Value-lag(Value))))) %>% select(-Value) %>% rename(Value=Value_a)->G_Des_s_nts_94
G_Des_s_nts_94 %>% filter(anio!=1994) %>% ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Total","Between Municipality","Between State","Between Firms","Within"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Non Tradable Services Labor Productivity Decomposition (changes from 1994)")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots2[[5]]<-recordPlot()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decomposition_by_year_clean_relative_1994.pdf",onefile=TRUE)
for (my.plot in my.plots2) {
  replayPlot(my.plot)
}
graphics.off()
