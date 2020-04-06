# Graphs by size, dodge; Final graphs without title.

require(tidyr)
require(dplyr)
require(ggplot2)

tabla<-read.csv("C:/Users/pc Luis/Documents/World Bank/Resultados/Table_by_size_1 (2).csv",header=TRUE)

tabla %>% filter(size!=0) %>% gather(value = "Value",key = "Variable",-c(anio,size)) %>% mutate(anio=as.factor(anio), size=as.factor(size)) %>% group_by(anio, Variable) %>% mutate(pct=prop.table(Value)*100)->tabla

tabla %>% filter(Variable=="Firms" | Variable=="Employees" | Variable=="VA" | Variable=="VA_per_w" | Variable=="Stock" | Variable=="Investment")->tabla_p

tabla_p %>% ungroup() %>% select(Variable) %>% distinct()->Var

my.plots<-list()

# Firms
i=1
tabla_p %>% filter(Variable==Var[[1]][i]) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
# Employees
i=2
tabla_p %>% filter(Variable==Var[[1]][i]) %>% mutate(Value=Value/1000000) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI. Value in millions.")
my.plots[[i]]<-recordPlot()
# VA
i=3
tabla_p %>% filter(Variable==Var[[1]][i]) %>% mutate(Value=Value/1000000) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI. Value in 2012 millions of thousands pesos.")
my.plots[[i]]<-recordPlot()
# VA per worker
i=4
tabla_p %>% filter(Variable==Var[[1]][i]) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI. Value in 2012 thousand pesos.")
my.plots[[i]]<-recordPlot()
# Stock
i=5
tabla_p %>% filter(Variable==Var[[1]][i]) %>% mutate(Value=Value/1000000) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI. Value in 2012 millions of thousands pesos.")
my.plots[[i]]<-recordPlot()
# Investment
i=6
tabla_p %>% filter(Variable==Var[[1]][i]) %>% mutate(Value=Value/1000000) %>% ggplot(aes(x=anio,y=Value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI. Value in 2012 millions of thousands pesos.")
my.plots[[i]]<-recordPlot()


pdf('C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs_by_size_clean.pdf', onefile=TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()
