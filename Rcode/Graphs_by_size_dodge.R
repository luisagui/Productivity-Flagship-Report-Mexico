# Script for regression.

delta.log(Vaw)~(l.National_Frontier-l.vaw)/l.National_Frontier+(l.States_Frontier-l.vaw)/l.States_Frontier+(l.Municipality_Frontier-l.vaw)/Municipality_Frontier+delta.log(q000a_real/h001a)+delta.log(l.National_Frontier)

# Graphs by size
require(tidyr)
require(dplyr)
require(ggplot2)


tabla<-read.csv("C:/Users/pc Luis/Documents/World Bank/Resultados/Table_by_size.csv",header=TRUE)
names(tabla)[1]<-"Legend"
tabla %>% select(-Legend) %>% filter(Variable=="Firms" | Variable=="Employees"| Variable=="VA"| Variable=="VA_per_w"| Variable=="Stock_Machinery" | Variable=="Investment_Machinery" | Variable=="Stock_Buildings"| Variable=="Investment_Buildings"| Variable=="Stock_Transport" | Variable=="Investment_Transport" |Variable=="Stock_Computers"| Variable=="Investment_Computers" |Variable=="Stock_Office_eq"| Variable=="Investment_Office_eq")->tabla
tabla %>% gather("anio","value",-c(size,Variable)) %>% mutate(anio=substr(anio,2,length(anio)),size=factor(size),value=as.numeric(value)) %>% group_by(Variable,anio) %>% mutate(pct=prop.table(value)*100, Total=sum(value,na.rm=TRUE)) %>% arrange(Variable, anio)->tabla
Var<-as.character(unique(tabla$Variable))

my.plots<-list()
for (i in 1:14){
#tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 20, label = Total, fill = NULL))+scale_y_continuous(breaks=seq(0,5000000,1000000))+ggtitle("Number of Firms by size")
tabla %>% filter(Variable==Var[i]) %>% mutate(value=value/1000000) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
} 
graphics.off()
i=1
tabla %>% filter(Variable==Var[i]) %>% mutate(value=value/1000000) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=2
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=3
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=4
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=5
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=6
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=7
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=8
tabla %>% filter(Variable==Var[i]) %>% mutate(value=value/1000000) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=9
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=10
tabla %>% filter(Variable==Var[i]) %>% mutate(value=value/1000000) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=11
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=12
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=13
tabla %>% filter(Variable==Var[i]) %>% mutate(value=value/1000000) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()
i=14
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity",position="dodge")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle(paste(Var[i], "by size", sep=" "))+coord_flip()+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI ")
my.plots[[i]]<-recordPlot()

graphics.off()


pdf('C:/Users/pc Luis/Documents/World Bank/Prueba.pdf', onefile=TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()
Var[1]
tabla %>% filter(Variable==Var[3])
