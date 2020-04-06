my.plots1<-list()
i=1
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 1000000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=2
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 100000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=3
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 100000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=4
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 10000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=5
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 100000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=6
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 50000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=7
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 10000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=8
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 500000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=9
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 100000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=10
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 1000000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=11
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 100000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=12
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 50000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=13
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 1000000, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()
i=14
tabla %>% filter(Variable==Var[i]) %>% ggplot(aes(x=anio,y=value,fill=size,label=paste0(round(pct,2),"%")))+geom_bar(stat="identity")+labs(x="year", fill="Size")+scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+geom_text(size = 3, position = position_stack(vjust = 0.5))+geom_text(aes(anio, Total + 1, label = round(Total), fill = NULL))+ggtitle(paste(Var[i], "by size", sep=" "))
my.plots1[[i]]<-recordPlot()

graphics.off()

pdf('C:/Users/pc Luis/Documents/World Bank/Prueba.pdf', onefile=TRUE,width = 8.5, height = 11)
for (my.plot in my.plots1) {
  replayPlot(my.plot)
}
graphics.off()
