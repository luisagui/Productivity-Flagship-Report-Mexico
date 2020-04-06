# Script to plot Structural change.

structural<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Structural_change_1.csv")
structural %>% rename(Z_Vaw_delta=Vaw_delta)->structural
structural %>% select(anio,Type, Structural, Within, Z_Vaw_delta) %>% gather(value = "Value", key = "Variable",-c(anio,Type))->structural

my.plots<-list()
structural %>% filter(Type==1 & anio>1994) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Structural","Within","Total"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Short term change in Labor Productivity, Major sectors")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI ")
my.plots[[1]]<-recordPlot()
structural %>% filter(Type==3 & anio>1994) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="year",fill="Decomposition")+scale_fill_discrete(labels=c("Structural","Within","Total"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Short term change in Labor Productivity, NAICS 2 dig")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI ")
my.plots[[2]]<-recordPlot()
structural %>% filter(Type==2 | Type==4) %>%  ggplot(aes(x=as.factor(Type),y=Value,fill=Variable,label=paste0(round(Value,3)))) +geom_bar(stat="identity",position="dodge")+labs(x="Sectors",fill="Decomposition")+scale_x_discrete(labels=c("Major sectors","NAICS 2 dig"))+scale_fill_discrete(labels=c("Structural","Within","Total"))+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Long term change in Labor Productivity")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI ")
my.plots[[3]]<-recordPlot()



pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Structural_change_1.pdf", onefile = TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

structural_st<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Structural_change_states.csv")
