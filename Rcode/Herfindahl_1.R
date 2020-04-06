# Herfindahl Graphs and regressions.

require(dplyr)
require(tidyr)
require(ggplot2)
require(lfe)
require()

Herfindahl_anual<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Herfindahl_anual_1.CSV")
my.plots<-list()

Herfindahl_anual %>% select(anio,Sector, starts_with("Herf")) %>%  filter(Sector==1) %>% gather(key="Variable",value="Value",-c(anio,Sector)) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3))))+geom_bar(stat="identity", position = "dodge")+labs(x="year")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Manufacturing Herfindahl by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")
my.plots[[1]]<-recordPlot()
graphics.off()
Herfindahl_anual %>% select(anio,Sector, starts_with("Herf")) %>%  filter(Sector==4) %>% gather(key="Variable",value="Value",-c(anio,Sector)) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3))))+geom_bar(stat="identity", position = "dodge")+labs(x="year")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Tradable-Services Herfindahl by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")
my.plots[[2]]<-recordPlot()
graphics.off()
Herfindahl_anual %>% select(anio,Sector, starts_with("Herf")) %>%  filter(Sector==5) %>% gather(key="Variable",value="Value",-c(anio,Sector)) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3))))+geom_bar(stat="identity", position = "dodge")+labs(x="year")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Non-Tradable Services Herfindahl by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")
my.plots[[3]]<-recordPlot()
graphics.off()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Herfindahl_Manuf_by_year_clean_1.pdf", onefile=TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

# Regresiones de Herfindahl por Municipio.
Herfindahl_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Herfindahl_Municipality_1.CSV")

Herfindahl_mun %>% select(-N)->Herfindahl_mun
ols1<-lm(Herf_emp~D_Manuf*as.factor(anio),data=Herfindahl_sector)
ols2<-lm(Herf_va~D_Manuf*as.factor(anio),data=Herfindahl_sector)
ols3<-lm(Herf_stock~D_Manuf*as.factor(anio),data=Herfindahl_sector)
ols4<-lm(Herf_inv~D_Manuf*as.factor(anio),data=Herfindahl_sector)


ols5<-felm(Herf_emp~D_Manuf*as.factor(anio)| 0| 0 | e17,data=Herfindahl_sector)
ols6<-felm(Herf_va~D_Manuf*as.factor(anio)| 0| 0 | e17,data=Herfindahl_sector)
ols7<-felm(Herf_stock~D_Manuf*as.factor(anio)| 0| 0 | e17,data=Herfindahl_sector)
ols8<-felm(Herf_inv~D_Manuf*as.factor(anio)| 0| 0 | e17,data=Herfindahl_sector)

stargazer(ols1,ols2,ols3,ols4,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="C:/Users/pc Luis/Documents/World Bank/Resultados/Regressions/Herfindahl.txt")
stargazer(ols1,ols2,ols3,ols4,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="C:/Users/pc Luis/Documents/World Bank/Resultados/Regressions/Herfindahl.tex")

stargazer(ols5,ols6,ols7,ols8,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="C:/Users/pc Luis/Documents/World Bank/Resultados/Regressions/Herfindahl_1.txt")
stargazer(ols5,ols6,ols7,ols8,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="C:/Users/pc Luis/Documents/World Bank/Resultados/Regressions/Herfindahl_1.tex")

