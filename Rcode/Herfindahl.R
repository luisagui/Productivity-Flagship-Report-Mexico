# Herfindahl Graphs and regressions.

require(dplyr)
require(tidyr)
require(ggplot2)
require(lfe)
require()

Herfindahl_anual<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Herfindahl_anual (2).CSV")

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Herfindahl_by_year.pdf")
Herfindahl_anual %>% select(anio, starts_with("Herf")) %>% gather(key="Variable",value="Value",-anio) %>%  ggplot(aes(x=as.factor(anio),y=Value,fill=Variable,label=paste0(round(Value,3))))+geom_bar(stat="identity", position = "dodge")+labs(x="year")+scale_fill_discrete(labels=c("Employment","Investment","Stock","Value added"))+geom_text(size = 3, position = position_dodge(width = 0.9))+ggtitle("Herfindahl by year")+theme(legend.position = "bottom", plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")
graphics.off()

# Regresiones de Herfindahl por Sector a 6 dígitos.
Herfindahl_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Herfindahl_sector (2).CSV")

Herfindahl_sector %>% select(anio,e17,starts_with("Herf")) %>% mutate(SCIAN2dig=substr(e17,1,2),SCIAN2dig=replace(SCIAN2dig,which(SCIAN2dig=="31"| SCIAN2dig=="32"| SCIAN2dig=="33"),"30"),D_Manuf=0,D_Manuf=replace(D_Manuf,which(SCIAN2dig=="30"),1))->Herfindahl_sector
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
