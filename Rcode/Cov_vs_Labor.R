# Descomposition Municipality

Des<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Municipality.csv")
Des %>% mutate(Vaw_Cov=Vaw_Cov*N_firms)%>% group_by(anio,e03) %>% mutate(h001a_State=sum(h001a_Suma,na.rm=TRUE)) %>% mutate(size_mun=h001a_Suma/h001a_State,l_labor=log(h001a_Suma))->Des

plots<-list()
plots1<-list()
my.plots<-list()

Des %>% filter(anio==1994) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1994")->plots[[1]]
Des %>% filter(anio==1999) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1999")->plots[[2]]
Des %>% filter(anio==2004) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2004")->plots[[3]]
Des %>% filter(anio==2009) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2009")->plots[[4]]
Des %>% filter(anio==2014) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2014")->plots[[5]]
multiplot(plotlist=plots,cols = 2)
my.plots[[1]]<-recordPlot()


Des %>% filter(anio==1994) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1994")->plots1[[1]]
Des %>% filter(anio==1999) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1999")->plots1[[2]]
Des %>% filter(anio==2004) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2004")->plots1[[3]]
Des %>% filter(anio==2009) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2009")->plots1[[4]]
Des %>% filter(anio==2014) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2014")->plots1[[5]]
multiplot(plotlist=plots1,cols = 2)
my.plots[[2]]<-recordPlot()
graphics.off()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Cov_vs_L.pdf")
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

Des_SCIAN2dig<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Municipality_SCIAN2dig.csv")
Des_SCIAN2dig %>% mutate(Vaw_Cov=Vaw_Cov*N_firms)%>%  mutate(Manuf=if_else(SCIAN2dig==30,1,0))->Des_SCIAN2dig
Des_SCIAN2dig %>% group_by(anio,e03,e04) %>% mutate(h001a_total=sum(h001a_Suma,na.rm = TRUE)) %>% mutate(size_mun=h001a_Suma/h001a_total,l_labor=log(h001a_Suma))->Des_SCIAN2dig

plots<-list()
plots1<-list()
plots2<-list()
plots3<-list()

my.plots_sector<-list()

Des_SCIAN2dig %>% filter(anio==1994 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1994 Manufacturing")->plots[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1999 Manufacturing")->plots[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2004 Manufacturing")->plots[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2009 Manufacturing")->plots[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2014 Manufacturing")->plots[[5]]
multiplot(plotlist=plots,cols = 2)
my.plots_sector[[1]]<-recordPlot()


Des_SCIAN2dig %>% filter(anio==1994 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1994 Manufacturing")->plots1[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1999 Manufacturing")->plots1[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2004 Manufacturing")->plots1[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2009 Manufacturing")->plots1[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & Manuf==1) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2014 Manufacturing")->plots1[[5]]
multiplot(plotlist=plots1,cols = 2)
my.plots_sector[[2]]<-recordPlot()

Des_SCIAN2dig %>% filter(anio==1994 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1994 Commerce & Services ")->plots2[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("1999 Commerce & Services")->plots2[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2004 Commerce & Services")->plots2[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2009 Commerce & Services")->plots2[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle("2014 Commerce & Services")->plots2[[5]]
multiplot(plotlist=plots2,cols = 2)
my.plots_sector[[3]]<-recordPlot()


Des_SCIAN2dig %>% filter(anio==1994 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1994 Commerce & Services")->plots3[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("1999 Commerce & Services")->plots3[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2004 Commerce & Services")->plots3[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2009 Commerce & Services")->plots3[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & Manuf==0) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle("2014 Commerce & Services")->plots3[[5]]
multiplot(plotlist=plots3,cols = 2)
my.plots_sector[[4]]<-recordPlot()
graphics.off()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Cov_vs_L_Sector.pdf")
for (my.plot in my.plots_sector) {
  replayPlot(my.plot)
}
graphics.off()

# By SCIAN 2 digits

plots<-list()
my.plots_SCIAN<-list()

Des_SCIAN2dig %>% ungroup() %>% select(SCIAN2dig) %>% distinct()->SCIAN

for (i in 1:dim(SCIAN)[1]){

Des_SCIAN2dig %>% filter(anio==1994 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle(paste("1994",as.numeric(SCIAN[i,1]),sep=" "))->plots[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle(paste("1999",as.numeric(SCIAN[i,1]),sep=" "))->plots[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle(paste("2004",as.numeric(SCIAN[i,1]),sep=" "))->plots[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle(paste("2009",as.numeric(SCIAN[i,1]),sep=" "))->plots[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,size_mun))+geom_point()+ggtitle(paste("2014",as.numeric(SCIAN[i,1]),sep=" "))->plots[[5]]
multiplot(plotlist=plots,cols = 2)
my.plots_SCIAN[[i]]<-recordPlot()

Des_SCIAN2dig %>% filter(anio==1994 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle(paste("1994",as.numeric(SCIAN[i,1]),sep=" "))->plots[[1]]
Des_SCIAN2dig %>% filter(anio==1999 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle(paste("1999",as.numeric(SCIAN[i,1]),sep=" "))->plots[[2]]
Des_SCIAN2dig %>% filter(anio==2004 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle(paste("2004",as.numeric(SCIAN[i,1]),sep=" "))->plots[[3]]
Des_SCIAN2dig %>% filter(anio==2009 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle(paste("2009",as.numeric(SCIAN[i,1]),sep=" "))->plots[[4]]
Des_SCIAN2dig %>% filter(anio==2014 & SCIAN2dig==as.numeric(SCIAN[i,1])) %>%  ggplot(aes(Vaw_Cov,l_labor))+geom_point()+ggtitle(paste("2014",as.numeric(SCIAN[i,1]),sep=" "))->plots[[5]]
multiplot(plotlist=plots,cols = 2)
my.plots_SCIAN[[i+16]]<-recordPlot()
}
pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Cov_vs_L_SCIAN.pdf")
for (my.plot in my.plots_SCIAN) {
  replayPlot(my.plot)
}
graphics.off()

# Graphs of covariance and means by year

Des_Country<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Country.csv")
Des_Country %>% mutate(Vaw_Cov=Vaw_Cov*N_States) %>% select(anio,Vaw_Avg,Vaw_Cov) %>% gather(Variable,Value,-anio)->Des_Country
Des_Country %>% ggplot(aes(Variable,Value,fill=as.factor(anio),label=paste0(round(Value,3))))+geom_bar(position = "dodge",stat="identity")+labs(fill="year")+geom_text(,size = 3, position = position_dodge(width = 0.9))+ggtitle("Labor Productivity, Mean vs Covariance by year")
my.plots_Mean<-recordPlot()
graphics.off()
pdf('C:/Users/pc Luis/Documents/World Bank/Mean_Cov_year.pdf')
replayPlot(my.plots_Mean)
graphics.off()



# Graphs of Covariance denisty at SCIAN4 dig
Des_SCIAN4dig<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Descomposition_Municipality_SCIAN4dig.csv")
Des_SCIAN4dig %>% mutate(Vaw_Cov=Vaw_Cov*N_firms)->Des_SCIAN4dig
summary(Des_SCIAN4dig$Vaw_Cov)
# We have that maximun and minimun values are too big in comparisson, so we trim the data, we drop values below P1 and above P99.
Des_SCIAN4dig %>% group_by(anio) %>% mutate(P01=quantile(Vaw_Cov,.01,na.rm=TRUE),P99=quantile(Vaw_Cov,.99,na.rm=TRUE)) %>% filter(Vaw_Cov<=P99 & Vaw_Cov>=P01)->Des_SCIAN4dig_plot

plot_list_SCIAN4dig<-list()
my.plots_SCIAN4dig<-list()

Des_SCIAN4dig_plot %>% filter(anio==1994) %>%   ggplot(aes(x=Vaw_Cov))+ geom_histogram(aes(y=..density..), bins=100,colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mun Covariance 1994 ")
my.plots_SCIAN4dig[[1]]<-recordPlot()
Des_SCIAN4dig_plot %>% filter(anio==1999) %>%   ggplot(aes(x=Vaw_Cov))+ geom_histogram(aes(y=..density..), bins=100, colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mun Covariance 1999 ")
my.plots_SCIAN4dig[[2]]<-recordPlot()
Des_SCIAN4dig_plot %>% filter(anio==2004) %>%   ggplot(aes(x=Vaw_Cov))+ geom_histogram(aes(y=..density..), bins=100, colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mun Covariance 2004 ")
my.plots_SCIAN4dig[[3]]<-recordPlot()
Des_SCIAN4dig_plot %>% filter(anio==2009) %>%   ggplot(aes(x=Vaw_Cov))+ geom_histogram(aes(y=..density..), bins=100, colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mun Covariance 2009 ")
my.plots_SCIAN4dig[[4]]<-recordPlot()
Des_SCIAN4dig_plot %>% filter(anio==2014) %>%   ggplot(aes(x=Vaw_Cov))+ geom_histogram(aes(y=..density..), bins=100, colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")+ggtitle("Mun Covariance 2014 ")
my.plots_SCIAN4dig[[5]]<-recordPlot()

#multiplot(plotlist = plot_list_SCIAN4dig,cols=2)
#my.plots_SCIAN4dig<-recordPlot()
graphics.off()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Cov_density.pdf",onefile = TRUE)
for(my.plot in my.plots_SCIAN4dig){
replayPlot(my.plot)
}
graphics.off()
