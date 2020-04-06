# Script for Entrants Survivors and Exiters by State and Sector (SCIAN2digit)
require(dplyr)
require(tidyr)
require(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
Ent_Surv_Exit<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Ent_Surv_States_Sector.csv")

# Check why the difference of labors is not equal to the sum of Entrants, Survivors(Increase, Decrease) and Exiters, something must be wrong.

Ent_Surv_Exit %>%  mutate(Ent=Ent_99+Ent_04+Ent_09+Ent_14,Surv_Increase=Surv_99_pos+Surv_04_pos+Surv_09_pos+Surv_14_pos,Surv_Decrease=Surv_99_neg+Surv_04_neg+Surv_09_neg+Surv_14_neg,Exit=Exit_99+Exit_04+Exit_09+Exit_14) %>% select(anio,e03,SCIAN2dig,Ent,Surv_Increase,Surv_Decrease,Exit)->Labor
Labor %>% select(anio,e03,SCIAN2dig,Exit) %>% filter(anio<2014) %>% mutate(anio=anio+5)->Exiters
Labor %>% select(-Exit) %>% left_join(Exiters, by=c("anio","e03","SCIAN2dig")) %>% filter(anio>1994) %>% mutate(Exit=-Exit, Delta=Ent+Surv_Increase+Surv_Decrease+Exit)->Labor_final

Labor_final %>% gather(Variable, Value,-c(anio,e03,SCIAN2dig))->Labor_plot

Nom_Estados<-haven::read_dta("C:/Users/pc Luis/Documents/World Bank/Codes states.dta")
Naics<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Lista de Sectores.csv")
Naics %>% select(-V3) %>% filter(V1>=30 ,V1!="4" ,V1!="5_6_7_8")->Naics
anios<-c(1999,2004,2009,2014)


my.plots<-list()
plot_list<-list()
plot_list_anio<-list()

for (i in 1:4){
  for(j in 1:32){
    for(k in 1:15){  
      q<-Labor_plot %>% filter(anio==anios[i],e03==as.numeric(Nom_Estados[j,1]),SCIAN2dig==as.numeric(Naics[k,1])) %>% ggplot(aes(x=Variable,y=Value))+ geom_bar(stat="identity")+labs(y="Employees")+ggtitle(paste(Naics[k,1],Nom_Estados[j,2],anios[i],sep=" "))
      plot_list[[length(plot_list)+1]]<-q
    }
  }
  plot_list_anio[[i]]<-plot_list
}
plost_list_anio[[i]]<-plot_list[(i-1)*(480)+1,480*i]
multiplot(plotlist=plot_list_anio[[1]], cols=15)
my.plots<-recordPlot()

