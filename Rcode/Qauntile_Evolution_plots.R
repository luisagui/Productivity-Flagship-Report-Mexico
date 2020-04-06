# Script to plot wvolution of particular percentiles of different variables at different disagreggations.

require(dplyr)
require(tidyr)
require(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

my.plots<-list()

# All economy
Q_anual<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices.csv")

my.plots1<-list()

my.plots1[[1]]<-Q_anual %>% filter(Variable=="h001a") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[1]]<-recordPlot()

my.plots1[[2]]<-Q_anual %>% filter(Variable=="q000c") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[2]]<-recordPlot()

my.plots1[[3]]<-Q_anual %>% filter(Variable=="sales") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[3]]<-recordPlot()

my.plots1[[4]]<-Q_anual %>% filter(Variable=="q010a") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[4]]<-recordPlot()

my.plots1[[5]]<-Q_anual %>% filter(Variable=="q400a") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[5]]<-recordPlot()

my.plots1[[6]]<-Q_anual %>% filter(Variable=="Vaw") %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
#my.plots[[6]]<-recordPlot()

multiplot(plotlist = my.plots1,cols=3)
my.plots[[1]]<-recordPlot()

# Manufacturing & Services

  Q_anual_Manuf<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_Manuf.csv")
  my.plots2<-list()

  # Manufacturing
  
  my.plots2[[1]]<-Q_anual_Manuf %>% filter(Variable=="h001a" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[7]]<-recordPlot()
  
  my.plots2[[2]]<-Q_anual_Manuf %>% filter(Variable=="q000c" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[8]]<-recordPlot()
  
  my.plots2[[3]]<-Q_anual_Manuf %>% filter(Variable=="sales" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[9]]<-recordPlot()
  
  my.plots2[[4]]<-Q_anual_Manuf %>% filter(Variable=="q010a" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[10]]<-recordPlot()
  
  my.plots2[[5]]<-Q_anual_Manuf %>% filter(Variable=="q400a" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[11]]<-recordPlot()
  
  my.plots2[[6]]<-Q_anual_Manuf %>% filter(Variable=="Vaw" & Manuf==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[12]]<-recordPlot()
  
  multiplot(plotlist = my.plots2,cols=3)
  my.plots[[2]]<-recordPlot()
  
  # Services
  
  my.plots3<-list()
  
  my.plots3[[1]]<-Q_anual_Manuf %>% filter(Variable=="h001a" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[13]]<-recordPlot()
  
  my.plots3[[2]]<-Q_anual_Manuf %>% filter(Variable=="q000c" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[14]]<-recordPlot()
  
  my.plots3[[3]]<-Q_anual_Manuf %>% filter(Variable=="sales" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[15]]<-recordPlot()
  
  my.plots3[[4]]<-Q_anual_Manuf %>% filter(Variable=="q010a" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[16]]<-recordPlot()
  
  my.plots3[[5]]<-Q_anual_Manuf %>% filter(Variable=="q400a" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[17]]<-recordPlot()
  
  my.plots3[[6]]<-Q_anual_Manuf %>% filter(Variable=="Vaw" & Manuf==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[18]]<-recordPlot()
  
  multiplot(plotlist = my.plots3,cols=3)
  my.plots[[3]]<-recordPlot()
  
# Tradable Services & Non-Tradable Services
  
  Q_anual_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_sector.csv")
  
  my.plots4<-list()
  # Tradable Services
  
  my.plots4[[1]]<-Q_anual_sector %>% filter(Variable=="h001a" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[19]]<-recordPlot()
  
  my.plots4[[2]]<-Q_anual_sector %>% filter(Variable=="q000c" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[20]]<-recordPlot()
  
  my.plots4[[3]]<-Q_anual_sector %>% filter(Variable=="sales" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[21]]<-recordPlot()
  
  my.plots4[[4]]<-Q_anual_sector %>% filter(Variable=="q010a" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[22]]<-recordPlot()
  
  my.plots4[[5]]<-Q_anual_sector %>% filter(Variable=="q400a" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[23]]<-recordPlot()
  
  my.plots4[[6]]<-Q_anual_sector %>% filter(Variable=="Vaw" & Sector==4) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[24]]<-recordPlot()
  
  multiplot(plotlist = my.plots4,cols=3)
  my.plots[[4]]<-recordPlot()
  
  # Non Tradable Services
  
  my.plots5<-list()
  
  my.plots5[[1]]<-Q_anual_sector %>% filter(Variable=="h001a" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[25]]<-recordPlot()
  
  my.plots5[[2]]<-Q_anual_sector %>% filter(Variable=="q000c" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[26]]<-recordPlot()
  
  my.plots5[[3]]<-Q_anual_sector %>% filter(Variable=="sales" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[27]]<-recordPlot()
  
  my.plots5[[4]]<-Q_anual_sector %>% filter(Variable=="q010a" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[28]]<-recordPlot()
  
  my.plots5[[5]]<-Q_anual_sector %>% filter(Variable=="q400a" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[29]]<-recordPlot()
  
  my.plots5[[6]]<-Q_anual_sector %>% filter(Variable=="Vaw" & Sector==5) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[30]]<-recordPlot()
  
  multiplot(plotlist = my.plots5,cols=3)
  my.plots[[5]]<-recordPlot()
  
# Knowledge Intensice Services & Non KIS
  
  Q_anual_Kbs<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_Kbs.csv")
  
  my.plots6<-list()
  
  # KIS
  
  my.plots6[[1]]<-Q_anual_Kbs %>% filter(Variable=="h001a" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[31]]<-recordPlot()
  
  my.plots6[[2]]<-Q_anual_Kbs %>% filter(Variable=="q000c" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[32]]<-recordPlot()
  
  my.plots6[[3]]<-Q_anual_Kbs %>% filter(Variable=="sales" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[33]]<-recordPlot()
  
  my.plots6[[4]]<-Q_anual_Kbs %>% filter(Variable=="q010a" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[34]]<-recordPlot()
  
  my.plots6[[5]]<-Q_anual_Kbs %>% filter(Variable=="q400a" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[35]]<-recordPlot()
  
  my.plots6[[6]]<-Q_anual_Kbs %>% filter(Variable=="Vaw" & Kbs==1) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[36]]<-recordPlot()
  
  multiplot(plotlist = my.plots6,cols=3)
  my.plots[[6]]<-recordPlot()
  
  # Non KIS
  
  my.plots7<-list()
  
  my.plots7[[1]]<-Q_anual_Kbs %>% filter(Variable=="h001a" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[37]]<-recordPlot()
  
  my.plots7[[2]]<-Q_anual_Kbs %>% filter(Variable=="q000c" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[38]]<-recordPlot()
  
  my.plots7[[3]]<-Q_anual_Kbs %>% filter(Variable=="sales" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[39]]<-recordPlot()
  
  my.plots7[[4]]<-Q_anual_Kbs %>% filter(Variable=="q010a" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[40]]<-recordPlot()
  
  my.plots7[[5]]<-Q_anual_Kbs %>% filter(Variable=="q400a" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[41]]<-recordPlot()
  
  my.plots7[[6]]<-Q_anual_Kbs %>% filter(Variable=="Vaw" & Kbs==0) %>% group_by(stat) %>% mutate(Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[42]]<-recordPlot()
  
  multiplot(plotlist = my.plots7,cols=3)
  my.plots[[7]]<-recordPlot()
  
  pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Quantile_evolution.pdf",onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
  
  #################################################################   Quantile evoultion using logs of percentiles
  
  loghyper<-function(k){
    a<-log(k+sqrt(k^2+1))
    return(a)
  }
  
  my.plots<-list()
  
  # All economy
  Q_anual<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices.csv")
  
  my.plots1<-list()
  
  my.plots1[[1]]<-Q_anual %>% filter(Variable=="h001a") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[1]]<-recordPlot()
  
  my.plots1[[2]]<-Q_anual %>% filter(Variable=="q000c") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[2]]<-recordPlot()
  
  my.plots1[[3]]<-Q_anual %>% filter(Variable=="sales") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[3]]<-recordPlot()
  
  my.plots1[[4]]<-Q_anual %>% filter(Variable=="q010a") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[4]]<-recordPlot()
  
  my.plots1[[5]]<-Q_anual %>% filter(Variable=="q400a") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[5]]<-recordPlot()
  
  my.plots1[[6]]<-Q_anual %>% filter(Variable=="Vaw") %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[6]]<-recordPlot()
  
  multiplot(plotlist = my.plots1,cols=3)
  my.plots[[1]]<-recordPlot()
  
  # Manufacturing & Services
  
  Q_anual_Manuf<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_Manuf.csv")
  my.plots2<-list()
  
  # Manufacturing
  
  my.plots2[[1]]<-Q_anual_Manuf %>% filter(Variable=="h001a" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[7]]<-recordPlot()
  
  my.plots2[[2]]<-Q_anual_Manuf %>% filter(Variable=="q000c" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[8]]<-recordPlot()
  
  my.plots2[[3]]<-Q_anual_Manuf %>% filter(Variable=="sales" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[9]]<-recordPlot()
  
  my.plots2[[4]]<-Q_anual_Manuf %>% filter(Variable=="q010a" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[10]]<-recordPlot()
  
  my.plots2[[5]]<-Q_anual_Manuf %>% filter(Variable=="q400a" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[11]]<-recordPlot()
  
  my.plots2[[6]]<-Q_anual_Manuf %>% filter(Variable=="Vaw" & Manuf==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[12]]<-recordPlot()
  
  multiplot(plotlist = my.plots2,cols=3)
  my.plots[[2]]<-recordPlot()
  
  # Services
  
  my.plots3<-list()
  
  my.plots3[[1]]<-Q_anual_Manuf %>% filter(Variable=="h001a" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[13]]<-recordPlot()
  
  my.plots3[[2]]<-Q_anual_Manuf %>% filter(Variable=="q000c" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[14]]<-recordPlot()
  
  my.plots3[[3]]<-Q_anual_Manuf %>% filter(Variable=="sales" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[15]]<-recordPlot()
  
  my.plots3[[4]]<-Q_anual_Manuf %>% filter(Variable=="q010a" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[16]]<-recordPlot()
  
  my.plots3[[5]]<-Q_anual_Manuf %>% filter(Variable=="q400a" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[17]]<-recordPlot()
  
  my.plots3[[6]]<-Q_anual_Manuf %>% filter(Variable=="Vaw" & Manuf==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[18]]<-recordPlot()
  
  multiplot(plotlist = my.plots3,cols=3)
  my.plots[[3]]<-recordPlot()
  
  # Tradable Services & Non-Tradable Services
  
  Q_anual_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_sector.csv")
  
  my.plots4<-list()
  # Tradable Services
  
  my.plots4[[1]]<-Q_anual_sector %>% filter(Variable=="h001a" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[19]]<-recordPlot()
  
  my.plots4[[2]]<-Q_anual_sector %>% filter(Variable=="q000c" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[20]]<-recordPlot()
  
  my.plots4[[3]]<-Q_anual_sector %>% filter(Variable=="sales" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[21]]<-recordPlot()
  
  my.plots4[[4]]<-Q_anual_sector %>% filter(Variable=="q010a" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[22]]<-recordPlot()
  
  my.plots4[[5]]<-Q_anual_sector %>% filter(Variable=="q400a" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[23]]<-recordPlot()
  
  my.plots4[[6]]<-Q_anual_sector %>% filter(Variable=="Vaw" & Sector==4) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[24]]<-recordPlot()
  
  multiplot(plotlist = my.plots4,cols=3)
  my.plots[[4]]<-recordPlot()
  
  # Non Tradable Services
  
  my.plots5<-list()
  
  my.plots5[[1]]<-Q_anual_sector %>% filter(Variable=="h001a" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[25]]<-recordPlot()
  
  my.plots5[[2]]<-Q_anual_sector %>% filter(Variable=="q000c" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[26]]<-recordPlot()
  
  my.plots5[[3]]<-Q_anual_sector %>% filter(Variable=="sales" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[27]]<-recordPlot()
  
  my.plots5[[4]]<-Q_anual_sector %>% filter(Variable=="q010a" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[28]]<-recordPlot()
  
  my.plots5[[5]]<-Q_anual_sector %>% filter(Variable=="q400a" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[29]]<-recordPlot()
  
  my.plots5[[6]]<-Q_anual_sector %>% filter(Variable=="Vaw" & Sector==5) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[30]]<-recordPlot()
  
  multiplot(plotlist = my.plots5,cols=3)
  my.plots[[5]]<-recordPlot()
  
  # Knowledge Intensice Services & Non KIS
  
  Q_anual_Kbs<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Quantile_Indices_Kbs.csv")
  
  my.plots6<-list()
  
  # KIS
  
  my.plots6[[1]]<-Q_anual_Kbs %>% filter(Variable=="h001a" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[31]]<-recordPlot()
  
  my.plots6[[2]]<-Q_anual_Kbs %>% filter(Variable=="q000c" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[32]]<-recordPlot()
  
  my.plots6[[3]]<-Q_anual_Kbs %>% filter(Variable=="sales" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[33]]<-recordPlot()
  
  my.plots6[[4]]<-Q_anual_Kbs %>% filter(Variable=="q010a" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[34]]<-recordPlot()
  
  my.plots6[[5]]<-Q_anual_Kbs %>% filter(Variable=="q400a" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[35]]<-recordPlot()
  
  my.plots6[[6]]<-Q_anual_Kbs %>% filter(Variable=="Vaw" & Kbs==1) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[36]]<-recordPlot()
  
  multiplot(plotlist = my.plots6,cols=3)
  my.plots[[6]]<-recordPlot()
  
  # Non KIS
  
  my.plots7<-list()
  
  my.plots7[[1]]<-Q_anual_Kbs %>% filter(Variable=="h001a" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Employees ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[37]]<-recordPlot()
  
  my.plots7[[2]]<-Q_anual_Kbs %>% filter(Variable=="q000c" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Total Investment ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[38]]<-recordPlot()
  
  my.plots7[[3]]<-Q_anual_Kbs %>% filter(Variable=="sales" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Sales ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[39]]<-recordPlot()
  
  my.plots7[[4]]<-Q_anual_Kbs %>% filter(Variable=="q010a" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Machinery") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[40]]<-recordPlot()
  
  my.plots7[[5]]<-Q_anual_Kbs %>% filter(Variable=="q400a" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("ICT equipment") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))#+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[41]]<-recordPlot()
  
  my.plots7[[6]]<-Q_anual_Kbs %>% filter(Variable=="Vaw" & Kbs==0) %>% group_by(stat) %>% mutate(Value=loghyper(Value),Value1=Value-first(Value)) %>% ggplot(aes(anio,Value1)) +geom_point(aes(color=stat))+ geom_line(aes(color=stat))+ labs(x=NULL,y=NULL,color=NULL)+scale_x_continuous(breaks=c(1994,1999,2004,2009,2014))+ ggtitle("Labor Productivity ") + theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  #my.plots[[42]]<-recordPlot()
  
  multiplot(plotlist = my.plots7,cols=3)
  my.plots[[7]]<-recordPlot()
  
  pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Quantile_evolution_logs.pdf",onefile=TRUE)
  for (my.plot in my.plots) {
    replayPlot(my.plot)
  }
  graphics.off()
  