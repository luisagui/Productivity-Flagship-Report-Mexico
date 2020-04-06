# Script to compute geographic concentrations analysis

# I already computed top and bottom industries, splitting manufacturing and services.
# We want to do additional analysis related to this indices.

require(dplyr)
require(tidyr)
require(ggplot2)
require(stargazer)

# First we are gonna compute histograms of  the indices, separately and together, It also has to be computed for 
# every census year. So, we are gonna have 3x2=6 different histograms, make sure they have the same cale,
# so that they can be somehow easy to compare visualy.
# I think the best way to plot them is to put all the evolution of a particular disagregation in the same plot.
# For this we are gonna need multiplot function.

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

# Manufacturing/ States

my.plots1<-list()

my.plots1[[1]]<-gc_manuf_state %>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots1[[2]]<-gc_manuf_state %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots1[[3]]<-gc_manuf_state %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots1[[4]]<-gc_manuf_state %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots1[[5]]<-gc_manuf_state %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots1,cols = 3)
my.plots[[1]]<-recordPlot()

# Manufacturing / Municipalties

my.plots2<-list()

my.plots2[[1]]<-gc_manuf_mun %>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots2[[2]]<-gc_manuf_mun %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots2[[3]]<-gc_manuf_mun %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots2[[4]]<-gc_manuf_mun %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots2[[5]]<-gc_manuf_mun %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots2,cols = 3)
my.plots[[2]]<-recordPlot()

# Commerce & Services /States

my.plots3<-list()

my.plots3[[1]]<-gc_serv_state %>% mutate(g=as.numeric(g))%>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots3[[2]]<-gc_serv_state %>% mutate(g=as.numeric(g)) %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots3[[3]]<-gc_serv_state %>% mutate(g=as.numeric(g)) %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots3[[4]]<-gc_serv_state %>% mutate(g=as.numeric(g)) %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots3[[5]]<-gc_serv_state %>% mutate(g=as.numeric(g)) %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots3,cols = 3)
my.plots[[3]]<-recordPlot()

# Commerce & Se4rvices/ Municipalities

my.plots4<-list()

my.plots4[[1]]<-gc_serv_mun %>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots4[[2]]<-gc_serv_mun %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots4[[3]]<-gc_serv_mun %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots4[[4]]<-gc_serv_mun %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots4[[5]]<-gc_serv_mun %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots4,cols = 3)
my.plots[[4]]<-recordPlot()

# Together

gc_state<-gc_serv_state %>% mutate(g=as.numeric(g),Low_Med_High=as.numeric(Low_Med_High)) %>% bind_rows(gc_manuf_state) %>% select(-Description)
gc_mun<-gc_manuf_mun %>% select(-Description,-Low_Med_High) %>% bind_rows(gc_serv_mun) %>% mutate(Low_Med_High=if_else(g<.02,1,if_else(g<.05,2,3)))

# State

my.plots5<-list()

my.plots5[[1]]<-gc_state %>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots5[[2]]<-gc_state %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots5[[3]]<-gc_state %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots5[[4]]<-gc_state %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots5[[5]]<-gc_state %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots5,cols = 3)
my.plots[[5]]<-recordPlot()

# Municipalities

my.plots6<-list()

my.plots6[[1]]<-gc_mun %>% filter(anio==1994) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1994")
my.plots6[[2]]<-gc_mun %>% filter(anio==1999) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("1999")
my.plots6[[3]]<-gc_mun %>% filter(anio==2004) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2004")
my.plots6[[4]]<-gc_mun %>% filter(anio==2009) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2009")
my.plots6[[5]]<-gc_mun %>% filter(anio==2014) %>% ggplot(aes(g)) + geom_histogram(binwidth = .01) + labs(x="Gamma",y="Number of industries") +
  scale_x_continuous(breaks=seq(-2,1,.1)) + ggtitle("2014")

multiplot(plotlist = my.plots6,cols = 3)
my.plots[[6]]<-recordPlot()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Geographic_Concetration_Hist.pdf",width = 10,onefile=TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

#  Compute table with share of industries falling in each category (Low, Medium, High)

gc_manuf_state %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_manuf_state
xtable(share_manuf_state)

gc_manuf_mun %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_manuf_mun
xtable(share_manuf_mun)

gc_serv_state %>% filter(Low_Med_High>=1) %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_serv_state
xtable(share_serv_state)

gc_serv_mun %>% mutate(Low_Med_High=if_else(g<.02,1,if_else(g<.05,2,3))) %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_serv_mun
xtable(share_serv_mun)

gc_state %>% filter(!is.na(Low_Med_High)) %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_state
xtable(share_state)

gc_mun %>% group_by(anio, Low_Med_High) %>% summarise_at(vars(g),funs(n())) %>% group_by(anio) %>% mutate(share=g*100/sum(g)) %>% 
  select(anio, Low_Med_High,share) %>% spread(key=anio,value=share)->share_mun
xtable(share_mun)

# Compute ratio between municipality and state, compute histograms and descriptives of ratio.

# Manufacturing

gc_manuf_mun_state<-gc_manuf_mun %>% select(-Description) %>%  rename_at(vars("G","x2","H","g","Low_Med_High"),funs(paste(.,"_Mun",sep=""))) %>% 
  left_join(gc_manuf_state,by=c("anio", "SCIAN4dig")) %>% mutate(ratio=g_Mun/g)

gc_manuf_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_94.txt")
gc_manuf_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_94.tex")

gc_manuf_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_99.txt")
gc_manuf_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_99.tex")

gc_manuf_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_04.txt")
gc_manuf_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_04.tex")

gc_manuf_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_09.txt")
gc_manuf_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_09.tex")

gc_manuf_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_14.txt")
gc_manuf_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_manuf_ratio_14.tex")

# Commerce and services

gc_serv_state<-gc_serv_state %>% mutate(g=as.numeric(g))

gc_serv_mun_state<-gc_serv_mun %>% filter(!is.infinite(g)) %>% mutate(Low_Med_High=if_else(g<.02,1,if_else(g<.05,2,3)))%>% rename_at(vars("G","x2","H","g","Low_Med_High"),funs(paste(.,"_Mun",sep=""))) %>% 
  left_join(gc_serv_state,by=c("anio", "SCIAN4dig")) %>% mutate(ratio=g_Mun/g)

gc_serv_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_94.txt")
gc_serv_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_94.tex")

gc_serv_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_99.txt")
gc_serv_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_99.tex")

gc_serv_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_04.txt")
gc_serv_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_04.tex")

gc_serv_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_09.txt")
gc_serv_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_09.tex")

gc_serv_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_14.txt")
gc_serv_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_serv_ratio_14.tex")

# Together

gc_mun_state<-gc_serv_mun_state %>% mutate(Low_Med_High=as.numeric(Low_Med_High)) %>% bind_rows(gc_manuf_mun_state) %>% select(-Description)

gc_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_94.txt")
gc_mun_state %>% filter(anio==1994) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1994", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_94.tex")

gc_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_99.txt")
gc_mun_state %>% filter(anio==1999) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 1999", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_99.tex")

gc_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_04.txt")
gc_mun_state %>% filter(anio==2004) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2004", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_04.tex")

gc_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_09.txt")
gc_mun_state %>% filter(anio==2009) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2009", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_09.tex")

gc_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(type="text",title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_14.txt")
gc_mun_state %>% filter(anio==2014) %>% select(-c(anio,SCIAN4dig,Low_Med_High_Mun,Low_Med_High)) %>% stargazer(title = "Geographic concentration 2014", out="C:/Users/pc Luis/Documents/World Bank/Resultados/Geographic_concentration_ratio_14.tex")


# Histograms

my.plotss<-list()

# Manufacturing

my.plots7<-list()

my.plots7[[1]]<-gc_manuf_mun_state %>% filter(anio==1994) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("1994")
my.plots7[[2]]<-gc_manuf_mun_state %>% filter(anio==1999) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("1999")
my.plots7[[3]]<-gc_manuf_mun_state %>% filter(anio==2004,ratio>-750) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1)) + ggtitle("2004")
my.plots7[[4]]<-gc_manuf_mun_state %>% filter(anio==2009) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("2009")
my.plots7[[5]]<-gc_manuf_mun_state %>% filter(anio==2014) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1)) + ggtitle("2014")

multiplot(plotlist = my.plots7,cols = 3)
my.plotss[[1]]<-recordPlot()

# Services

my.plots8<-list()

my.plots8[[1]]<-gc_serv_mun_state %>% filter(anio==1994) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries") + ggtitle("1994")
my.plots8[[2]]<-gc_serv_mun_state %>% filter(anio==1999) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries") + ggtitle("1999")
my.plots8[[3]]<-gc_serv_mun_state %>% filter(anio==2004,ratio<125) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries") + ggtitle("2004")
my.plots8[[4]]<-gc_serv_mun_state %>% filter(anio==2009) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries") + ggtitle("2009")
my.plots8[[5]]<-gc_serv_mun_state %>% filter(anio==2014,ratio>-175) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries") + ggtitle("2014")

multiplot(plotlist = my.plots8,cols = 3)
my.plotss[[2]]<-recordPlot()

# Together

my.plots9<-list()

my.plots9[[1]]<-gc_mun_state %>% filter(anio==1994) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("1994")
my.plots9[[2]]<-gc_mun_state %>% filter(anio==1999) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("1999")
my.plots9[[3]]<-gc_mun_state %>% filter(anio==2004,ratio>-750, ratio<125) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1)) + ggtitle("2004")
my.plots9[[4]]<-gc_mun_state %>% filter(anio==2009) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1))+ ggtitle("2009")
my.plots9[[5]]<-gc_mun_state %>% filter(anio==2014,ratio>-175) %>% ggplot(aes(ratio)) + geom_histogram() + labs(x="Ratio",y="Number of industries")+
  scale_x_continuous(breaks=seq(-100,100,1)) + ggtitle("2014")

multiplot(plotlist = my.plots9,cols = 3)
my.plotss[[3]]<-recordPlot()


pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Geographic_Concetration_Hist_ratios.pdf",width = 10,onefile=TRUE)
for (my.plot in my.plotss) {
  replayPlot(my.plot)
}
graphics.off()


# Now we are gonna need to compute a heat map for the 10 top industrie and the bottom 10 industries, it has to be done a map per industry
# so at the end we are gonna 20 different maps. This can ve done with q gis, but I willl work the data in R and the use it in Qgis.

# First we will have to get the file with the number of workers per year-industry.

Emp_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Emp_mun_4dig.csv")
Emp_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Emp_state_4dig.csv")

# Top 10 & bottom 10 industries to merge with employment data

manuf_mun_columns<-gc_manuf_mun %>% group_by(anio) %>% mutate(rank=data.table::frank(-g,ties.method = "dense"), max=max(rank,na.rm=TRUE)) %>% filter(rank<=10 | rank>max-10) %>%
  arrange(anio,rank) %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% ungroup() %>% select(anio_SCIAN4dig) %>% t() %>% as.vector()

manuf_state_columns<-gc_manuf_state %>% group_by(anio) %>% mutate(rank=data.table::frank(-g,ties.method = "dense"), max=max(rank,na.rm=TRUE)) %>% filter(rank<=10 | rank>max-10) %>%
  arrange(anio,rank) %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% ungroup() %>% select(anio_SCIAN4dig) %>% t() %>% as.vector()

serv_mun_columns<-gc_serv_mun %>% group_by(anio) %>% mutate(rank=data.table::frank(-g,ties.method = "dense"), max=max(rank,na.rm=TRUE)) %>% filter(rank<=10 | rank>max-10) %>%
  arrange(anio,rank) %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% ungroup() %>% select(anio_SCIAN4dig) %>% t() %>% as.vector()

serv_state_columns<-gc_serv_state %>% group_by(anio) %>% mutate(rank=data.table::frank(-g,ties.method = "dense"), max=max(rank,na.rm=TRUE)) %>% filter(rank<=10 | rank>max-10) %>%
  arrange(anio,rank) %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% ungroup() %>% select(anio_SCIAN4dig) %>% t() %>% as.vector()

Emp_mun %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% select(anio_SCIAN4dig,e03,e04,sum) %>%
  spread(key=anio_SCIAN4dig,value=sum) %>% mutate_all(~replace(.,is.na(.),0)) %>% as.tbl()->Emp_mun_maps

Emp_state %>% mutate(anio_SCIAN4dig=paste(anio,SCIAN4dig,sep="_")) %>% select(anio_SCIAN4dig,e03,sum) %>%
  spread(key=anio_SCIAN4dig,value=sum) %>% mutate_all(~replace(.,is.na(.),0)) %>% as.tbl()->Emp_state_maps

# Now we have to select the columns, using top and bottom industries we already computed./ Make sure I have all the municipalities in the data
# Otherwise we will have missings in rhe map.

Merge_qgis_mun<-Emp_mun_maps %>% select(e03,e04,one_of(c(manuf_mun_columns,serv_mun_columns))) %>% mutate(e03=if_else(e03<10,paste("0",as.character(e03),sep=""),as.character(e03)),
  e04=if_else(e04<10,paste("00",as.character(e04),sep=""),if_else(e04<100,paste("0",as.character(e04),sep=""),as.character(e04)))) %>%  
  mutate(CVEGEO=paste(e03,e04,sep="")) %>% select(CVEGEO,everything()) %>% select(-c(e03,e04))

data.table::fwrite(Merge_qgis_mun,"C:/Users/pc Luis/Documents/World Bank/Resultados/Mun_Emp_top_bottom10.txt")

Merge_qgis_state<-Emp_state_maps %>% select(e03,one_of(manuf_state_columns),one_of(serv_state_columns)) %>% mutate(e03=if_else(e03<10,paste("0",as.character(e03),sep=""),as.character(e03))) %>% 
 rename(CVE_ENT=e03)

data.table::fwrite(Merge_qgis_state,"C:/Users/pc Luis/Documents/World Bank/Resultados/State_Emp_top_bottom10.txt")

# Ratio for top and bottom industries for both sate level and municipality level, anf for manufacturing and Services.

gc_manuf_mun_state %>% group_by(anio) %>% mutate(rank=data.table::frank(-g_Mun,ties.method = "dense"), max=max(rank,na.rm=TRUE)) %>% filter(rank<=10 | rank>max-10) %>%
  arrange(anio,rank) %>% select(anio, SCIAN4dig,rank,ratio)
