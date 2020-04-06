library(dplyr)
library(tidyr)
library(ggplot2)

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


# Index of Employment and value added, basis 1994. Plots divided by Mabufacturing vs Services & Commerce and by Sector(SCIAN2dig)

Base %>% mutate(SCIAN2dig=substr(e17,1,2)) %>% mutate(SCIAN2dig=if_else((SCIAN2dig=="31" |SCIAN2dig=="32" |SCIAN2dig=="33"),"30",SCIAN2dig)) %>% mutate(Sector=if_else(SCIAN2dig=="30",1,0))->Base

# Total 

Base %>% rename(year=anio) %>%  group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h001a) %>% arrange(year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h001a*100/first(h001a))%>% select(year,starts_with("Index")) %>% gather(key="Variable",value="Index",-year) %>% ggplot(aes(year,Index))+geom_line(aes(color=Variable))+ggtitle("Total Index")
ggsave("TotalIndex.pdf")

# Manufacturing vs Services & commerce

Base %>% rename(year=anio) %>% group_by(Sector,year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h001a) %>% arrange(Sector,year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h001a*100/first(h001a))%>% select(Sector,year,starts_with("Index")) %>% gather(key="Variable",value="Index",-c(Sector,year))->Base_Indices_Manuf
Base_Indices_Manuf%>%filter(Sector==0) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle("Commerce & Services Index")->Services
Base_Indices_Manuf%>%filter(Sector==1) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle("Manufacturing Index" )->Manuf

multiplot(plotlist = list(Services,Manuf))
ggsave("ManufvsServicesIndices.pdf")

# In Sectors
Base %>% rename(year=anio) %>%  group_by(SCIAN2dig,year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h001a) %>% arrange(SCIAN2dig,year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h001a*100/first(h001a))%>% select(SCIAN2dig,year,starts_with("Index")) %>% gather(key="Variable",value="Index",-c(SCIAN2dig,year))->Base_Indices_Sector

clase<-unique(Base_Indices_Sector$SCIAN2dig)

plot_list=list()

for (i in 1:length(clase)) {
  q1<-Base_Indices_Sector %>% filter(SCIAN2dig==clase[i]) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle(paste(clase[i],"Sector Index",sep=" "))
  plot_list[[i]]<-q1
}
multiplot(plotlist = plot_list,cols=cols = 6)
ggsave("SectorsIndices.pdf")
