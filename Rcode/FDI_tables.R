# Script to built comparative tables in excel against three measures of FDI

require(dplyr)
require(tidyr)

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

Base_fdi_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_states.csv")

Base_fdi_state<-Base_fdi_state %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_Fo=d311_suma*100/N) 

Base_fdi_state %>% select(anio,e03,emp_share_fo) %>% spread(key=anio,value=emp_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_emp_share_FDI.csv")

Base_fdi_state %>% select(anio,e03,sales_share_fo) %>% spread(key=anio,value=sales_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_sales_share_FDI.csv")

Base_fdi_state %>% select(anio,e03,N_share_Fo) %>% spread(key=anio,value=N_share_Fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_N_share_FDI.csv")

# Sectors FDI (2dig)

Base_fdi_sectors<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_sector_size.csv")

Base_fdi_sectors<-Base_fdi_sectors %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_Fo=d311_suma*100/N) 

Base_fdi_sectors %>% select(anio,SCIAN2dig,emp_share_fo) %>% spread(key=anio,value=emp_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector_emp_share_FDI.csv")

Base_fdi_sectors %>% select(anio,SCIAN2dig,sales_share_fo) %>% spread(key=anio,value=sales_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector_sales_share_FDI.csv")

Base_fdi_sectors %>% select(anio,SCIAN2dig,N_share_Fo) %>% spread(key=anio,value=N_share_Fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector_N_share_FDI.csv")

# Sectors FDI (3dig)

Base_fdi_sectors3dig<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_sector_3dig_size.csv")

Base_fdi_sectors3dig<-Base_fdi_sectors3dig %>% select(anio,SCIAN3dig,starts_with("size"))

Base_fdi_sectors3dig %>% select(anio,SCIAN3dig,size_Fo_w) %>% spread(key=anio,value=size_Fo_w) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector3dig_emp_share_FDI.csv")

Base_fdi_sectors3dig %>% select(anio,SCIAN3dig,size_Fo_s) %>% spread(key=anio,value=size_Fo_s) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector3dig_sales_share_FDI.csv")

Base_fdi_sectors3dig %>% select(anio,SCIAN3dig,size_Fo) %>% spread(key=anio,value=size_Fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/sector3dig_N_share_FDI.csv")

# Municipality FDI

Base_fdi_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_mun.csv")

Base_fdi_mun<-Base_fdi_mun %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_fo=d311_suma*100/N) 

Base_fdi_mun %>% select(anio,e03,e04,emp_share_fo) %>% spread(key=anio,value=emp_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_emp_share_FDI.csv")

Base_fdi_mun %>% select(anio,e03,e04,sales_share_fo) %>% spread(key=anio,value=sales_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_sales_share_FDI.csv")

Base_fdi_mun %>% select(anio,e03,e04,N_share_fo) %>% spread(key=anio,value=,N_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_N_share_FDI.csv")

# Municipality-Sector FDI

Base_fdi_mun_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_mun_size.csv")

Base_fdi_mun_sector<-Base_fdi_mun_sector %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_fo=d311_suma*100/N) 

Base_fdi_mun_sector %>% select(anio,e03,e04,SCIAN2dig,emp_share_fo) %>% spread(key=anio,value=emp_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_sector_emp_share_FDI.csv")

Base_fdi_mun_sector %>% select(anio,e03,e04,SCIAN2dig,sales_share_fo) %>% spread(key=anio,value=sales_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_sector_sales_share_FDI.csv")

Base_fdi_mun_sector %>% select(anio,e03,e04,SCIAN2dig,N_share_fo) %>% spread(key=anio,value=,N_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_sector_N_share_FDI.csv")

# State-Sector FDI

Base_fdi_state_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_state_size.csv")

Base_fdi_state_sector<-Base_fdi_state_sector %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_fo=d311_suma*100/N) 

Base_fdi_state_sector %>% select(anio,e03,SCIAN2dig,emp_share_fo) %>% spread(key=anio,value=emp_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_sector_emp_share_FDI.csv")

Base_fdi_state_sector %>% select(anio,e03,SCIAN2dig,sales_share_fo) %>% spread(key=anio,value=sales_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_sector_sales_share_FDI.csv")

Base_fdi_state_sector %>% select(anio,e03,SCIAN2dig,N_share_fo) %>% spread(key=anio,value=,N_share_fo) %>% 
  data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_sector_N_share_FDI.csv")

# Data for maps at Municipality-Sector and State-Sector

Base_fdi_mun_sector %>% select(anio,e03,e04,SCIAN2dig,emp_share_fo) %>% unite(anio_sector,anio,SCIAN2dig,sep="_") %>%
  spread(key=anio_sector,value=emp_share_fo) %>% data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/mun_sector_emp_share_FDI_maps.csv")

Base_fdi_state_sector %>% select(anio,e03,SCIAN2dig,h001a_Fo_suma,h001a_suma,emp_share_fo) %>% mutate(SCIAN2dig=replace(SCIAN2dig,which(SCIAN2dig==48 | SCIAN2dig==49),40)) %>%
  group_by(anio,e03,SCIAN2dig) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma) %>% select(anio,e03,SCIAN2dig,emp_share_fo) %>% 
  unite(anio_sector,anio,SCIAN2dig,sep="_") %>% spread(key=anio_sector,value=emp_share_fo) %>% data.table::fwrite("C:/Users/pc Luis/Documents/World Bank/Resultados/state_sector_emp_share_FDI_maps.csv")

# Density/ Histograms plots for different agregations
# Make sure all the plots have the same scale

# Municipality

  # Histogram
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Hist_FDI_mun.jpg")
  Base_fdi_mun %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,fill=anio))+geom_histogram(binwidth=5,position=position_dodge(width = 4))+scale_x_continuous(breaks=seq(0,85,by=5))+labs(x="FDI",fill="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")+coord_flip()
  graphics.off()
  
  # Density
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Density_FDI_mun.jpg")
  Base_fdi_mun %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,color=anio))+geom_density(size=1)+scale_x_continuous(breaks=seq(0,85,by=5))+labs(x="FDI",color="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  graphics.off()

# Municipality/Sector
  
  # Histogram
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Hist_FDI_mun_sector.jpg")
  Base_fdi_mun_sector %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,fill=anio))+geom_histogram(binwidth=5,position=position_dodge(width = 4))+scale_x_continuous(breaks=seq(0,100,by=5))+labs(x="FDI",fill="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")+coord_flip()
  graphics.off()
  
  # Density
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Density_FDI_mun_sector.jpg")
  Base_fdi_mun_sector %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,color=anio))+geom_density(size=1)+scale_x_continuous(breaks=seq(0,100,by=5))+labs(x="FDI",color="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  graphics.off()
  
# State
  
  # Histogram
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Hist_FDI_state.jpg")
  Base_fdi_state %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,fill=anio))+geom_histogram(binwidth=5,position=position_dodge(width = 4))+scale_x_continuous(breaks=seq(0,85,by=5))+labs(x="FDI",fill="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")+coord_flip()
  graphics.off()
  
  # Density
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Density_FDI_state.jpg")
  Base_fdi_state %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,color=anio))+geom_density(size=1)+scale_x_continuous(breaks=seq(0,85,by=5))+labs(x="FDI",color="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  graphics.off()
  
# State/Sector
  
  # Histogram
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Hist_FDI_state_sector.jpg")
  Base_fdi_state_sector %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,fill=anio))+geom_histogram(binwidth=5,position=position_dodge(width = 4))+scale_x_continuous(breaks=seq(0,100,by=5))+labs(x="FDI",fill="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")+coord_flip()
  graphics.off()
  
  # Density
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Density_FDI_state_sector.jpg")
  Base_fdi_state_sector %>% select(anio,emp_share_fo) %>% mutate(anio=as.factor(anio)) %>% ggplot(aes(emp_share_fo,color=anio))+geom_density(size=1)+scale_x_continuous(breaks=seq(0,100,by=5))+labs(x="FDI",color="year")+theme(legend.position = "bottom")+labs(caption = "Source: Author calculations with data from Mexico큦 Economic Censuses 1994-2014, INEGI.")
  graphics.off()

# Sector/ 2 dig/ 3 dig
  
  sectors<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Lista de sectores.csv") %>% select("2012 SCIAN",name) %>% rename(SCIAN2dig="2012 SCIAN") %>% 
    mutate(SCIAN2dig=as.integer(SCIAN2dig))
  Base_fdi_sectors_1<-Base_fdi_sectors %>% left_join(sectors,by="SCIAN2dig") %>% unite(Sector,SCIAN2dig,name,sep=" ")
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector2dig_94.jpg")
  Base_fdi_sectors_1 %>% filter(anio==1994) %>% ggplot(aes(x=as.factor(Sector),y=emp_share_fo))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector2dig_99.jpg")
  Base_fdi_sectors_1 %>% filter(anio==1999) %>% ggplot(aes(x=as.factor(Sector),y=emp_share_fo))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector2dig_04.jpg")
  Base_fdi_sectors_1 %>% filter(anio==2004) %>% ggplot(aes(x=as.factor(Sector),y=emp_share_fo))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector2dig_09.jpg")
  Base_fdi_sectors_1 %>% filter(anio==2009) %>% ggplot(aes(x=as.factor(Sector),y=emp_share_fo))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector2dig_14.jpg")
  Base_fdi_sectors_1 %>% filter(anio==2014) %>% ggplot(aes(x=as.factor(Sector),y=emp_share_fo))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  # Sectors/ 3 digit
  sectores<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/sectores3dig.csv")
  Base_fdi_sectors3dig_1<-Base_fdi_sectors3dig %>% left_join(sectores,by="SCIAN3dig") %>% unite(Sector,SCIAN3dig,name,sep = " ")
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector3dig_94.jpg")
  Base_fdi_sectors3dig_1 %>% filter(anio==1994) %>% ggplot(aes(x=as.factor(Sector),y=size_Fo_w))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors3dig_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector3dig_99.jpg")
  Base_fdi_sectors3dig_1 %>% filter(anio==1999) %>% ggplot(aes(x=as.factor(Sector),y=size_Fo_w))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors3dig_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector3dig_04.jpg")
  Base_fdi_sectors3dig_1 %>% filter(anio==2004) %>% ggplot(aes(x=as.factor(Sector),y=size_Fo_w))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors3dig_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector3dig_09.jpg")
  Base_fdi_sectors3dig_1 %>% filter(anio==2009) %>% ggplot(aes(x=as.factor(Sector),y=size_Fo_w))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors3dig_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sector3dig_14.jpg")
  Base_fdi_sectors3dig_1 %>% filter(anio==2014) %>% ggplot(aes(x=as.factor(Sector),y=size_Fo_w))+
    geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5),axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()+scale_x_discrete(limits=rev(levels(as.factor(Base_fdi_sectors3dig_1$Sector))))+scale_y_continuous(breaks=seq(0,100,by=5))
  graphics.off()
  
  