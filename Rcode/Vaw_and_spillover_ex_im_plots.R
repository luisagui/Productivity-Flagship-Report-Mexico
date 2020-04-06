# Script to plot tdifferent measures of Vaw at municipality or state level with differetn variables at those levels.

Vaw_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_Mun.csv")
Vaw_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_State.csv")

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


# Electoral competition.
elec_states<-data.table::fread()
expan92_16<-data.table::fread()

elec_mun_state<-expan92_16 %>% left_join(elec_states,by=c("year_start"="anio","CVE_ENT"="e03")) %>%  filter(year_start=="1993"| year_start=="1998"| year_start=="2003" | year_start=="2008" | year_start=="2013") %>%
  select(year_start,CVE_ENT,CVE_MUN,Nombre,party_mixed,party) %>% mutate(Oposition=if_else(party_mixed!=party,1,0)) %>%
  group_by(year_start,CVE_ENT,Nombre) %>% summarise_at(vars(Oposition),funs(sum(.,na.rm=TRUE),N=n()))


Vaw_elec_2plot<-Vaw_state %>% mutate(anio=anio-1) %>% left_join(elec_mun_state,by=c("anio"="year_start","e03"="CVE_ENT")) %>% mutate(share_op=sum/N.y)

my.plots<-list()

my.plots[[1]]<-Vaw_elec_2plot %>% arrange(anio,e03) %>% filter(anio==1993 | anio==2013) %>% group_by(e03) %>% mutate_at(vars(P5,avg,Vaw,P95,share_op),funs(D=.-lag(.))) %>% 
  select(anio,e03,Nombre, ends_with("_D")) %>% filter(anio==2013) %>%
  ggplot(aes(share_op_D,P5_D))+geom_point()+geom_smooth(method = "lm",formula=y~x)+ggtitle("P5")+labs(y="",x="Delta(Opposition share)")

my.plots[[2]]<-Vaw_elec_2plot %>% arrange(anio,e03) %>% filter(anio==1993 | anio==2013) %>% group_by(e03) %>% mutate_at(vars(P5,avg,Vaw,P95,share_op),funs(D=.-lag(.))) %>% 
  select(anio,e03,Nombre, ends_with("_D")) %>% filter(anio==2013) %>%
  ggplot(aes(share_op_D,Vaw_D))+geom_point()+geom_smooth(method = "lm",formula=y~x)+ggtitle("Weighted mean")+labs(y="",x="Delta(Opposition share)")

my.plots[[3]]<-Vaw_elec_2plot %>% arrange(anio,e03) %>% filter(anio==1993 | anio==2013) %>% group_by(e03) %>% mutate_at(vars(P5,avg,Vaw,P95,share_op),funs(D=.-lag(.))) %>% 
  select(anio,e03,Nombre, ends_with("_D")) %>%  filter(anio==2013) %>%
  ggplot(aes(share_op_D,avg_D))+geom_point()+geom_smooth(method = "lm",formula=y~x)+ggtitle("Unweighted mean")+labs(y="",x="Delta(Opposition share)")

my.plots[[4]]<-Vaw_elec_2plot %>% arrange(anio,e03) %>% filter(anio==1993 | anio==2013) %>% group_by(e03) %>% mutate_at(vars(P5,avg,Vaw,P95,share_op),funs(D=.-lag(.))) %>% 
  select(anio,e03,Nombre, ends_with("_D")) %>% filter(anio==2013) %>%
  ggplot(aes(share_op_D,P95_D))+geom_point()+geom_smooth(method = "lm",formula=y~x)+ggtitle("P95")+labs(y="",x="Delta(Opposition share)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Competencia_electoral.jpg")
multiplot(plotlist=my.plots,cols=2)
graphics.off()

# Now we can plot Spillovers measures, we want to compare Spillovers computed for imports , exports and FDI

#spi<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Spillovers.csv") # This data is the one I requested from the lab, it is not vomplete.
spi_ex_im<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Spillover_Ex_Im.csv")

spi_ex_im<-spi_ex_im %>% mutate(Exports=if_else(Exports==1,"Ex","Im")) %>% gather(key="Variable",value=value,-c(anio,SCIAN3dig,Exports)) %>% 
  unite(key,Variable,Exports) %>% spread(key="key",value=value) %>% as.tbl()

spi<-spi %>% separate(Sector,c("SCIAN3dig","Name"),sep=" ")
spi_all<-spi %>% select(-Name) %>% mutate(SCIAN3dig=as.numeric(SCIAN3dig)) %>% left_join(spi_ex_im,by=c("anio","SCIAN3dig"))

# Plots

my.plots2<-list()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Horizontal_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Horizontal_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Horizontal_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Horizontal_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Backward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Backward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Backward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Backward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Forward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Forward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Forward_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Forward_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

# Employment measures

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_emp_Im_vs_Horizontal_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_emp_Im_vs_Horizontal_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_emp_Im,Horizontal_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_emp_Im_vs_Horizontal_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_emp_Im_vs_Backward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_emp_Im_vs_Backward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_emp_Im,Backward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_emp_Im_vs_Backward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_emp_Im_vs_Forward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_emp_Im_vs_Forward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_emp_Im,Forward_emp_Ex))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_emp_Im_vs_Forward_emp_Ex.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

# FDI vs Imports-Exports

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Horizontal.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Horizontal.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Horizontal.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Backward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Backward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Backward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Backward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Im_vs_Forward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Im_vs_Forward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Im,Forward))+geom_point()+geom_smooth()+ggtitle("2014")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Im_vs_Forward.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()

my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Ex_vs_Horizontal.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()
                                                          
my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Ex_vs_Horizontal.jpg")
multiplot(plotlist = my.plots2,cols=3)
graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Ex,Horizontal))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Ex_vs_Horizontal.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Ex_vs_Backward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Ex_vs_Backward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Ex,Backward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Ex_vs_Backward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Horizontal_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Horizontal_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Horizontal_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Horizontal_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Horizontal_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Horizontal_Ex_vs_Forward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Backward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Backward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Backward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Backward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Backward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Backward_Ex_vs_Forward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
                                                          
                                                          my.plots2[[1]]<-spi_all %>% filter(anio==1994) %>% ggplot(aes(Forward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1994")
                                                          my.plots2[[2]]<-spi_all %>% filter(anio==1999) %>% ggplot(aes(Forward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("1999")
                                                          my.plots2[[3]]<-spi_all %>% filter(anio==2004) %>% ggplot(aes(Forward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2004")
                                                          my.plots2[[4]]<-spi_all %>% filter(anio==2009) %>% ggplot(aes(Forward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2009")
                                                          my.plots2[[5]]<-spi_all %>% filter(anio==2014) %>% ggplot(aes(Forward_Ex,Forward))+geom_point()+geom_smooth()+ggtitle("2014")
                                                          
                                                          jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Forward_Ex_vs_Forward.jpg")
                                                          multiplot(plotlist = my.plots2,cols=3)
                                                          graphics.off()
