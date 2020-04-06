library(xlsx)
library(dplyr)
library(tidyr)

# Corrección de Variables duplicadas en la base final.
Base %>% rowwise() %>% mutate(k010a_1=if_else(anio<2014,sum(k010a,k100a,na.rm=TRUE),k010a),m030a_1=if_else(anio<2014,sum(m030a,m310a,na.rm=TRUE),m030a),m010a_1=if_else(anio<2014,sum(m010a,m100a,na.rm=TRUE),m010a),k040a_1=if_else(anio<2014,sum(k040a,k411a,na.rm=TRUE),k040a),k041a_1=if_else(anio<2014,sum(k041a,k412a,na.rm=TRUE),k041a)) %>% select(-c(k010a,k100a,m030a,m310a,m010a,m100a,k040a,k411a,k041a,k412a) %>% rename(k010a=k010a_1,m030a=m030a_1,m010a=m010a_1,k040a=k040a_1,k041a=k041a_1))->Base

# We are gonna keep variables just for manufacturing , services and commerce.

Base %>% filter(SCIANdig!="11"& SCIAN2gig!="21"& SCIAN2dig!="22" & SCIAN2dig!="23") %>% mutate(manufactura=if_else(SCIAN2dig=="30",1,0))->Base

# We have to compute descriptive statistics over real variables, so we have to use deflators

deflactores<-haven::read_dta("Base_Deflactores_Censos.dta")
deflactores %>% select(-c(SCIAN2dig ,SCIAN3dig, SCIAN4dig, SCIAN5dig, SCIAN6dig))->deflactores

Base %>% left_join(deflactores,by=c("e17"="SCIAN"))->Base_1

# Compute real variables

Base %>% mutate()


# Descriptive statistics by census of the variables discussed.

#"e03","e04","e17","g111a","h000a","h000d","h001a","h001d","h010a","h010d","h020a","h020d","j000a","k010a","k020a","k030a","k040a","k041a","k100a","k310a","k411a","k412a","k421a","k431a","k810a","k910a","m010a","m030a","m100a","m310a","p030c","q010a","q020a","q030a","q400a","q900a","d311","d312","d313","d314","q010c","q020c","q030c","q400c","q900c","p100c","p200c","p230c","p900c","p330c",""


# With the following line you get all the statistics

sample %>% group_by(year) %>%  summarise_each(funs(mean(.,na.rm = TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),-plantid) %>% gather(key="Variable",value="Value",-year) %>% separate(col=Variable,into=c("Variable","stat"),sep="_")  %>% spread(key=Variable,value=Value)%>% arrange(year)->tabstat

Base %>% group_by(year) %>%  summarise_each(funs(Avg=mean(.,na.rm = TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),h001a,h001d,j000a,k010a,k020a,k030a,k040a,k041a,k810a,k910a,m010a,m030a,p030c,q010a, q020a,q030a,q400a,q900a,q010c,q020c,q030c,q400c,q900c) %>% gather(key="Variable",value="Value",-year) %>% separate(col=Variable,into=c("Variable","stat"),sep="_")  %>% spread(key=Variable,value=Value)%>% arrange(year)

#xlsx::write.xlsx(tabstat,file="C:/Users/pc Luis/Documents/World Bank/R/Prueba Estadísticas.xlsx", sheetName="Descriptive Statistics by Census", row.names=FALSE)

# Create database of changes by two; 1999-1994, 2003-1999 and so on.

Base %>% complete(CDE, year) %>% arrange(CDE, year) %>% group_by(CDE) %>% mutate_each(funs(delta=.-lag(.)),h001a,h001d,j000a,k010a,k020a,k030a,k040a,k041a,k810a,k910a,m010a,m030a,p030c,q010a, q020a,q030a,q400a,q900a,q010c,q020c,q030c,q400c,q900c) %>% filter(!is.na(ends_with("delta")))->Base_cambios 

# Descriptive Statistics of the changes in Stata format.

Base_cambios %>% group_by(year) %>%  summarise_each(funs(Avg=mean(.,na.rm = TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),ends_with("delta")) %>% gather(key="Variable",value="Value",-year) %>% separate(col=Variable,into=c("Variable","stat"),sep="_")  %>% spread(key=Variable,value=Value)%>% arrange(year)

#-----------------------------#

# We want to do the same but with the changes in the variables in the consecutive years.
Base %

# head(sample %>% mutate_all(~if_else(is.na(.x),0,.x)))

# Index of Total employment & value added / basis=1994

Base %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE),h001a, va)) %>% mutate(Index_basis=h000) %>% gather(key="",value="",)

sample %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),lnva,lnb,lnw) %>% arrange(year) %>%  mutate(Index_lnva=lnva*100/first(lnva),Index_lnb=lnb*100/first(lnb),Index_lnw=lnw*100/first(lnw)) %>% select(year,starts_with("Index")) %>% gather(key="Variable",value="Index",-year)%>% ggplot(aes(year,Index))+geom_line(aes(color=Variable))+ggtitle("Total Index")

panel_2 %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h010a) %>% arrange(year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h010a*100/first(h010a))%>% select(year,starts_with("Index")) %>% gather(key="Variable",value="Index",-year) %>% ggplot(aes(year,Index))+geom_line(aes(color=Variable))+ggtitle("Total Index")


# Now, we divide by Manufacturing & Others

panel_2 %>% group_by(Sector,year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h010a) %>% arrange(Sector,year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h010a*100/first(h010a))%>% select(Sector,year,starts_with("Index")) %>% gather(key="Variable",value="Index",-c(Sector,year))->Base_Indices_Manuf
Base_Indices_Manuf%>%filter(Sector==0) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle("Commerce & Services Index")->Services
Base_Indices_Manuf%>%filter(Sector==1) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle("Manufacturing Index" )->Manuf

multiplot(plotlist = list(Services,Manuf))

# In Sectors
panel_2 %>% group_by(SCIAN2dig,year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),a131a,h010a) %>% arrange(SCIAN2dig,year) %>% mutate(Index_va=a131a*100/first(a131a),Index_Emp=h010a*100/first(h010a))%>% select(SCIAN2dig,year,starts_with("Index")) %>% gather(key="Variable",value="Index",-c(SCIAN2dig,year))->Base_Indices_Sector

clase<-unique(Base_Indices_Sector$SCIAN2dig)

plot_list=list()

for (i in 1:(length(clase)-1)) {
  q1<-Base_Indices_Sector %>% filter(SCIAN2dig==clase[i]) %>%  ggplot(aes(year,Index))+geom_point(aes(color=Variable))+ggtitle(paste(clase[i],"Sector Index",sep=" "))
  plot_list[[i]]<-q1
}
multiplot(plotlist = plot_list,cols=cols = )

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




