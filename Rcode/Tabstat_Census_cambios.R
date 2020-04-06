library(dplyr)
library(tidyr)
library(ggplot2)

# tidyr::complete() sirve para completar el panel.

sample<-haven::read_dta("C:/Users/pc Luis/Documents/World Bank/Stata/Datos/sample.dta")
head(sample)
# Indices para value added y total employment, total , sector y SCIAN2

sample %>% group_by(year) %>% summarise_each(funs(sum(.,na.rm=TRUE)),lnva,lnb, lnw, lnk,lnm) %>% mutate(lnva_i=lnva/first(lnva)*100,lnb_i=lnb/first(lnb)*100) %>% ggplot(aes(year,value))+geom_line(aes(year,lnb_i))+geom_line(aes(year,lnva_i),col="blue")

# Estadísticas para los cambios de un año a otro, primero hay que construir un panel de prueba

archivos<-list.files(path = "C:/Users/pc Luis/Documents/World Bank/Insumos", pattern = ".dta")
bases<-lapply(archivos,function(x) haven::read_dta(paste("C:/Users/pc Luis/Documents/World Bank/Insumos/",x,sep="")))
bases[[1]] %>% mutate(year=1993, id=1:10)->bases[[1]]                                             
bases[[2]] %>% mutate(year=1998, id=2:11)->bases[[2]]
bases[[3]] %>% mutate(year=2003, id=3:12)->bases[[3]]
bases[[4]] %>% mutate(year=2008, id=4:22)->bases[[4]]
bases[[5]] %>% mutate(year=2013, id=1:10)->bases[[5]]
panel_1<-data.table::rbindlist(bases[-c(1,2)], fill=TRUE)
panel_1 %>% select(id,year, e17, a131a, h010a) %>% mutate(SCIAN2dig=substr(as.character(e17),1,2))->panel_2
panel_2 %>% mutate(a131a=as.numeric(a131a))->panel_2

# Con este tengo las estadísticas de lo que quiero

panel_2 %>% tidyr::complete(id ,year) %>%  arrange (id ,year) %>% group_by(id) %>% mutate_each(funs(delta=.-lag(.)),h010a,a131a) %>% select(id, year,ends_with("delta")) %>% na.omit %>% group_by(year)%>% summarise_each(funs(Avg=mean(.,na.rm = TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),ends_with("delta")) %>% gather(key="Variable", value="Value",-year) %>% separate(Variable,c("Variable",NA,"Stat"),sep="_") %>% spread(key=Variable,value=Value) %>% arrange(year)

# Create database of changes by two; 1999-1994, 2003-1999 and so on.

Base %>% complete(CDE, year) %>% arrange(CDE, year) %>% group_by(CDE) %>% mutate_each(funs(delta=.-lag(.)),h001a,h001d,j000a,k010a,k020a,k030a,k040a,k041a,k810a,k910a,m010a,m030a,p030c,q010a, q020a,q030a,q400a,q900a,q010c,q020c,q030c,q400c,q900c) %>% select(id, year,ends_with("delta")) %>% na.omit->Base_cambios 

# Descriptive Statistics of the changes in Stata format.

Base_cambios %>% na.omit %>% group_by(year)%>% summarise_each(funs(Avg=mean(.,na.rm = TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),ends_with("delta")) %>% gather(key="Variable", value="Value",-year) %>% separate(Variable,c("Variable",NA,"Stat"),sep="_") %>% spread(key=Variable,value=Value) %>% arrange(year)->tabstatdelta

write.xlsx(tabstatdelta,file="", sheetName="Delta Descriptive Statistics by Census",append=TRUE, row.names=FALSE)


#Base_cambios %>% group_by(year) %>% summarise_each(funs(mean(.,na.rm=TRUE)),ends_with("delta")) %>% mutate(stats="Avg")->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p1=quantile(.,.01,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p1")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p10=quantile(.,.1,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p10")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p25=quantile(.,.25,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p25")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p50=quantile(.,.5,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p50")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p75=quantile(.,.75,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p75")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p90=quantile(.,.9,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p90")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(p99=quantile(.,.99,na.rm=TRUE)),ends_with("delta")) %>%  mutate(stats="p99")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(n()),ends_with("delta")) %>%  mutate(stats="N")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1)->tabstatdelta
#Base_cambios %>% group_by(year) %>% summarise_each(funs(sum(is.na(.))),ends_with("delta")) %>%  mutate(stats="NA's")->tabstatdelta_1
#bind_rows(tabstatdelta,tabstatdelta_1) %>% arrange(year)->tabstatdelta
