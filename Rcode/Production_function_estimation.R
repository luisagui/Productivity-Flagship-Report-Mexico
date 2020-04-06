
# Script para estimar la función de producción utilizando el paquete " prodest".

# Se quiere estimar la función de producción a 6 dígitos dentro de manufacturas y a 2 dígitos entre sectores.
# Se quiere hacer la estimación utilizando:Ackerberg, D., Caves, K. and Frazer, G. (2015). (ACF)
# "Identification properties of recent production function estimators." Econometrica, 83(6), 2411-2451. 

library(prodest)
library(dplyr)

# Primero se leen solo las variables que se vayan a utilizar del panel, de otra forma es muy pesado trabajar con todas.
# Se necesitan variables de "Value added"(a131a), "Capital"(Activos fijos[q's]), "Personal ocupado"(h's) o "Salarios"(j's),
# distintas proxys para capital:"materiales"(k020a o k200a),"Materias primas"(k030a o k310),"Consumo decombustibles, lubricantes y energéticos"(k040a o k411a)
# ,"Consumo de energía eléctrica"(k041a o k412a).


var_y<-c("anio","CDE","e17","e03","a131a,","q000a","q010a","q020a","q030a","q400a","q900a","h000a","h000b","h000c","h000d","h001a","h001b","h001c","h001d","h010a","h010b","h010c","h010d","h020a","h020b","h020c","h020d","h101a","h101b","h101c","h101d","h203a","h203b","h203c","h203d","j000a","j010a","j020a","j100a","j200a","j203a","j300a","j400a","j500a","j600a")
var_job_flows<-c("anio","CDE","e17","e03","h000a","h000b","h000c","h000d","h001a","h001b","h001c","h001d","h010a","h010b","h010c","h010d","h020a","h020b","h020c","h020d","h101a","h101b","h101c","h101d","h203a","h203b","h203c","h203d")

var<-c("anio","CDE","e17","e03","a131a,","q000a","q010a","q020a","q030a","q400a","q900a","h000a","h000b","h000c","h000d","h001a","h001b","h001c","h001d","h010a","h010b","h010c","h010d","h020a","h020b","h020c","h020d","h101a","h101b","h101c","h101d","h203a","h203b","h203c","h203d","j000a","j010a","j020a","j100a","j200a","j203a","j300a","j400a","j500a","j600a","k020a","k200a","k030a","k310a","k040a","k411a","k041a","k412a")

Base<-fst:read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_censos/Panel94_09.fst",columns=var_y)
Base14<-fst:read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_censos/Temp_Insumo2014_1",columns=var_y)
Base<-data.table::rbindlist(list(Base,Base14),fill=TRUE)
rm(Base14)
Base %>% arrange(CDE,anio)->Base
# Los datos tienen el año en que se levanta el censo, pero los datos reportados son del año anterior.
# Por lo que hay que agregar la variable anio_datos, además hay que agregar los valores de clase de actividad a 2 y 6 dígitos.

Base %>% mutate(anio_datos=as.integer(anio)-1,SCIAN_2dig=as.double(substr(as.character(clase),1,2)),SCIAN_6dig=as.double(substr(as.character(clase),1,6)))->Base

# Se trabaja con variables reales, por lo cual se lee una base de deflactores.
# Esta base contiene deflactores mensuales de 1993-2015, hay que decidir si se van a utilizar los deflactores en un punto 
# específico del año(mitad, final) o un promedio anual.Esto tiene que decidirse antes de hacer el join, para evitar que 
# se multipliquen las observaciones por 12. Si no hay 

deflactores<-haven::read_dta("Z:/Procesamiento/Trabajo/Luis A/Panel_censos/Base_deflactores.dta")
deflactores %>% filter(year==1993 | year==1998 | year==2003 | year==2008 | year==2013 )->deflactores



# Dependiendo al nivel que se vaya a trabajar son los deflactores que se utilizan. Solo se
# tienen deflactores de capital para manufactura y a 4 dígitos, hay que buscar para los demás sectores.
# La deflacción de los activos fijos se hace a nivel de componentes y al final se agregan
# variables reales.

Base %>% left_join(deflactores,by=c(e17="SCIAN",anio_datos="year")) %>%mutate(q000aR=q000a/INPP) %>% mutate(lnva=log(),lnl=log(),lnk=log(),lnm=log())->Base

ACF.fit<-prodestACF(Base$lnva,Base$lnl,Base$lnk,Base$id,Base$anio)
Base %>% mutate(TFP=omega, lnY_hat=lnY-omega)->Base


c(clee,e01,e02, e03, e17,a131a,h000a,h001a,h010a,j010a,j000a,k020a,q000a,q010a,q020a,q030a,q400a,q900a)


insumo2014<-fst::read_fst("C:/Users/pc Luis/Documents/World Bank/Insumos/CE2014.fst",col=c("e03","clase","a131a","h000a","h001a","h010a","j010a","j000a","k020a","q000a","q010a","q020a","q030a","q400a","q900a"))

insumo2014<-fst::read_fst("C:/Users/pc Luis/Documents/World Bank/Insumos/CE2014.fst")

insumo2014 %>% mutate(SCIAN_2dig=substr(as.character(clase),1,2))->insumo2014

install.packages("DescTools")
insumo2014_prueba<-lapply(insumo2014, function(x) DescTools::Winsorize(insumo2014$x,minval=quantile(x,.01),maxval = quantile(x,.99)))

deflactores<-haven::read_dta("C:/Users/pc Luis/Documents/World Bank/Data producer prices/Base_Deflactores.dta")

deflactores %>% group_by(year, SCIAN6dig) %>% summarise_at(c("INPP","Electricidad","Combustibles1","Combustibles2","deflInvEq","deflInvStr","deprEq","deprStr","INPP_mp"),c(mean,last))->def6dig
View(def6dig)
