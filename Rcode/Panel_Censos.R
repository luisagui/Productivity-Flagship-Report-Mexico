#install.packages("tidyverse")
library(tidyverse)
library(haven)
# setwd("Z:/Procesamiento/Insumos/Proyecto 607/Censos Económicos")

# No hay memoria suficiente para guardar todos los censos en una lista, hay que
# hacer el proceso de leer la base junto con la de correspondencias y pegarle el indicador,
# Escribir una base en la carpeta de trabajo y eliminarla de la memoria, y así con cada base

setwd("~/RA_UCSC/Stata/Datos/Ejemplo_Censo04")
correspondencias<-as_tibble(read_dta("Z:/Procesamiento/Insumos/Proyecto 607/Censos Económicos/correspondencias_1994-2014.dta"))
correspondencias<-filter(correspondencias, d_1994==0 & d_1999==0 & d_2004==0 & d_2009==0 & d_2014==0)
correspondencias<-select(correspondencias_1,CDE,NIC_NOP_1994,NIC_NOP_1999,NIC_NOP_2004,NIC_NOP_2009,NIC_NOP_2014)
anio<-c("1994", "1999", "2004", "2009", "2014")
censos<-c("Insumo1994", "Insumo1999","Insumo2004","Insumo2009","Insumo2014")

for(i in 1:5){
  assign()
  read_dta
  unite(insumox,NIC_NOP_año,c(nic,nop),sep="",remove=TRUE)
  left_join(insumox,correspondencias,by="NIC_NOP_año")
  select(rename(data,newvariable= oldvariable))
  CE2004 %>% rename(!!paste("NIC_NOP_",anios[i],sep=""):="BUENO")->CE2004 
  mutate()
  write.csv()
  rm()
}
setwd ("Z:/Procesamiento/Trabajo/Luis A/Panel_censos")
archivos<-list.files(path=, pattern="Temp")
bases<-lapply(archivos, function(x) read_dta(x)) 
bases<-lapply(bases, function(x) as_tibble(apply(x, 2, function(y) as.numeric(gsub(",", ".", gsub("\\.", "", y))))))
panel<-bind_rows(bases)



# Hay que guardar todos en una lista, aplicarle lo mismo a cada elemto de la lista y luego apppend y listo
# Las variables tienen que ser de las misa clase, sino no se pueden pegar. El principal error es que algunos tienen 
# "," en lugar de "." por lo que no se pueden convertir a numérico, por lo cual hay que sustituir primero estos,
# con el comando gsub.


# No se puede hacer así por el tamaño de las bases
CE2004 <- as_tibble(read_dta("~/RA_UCSC/Stata/Datos/Ejemplo_Censo04/CE2004.dta"))
CE2004 %>% mutate(anio=anios[3]) ->CE2004
CE2004<-as_tibble(apply(CE2004, 2, function(y) as.numeric(gsub(",", ".", gsub("\\.", "", y)))))

CE2009 <- as_tibble(read_dta("~/RA_UCSC/Stata/Datos/Ejemplo_Censo09/CE2009.dta"))
CE2009 %>% mutate(anio=anios[4]) ->CE2009
CE2009<-as_tibble(apply(CE2009, 2, function(y) as.numeric(gsub(",", ".", gsub("\\.", "", y)))))

CE2004 %>% mutate(NIC_NOP_2004="ab")->CE2004
CE2004 %>% rename("BUENO" = paste("NIC_NOP_",anios[i],sep=""))->CE2004
CE2004 %>% rename(!!paste("NIC_NOP_",anios[i],sep=""):="BUENO")->CE2004 

# Hay que revisar bien para que sirven los "!!" y los ":" en los comandos
# vignette("programming") Aquí vienen esas cositas,!! sirve para pasarle la función ya evaluada, los : funcionan para
# que R lo tomé como código válido, esto te lo da rlang.

# Z: Insumos/Proyecto 607/ Censos Económicos
# El archivo de correspondencias tiene unas columnas (d_anio) que indican si
# el NIC_NOP_ANIO se repite dentro del censo, si se repite no hay que pegar el CDE
# Para poder aplicar unite y paste juntos hay que llamar la función paste con 2 signos de 
# admiración antes i.e. "unite(x,!!paste(),sep="",remove=TRUE)

setwd ("C:/Users/pc Luis/Documents/World Bank/Insumos")

# Hay una librería que se llama feather que hace mucho más eficente la lectura y el peso de los archivos.

# Se pasa todo a fst, hace tanto la lectura como la escritura del archivo más eficiente
# comprime todo.

print(object.size(mtcars), units="Kb")

# Para medir los tiempos de ejecución más preciso, con milisegundos.
install.packages("microbenchmark")

# Para apilar las bases existen varios comando de distinta librerias que lo hacen
# (bind_rows(dplyr), rbind(base) rbind_list(data.table)) el más eficiente es rbind_list.

# rbindlist no puede aplicarse a toda la lista.

archivos<-list.files(path = "C:/Users/pc Luis/Documents/World Bank/Insumos", pattern = ".dta")
bases<-lapply(archivos,function(x) haven::read_dta(paste("C:/Users/pc Luis/Documents/World Bank/Insumos/",x,sep="")))
panel<-data.table::rbindlist(list(bases[[1]],bases[[2]]), fill=TRUE)
bases<-bases[-c(1,2)]
panel<-data.table::rbindlist(list(panel,bases[[1]]), fill=TRUE)
bases<-bases[-1]

pryr::mem_change(dplyr::select(bases[[1]],-c(h182a,h182b,h182c,h182d,h152a,h152b,h152c,h152d,h114a,h114b,h114c,h114d,h183a,h183b,h183c,h183d,h189a,h189b,h189c,h189d,j100a,j151a,k540a,k310a,k921a,k635a,l120a,l410a,l520a,l713a,T663a,T661a,l610a,l621a,l621a,k314a,k312a,l320a,k832a,m540a,m951a,m952a,m953a,m954a,m955a,n110a,n400a,m630a,n640a,n700a,n117a,n620a,n620a,n620a,n416a,n101a))->bases[[1]])
pryr::mem_change(dplyr::select(bases[[2]],-c(o501,o502,o503,o504,o521_1a,o521_2a,o521_3a,o521_4a,o521_6a,o521_7a,o521_8a,o522a,o531_1a,o531_2a,o531_3a,o531_4a,o531_5a,o531_6a,o531_7a,o531_9a,o532a,o521_11a,o541_1a,o541_2a,o541_3a,o541_4a,o541_5a,o541_6a,o541_7a,o541_9a,o542a,o551a,o561_1a,o561_2a,o561_3a,o561_9a,o562a,z102_1a,z102_2a,z102_3a,z105_1a,z105_2a,z105_3a,z105_4a,z105_5a,z106_1a,z106_1b,z106_1c,z106_2a,z106_2b,z106_2c,z106_3a,z106_3b,z106_3c,z106_4a,z106_4b,z106_4c,z108_1a,z108_1b,z108_1c,z108_2a,z108_2b,z108_2c,z108_3a,z108_3b,z108_3c,z108_9a,z108_9b,z108_9c,z111_1a,z111_2a,z111_3a,z111_4a,z111_5a,z112_1a,z112_2a,z112_3a,z114_1a,z114_1b,z114_1c,z114_2a,z114_2b,z114_2c,z115_1a,z115_1b,z115_1c,z115_2a,z115_2b,z115_2c,z116_1a,z116_1b,z116_1c,z116_2a,z116_2b,z116_2c,z116_3a,z116_3b,z116_3c))->bases[[2]])
bases<-data.table::rbindlist(bases,fill=TRUE)
panel<-data.table::rbindlist(list(panel,bases),fill=TRUE)

panel<-data.table::rbindlist(list(panel,bases[[1]]), fill=TRUE)
bases<-bases[-1]
panel<-data.table::rbindlist(list(panel,bases[[1]]), fill=TRUE)
rm(bases)

# Se toman los que menos pesan, que son los de 1994 y 2004


# Renombre de las variables de los distintos censos económicos a nomenclatura de 2014


insumo1994 %>% mutate(j300a=css+sar,s611a=rmp+rmb ) %>% select(-c(css,sar,rmp,rmb,po30j,po31d,obe31d,obb31d,em31d,pnr31d,pr30j,pnr30j,pr31d,ob31d,te31d,ev31d)) %>% 
rename(a121a=it ,p210a=gpeb ,p280a=repa ,k332a=mcb ,k040a=cl ,k041a=ee ,p210a=ppe ,k710a=psm ,a111a=pbt ,a131a=vacb ,j000a=rt ,l124a=pi ,k500a=pava ,q000c=adaf ,l300a=pd ,h001a=po30j ,k000a=gd ,l000a=ognd ,m000a=id ,n000a=oind ,q000a=afn ,a221a=fbcf ,h001a=pop ,o114a=vpe ,k331a=gp ,h114b=obhe ,h114c=obme ,h171b=obhb ,h171c=obmb ,h203b=emh ,h203c=emm ,h020b=nrh ,h020c=nrm ,h114a=obre ,h171a=obrb ,h203a=emp ,h020a=pnr ,j000a=susa ,j114a=sale ,j117a=salb ,j203a=sue ,j020a=ps ,j400a=ops ,j500a=ur ,h001d=dt ,q111a=mepa ,q112a=meba ,q151a=ppda ,q020a=elca ,q220a=tea ,q030a=ueta ,q400a=eca ,q900a=meoa ,a221a_111a=mepf ,a221a_112a=mebf ,a221a_151a=ppdf ,a221a_020a=elcf ,a221a_220a=tef ,a221a_030a=uetf ,a221a_400a=ecf ,a221a_900a=meof ,o113a=vpex ,o114a=vpb ,m700a=ipsm ,o020a=afp ,m999a=oida ,m332a=ip ,k910a=eem ,h114b=obh ,h114c=obm ,h114a=ob31d ,h114a=obr ,j114a=sal ,a511a=mbcv ,k100a=cmr ,m100a=vnm ,k415a=cer ,k060a=psp ,o116a=veg ,h171b=temph ,h171c=tempm ,h172b=evh ,h172c=evm ,h000a=ptem ,h172a=eve ,j117a=susae ,m500a=ira ,k820a=psc ,m000a=ibt ,q010a=eta) %>% 
mutate(q110a=q111a+q112a)

insumo1999 %>% mutate(j300a=cpss+aprr,l621a=gnsin+recla,l300a=dsm+daa+der+perpes+derpor,n620a=repo+idery+pvaly,m999a=oida+iamdc+iaeoe,n999a=cya+oinda,p900a=iicyl+iiob) %>% select(-c(cpss,aprr,gnsin,recla,dsm,daa,der,perpes,derpor,repo,idery,pvaly,oida,iamdc,iaeoe,cya,oinda,iicyl,iiob,iicar,iisal,iipqui,iiovi,iiaba,ifcar,ifsal,ifpqui,ifovi,ifabal,afletv,afop,estacu,cafmec,cafletv,cafop,estacuc,vafmec,vafletv,vafop,estacuv,dmec,cletv,dopro,estacud)) %>% 
rename(a111a=pbt ,a121a=it ,a131a=vacb ,j000a=rt ,l220a=impuestos ,l100a=iscp ,q000b=aftd ,a511a=margenbrut ,h001a=popt ,h001b=poph ,h001c=popm ,h001d=pophht ,H101A=popto ,H101B=popho ,H101C=popmo ,H101D=pophhto ,h182a=pyat ,h182b=pyah ,h182c=pyam ,h182d=pyahht ,h152a=pat ,h152b=pah ,h152c=pam ,h152d=hhtpa ,h114a=ppott ,h114b=poth ,h114c=potm ,h114d=pothht ,h183a=ptpat ,h183b=ptpah ,h183c=ptpam ,h183d=hhtpt ,h189a=totpa ,h189b=otpah ,h189c=otpam ,h189d=tthtotpa ,h203a=eot ,h203b=eoh ,h203c=eom ,h203d=eohht ,h114a=obt ,h114b=obh ,h114c=obm ,h114d=obhht ,H203A=emact ,H203B=emach ,H203C=emacm ,H203D=emachht ,H020A=pnrt ,H020B=pnrh ,H020C=pnrm ,H020D=pnrhht ,i100a=torst ,i100b=torsh ,i100c=torsm ,i100d=torshht ,j000a=sys ,J010A=sa ,j100a=satpa ,j151a=sspa ,j200a=su ,j020a=ps ,j400a=ops ,j500a=ur ,j600a=ppid ,k010a=mcr ,k020a=mps ,k910a=ees ,k040a=cyl ,k950a=semyr ,k041a=enel ,k050a=abmi ,k540a=amep ,k950a=aobmi ,k820a=secom ,k620a=pcys ,k810a=pub ,k931a=reg ,k310a=mpya ,k921a=primg ,k635a=aseg ,l120a=premg ,l410a=comisg ,l520a=cuderg ,l713a=precon ,T663a=iderg ,T661a=pvalg ,l610a=cnad ,l621a=gnsin ,l621a=recla ,a799a=ogda ,k710a=gsm ,k331a=ers ,k332a=mcb ,s531a=tpe ,l300a=dsm ,l300a=daa ,l330a=contcon ,k314a=psabloq ,k312a=iptar ,l320a=pdudar ,k720a=pagsub ,k321a=mpccp ,k322a=mpcsub ,K060A=psp ,k415a=eecpr ,k040a=cpgee ,k832a=actri ,k000a=tgda ,l220a=iin ,l350a=proobiyv ,l999a=ogfis ,l000a=tgnda ,m332a=vnmeb ,s537a=serpros ,m010a=vnmar ,m500a=iabmei ,m540a=iaemyep ,m200a=ipreser ,m310a=vnpe ,m421a=vnpcr ,m422a=vngn ,m423a=vncon ,m700a=ipsm ,m931a=ipreg ,m321a=ieoccp ,m322a=ieocs ,m951a=iptdom ,m952a=iptcom ,m953a=iptind ,m954a=iptsep ,m955a=ivatra ,n110a=iy ,n400a=comisy ,m630a=asey ,n640a=rcvv ,n700a=rcam ,n117a=premy ,n416a=admon ,n101a=primdy ,m340a=veemn ,m340c=veeme ,m356a=sertrao ,m210a=itpa ,m220a=itca ,m230a=paq ,m000a=tida ,n911a=cudery ,n200a=sub ,n000a=tinda ,o010a=vpe ,o121a=vpcex ,o122a=vgnex ,o123a=vcoob ,o020a=vafpup ,o210a=eicpup ,o220a=oafppup ,o113a=vme ,o114a=vmb ,o111a=voeccp ,o112a=voecsub ,o300a=vaagpro ,o116a=geecp ,o117a=geece ,p230a=iimpps ,p100a=iimcr ,p330a=iipe ,p340a=iipp ,p200a=iimpa ,p370a=iipcr ,p380a=iicon ,p310a=iime ,p320a=iimb ,p210a=iies ,p210a=iirs ,p220a=iimco ,p260a=iiita ,p350a=iicge ,p280a=iirpa ,p900a=iicyl ,p900a=iiob ,p000a=iitot ,p340b=ifpp ,p200b=ifmpa ,p330b=ifpe ,p370b=ifpcru ,p380b=ifcon ,p230b=ifmpps ,p100b=ifmcr ,p900b=ifob ,p310b=ifme ,p310b=ifmb ,p210b=ifes ,p210b=ifrs ,p220b=ifmco ,p350b=ifcge ,p260b=ifita ,p280b=ifra ,p900b=ifcyl ,p000b=iftot ,p000c=var_tote ,a211a=inv_tot ,q111a=myepes ,q321a=afetp ,q010a=afmec ,q112a=myepbs ,q100a=myepps ,q290a=ceifs ,q220a=aftst ,q300a=afets ,q400a=afecs ,q900a=afmobs ,q210a=aftds ,q132a=aryeqp ,q133a=eseysa ,q134a=myeit ,q151a=pyplm ,q161a=afductot ,q000a=afts ,q111c=myepec ,q321c=cfetp ,q112c=myepbc ,q010c=myeppc ,q290c=ceifc ,q220c=aftco ,q300c=afetc ,q400c=afecc ,q900c=afmobc ,q210c=aftdc ,q132c=aryeqpc ,q133c=eseysac ,q134c=myeitc ,q151c=pyplmc ,q161c=afductoc ,q000c=aftc ,q111d=myepev ,q321d=vaetp ,q112d=myepbv ,q010d=myeppv ,q290d=ceifv ,q220d=aftve ,q300d=afetv ,q400d=afecv ,q900d=afmobv ,q210d=aftdv ,q132d=arteqpv ,q133d=eseysacv ,q134d=myeitv ,q151d=pyplmv ,q000d=aftv ,q111b=myeped ,q321b=detps ,q112b=myepbd ,q010b=myeppd ,q290b=ceifd ,q300b=afetd ,q400b=afecd ,q900b=afmobd ,q210b=aftdd ,q132b=arteqpd ,q133b=eseysacd ,q134b=myeitd ,q151b=myplmd ,q161b=afductod)

#Falta renombrar 4 variables que no encontré por ningún lado(1994)
#Falta renombrar 11 y elimine varias (1999)

# con esto agrupo lo del SAR, además elimino las variable totales de inicio y fin de
# año y me quedo solo con los promedios que son los reportados en 2014.
# De hombres y mujeres me quedo con los datos de mitade del año
insumo2004 %>% rename(h203a=h200a ,h203b=h200b ,h203c=h200c ,h203d=h200d ,h020a=h300a ,h020b=h300b ,h020c=h300c ,h020d=h300d ,j100a=j101a ,l124a=l110a ,m700a=m710a ,o010a=o110a ,o020a=o220a ,p030a=p340a ,p030b=p340b ,p030c=p340c ,p900a=p999a ,p900b=p999b ,q010a=q100a ,q010b=q100b ,q010c=q100c ,q010d=q100d ,q020a=q200a ,q020b=q200b ,q020c=q200c ,q020d=q200d ,q030a=q300a ,q030b=q300b ,q030c=q300c ,q030d=q300d)->insumo2004


insumo2009 %>% rename(h203a=h200a ,h203b=h200b ,h203c=h200c ,h203d=h200d ,k030a=k300,k070a=k700a ,m030a=m300a ,p030a=p340a ,p030b=p340b ,p030c=p340c ,q010a=q101a ,q030a=q300a ,g111a=g111 ,z111_2a=z101_2 ,z111_1a=z101_3 ,z114_1a=z103 ,z102_1a=z104_1 ,z102_2a=z104_2 ,z102_3a=z104_3 ,z101a=z106_1 ,z103a=z106_2 ,a511a=b111a ,h020a=h300a ,o010a=o100 ,q010c=q101c ,q010d=q101d ,q020a=q201a ,q020c=q201c ,q020d=q201d ,q030c=q300c ,q030d=q300d)->insumo2009

aux<-c("insumo1994" ,"insumo1999", "insumo2004", "insumo2009", "insumo2014")
assign(aux[1],bases[[1]])
source("C:/Users/pc Luis/Documents/World Bank/R/Renombrar_prueba.R")

#-------------------------------------------------------------------------------------#
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



## Curvas por fecha de anuncio, una semana antes , una semana después y en la fecha de anuncio, hay datos a partir de fechaanuncio[4] #
fechaanuncio1<-fechaanuncio[fechaanuncio>min(a$Fecha)]

a<-df1
plot_list<-list()
for (j in 1:length(fechaanuncio1)){
  aux<-filter(a, Fecha==fechaanuncio1[j] | Fecha==fechaanuncio1[j]-7 |Fecha==fechaanuncio1[j]+7)
  if (nrow(aux)!=0){
    aux$Fecha<-as.factor(aux$Fecha)
    q1<-ggplot(aux, aes(plazo, tasa))+geom_point(aes(col=Fecha))
    plot_list[[j]]<-q1	
  }
}
multiplot(plotlist=plot_list,cols=5)


pdf("C:/Users/pc Luis/Documents/World Bank/R/myplot.pdf")
myplot <- multiplot(list(a,b))
print(myplot)
dev.off()
