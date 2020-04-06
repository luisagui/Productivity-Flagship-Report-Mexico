#install.packages("GmapsDistance")
#library(GmapsDistance)
require(dplyr)
require(tidyr)
require(osrm)
require(plyr)

#set.api.key("AIzaSyAnCmsbtyA0hYKL3Z02uPthM02UNax41Cc")


land<-readxl::read_excel("C:/Users/pc Luis/Documents/World Bank/superficie_municipios.xlsx")
mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Mapas/Municipios_coordenadas.csv")
# linear distance computed with QGIS
mun_distance<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Mapas/Distancias_Municipios_final.csv")


mun %>% mutate(CVE_ENT=if_else(CVE_ENT<10,paste(0,as.character(CVE_ENT),sep=""),as.character(CVE_ENT)),
               CVE_MUN=if_else(CVE_MUN<10,paste(00,as.character(CVE_MUN),sep=""),
                                                             if_else(CVE_MUN<100 & CVE_MUN>=10 ,paste(0,as.character(CVE_MUN),sep=""),
                                                                     as.character(CVE_MUN))),CVEGEO=paste(CVE_ENT,CVE_MUN,sep="")) %>%
  as.tbl()

mun<- mun %>% select(CVEGEO,x,y) %>% rename(long=x,lat=y)

# In order to get driving distance and trio duration, I tried to use "gmapdistance" package but it google maps API is limited to certain
# amount of requests per day,and each request have a cost, so I decided to avoid using google maps.
# Instead I found "osrm" package that use OpenStreetMaps to compute distances,it is not limited on the amount on requestant it does not
# cgharge you per request, hte issue with this package is that if you use the demo server you can only compute duration and some times it
# get too busy, denying the request, there are two ways to handle this.
# First, automate the requests until a distance is computed, and find average speed used to compute distance from trip.
# Second, use a local server, that measn installing in your computer all needed to make requests thorough your computer instead of
# the public server. This is not really clear how to do it with windows, takes a whole day (around 15 hrs) and R wrapper has error
# with querys, so far I have not find the way to make it work correctly.

# I decided to use the First approach.
options(osrm.server = "http://router.project-osrm.org/", osrm.profile = "driving") # Demo server
options(osrm.server = "http://127.0.0.1:5000/", osrm.profile = "driving") # Local server, it has some issues with requests.

# I already have long and lat in degrees, the issue is I can only request 10000 directions as much per request,
# I need to build a database with 2463x2463=6066369 elements, actually I only need the upper or lower triangle (not both).


filas<-list()

for (i in 1:dim(mun)[1]){
  bi=0
  while(bi!=100){
  tabla<-try(osrmTable(src=mun[i,],dst=mun[1:340,]) %>%  `[[`("durations"),silent=TRUE)
    if (class(tabla)=="NULL") {
      cat("ERROR1: ", tabla, "\n")
      Sys.sleep(1)
      print("reconntecting...")
      bi <- bi+1
      print(bi)
      print(i)
    } else
      break 
  }
    for (j in c(680,1020,1360,1700,2040,2380,2463)){
      if (j==2463){
        bo=0
        while(bo!=100){
          x=try(osrmTable(src=mun[i,],dst=mun[2381:j,]) %>%  `[[`("durations"),silent=TRUE)
          if (class(x)=="NULL") {
            cat("ERROR1: ", x, "\n")
            Sys.sleep(1)
            print("reconntecting...")
            bo <- bo+1
            print(bo)
            print(i)
            print(j)
          } else
            break 
        }
      } else
      
      tabla<-tabla %>% cbind(x)
    }
  filas[[i]]<-tabla
}
# It took around 14 hrs

# The way I compute distance matrix was by requesting as much destinations allowed by the command, per source (2463 sources). After a quick
# test 340 destinatios was the magic number, so I took destinations by groups of 340 till I reach the 2463. 
# It also happens that sometimes the server is too busy, so it shows an error "too many requests" the way to deal
# with this is to try until it works, for this I used "try" command, still I got some different errors for other cases
# like "lexical error" for those cases I was not able to get any result, so in the final database I will have NA's
# The last step is to fill thos NA's with their value.
# I saved results in a list and append them to get the matrix.

# Some distances were computed two times, they are identified with ".1" at the end, I add the mun column
# to identify the municipality and I drop the duplicated columns.
dfp<-filas %>% lapply(. %>% as.data.frame(.) %>% mutate(mun=rownames(.)) %>% select(-ends_with(".1")) %>%
                        select(mun,everything()))
#df<-ldply(filas,data.frame)
df1<-ldply(dfp,data.frame)

df1 %>% as.tbl()
# There are less tha 5 missings per column, in order to fill this missings I have to do work with each case
# it will be easier to think in a way to do it automatically

df2<-df1 %>% gather(key=destination,value=duration,-mun)
missings<-df2 %>% filter(is.na(duration))
missings<-missings %>% mutate(destination=substr(destination,2,nchar(destination)))

d_missings<-data.frame()

for (i in 1:dim(missings)[1]){
bo=0
  while(bo!=100){
  y=try(osrmTable(src=(missings %>% select(mun) %>% mutate(mun=as.numeric(mun)) %>%
                               left_join(mun,by=c("mun"="CVEGEO")))[i,],
            dst=(missings %>% select(destination) %>% mutate(destination=as.numeric(destination)) %>%
                   left_join(mun,by=c("destination"="CVEGEO")))[i,]) %>% `[[`("durations"),silent=TRUE)
    if (class(y)=="NULL") {
      cat("ERROR1: ", y, "\n")
      Sys.sleep(1)
      print("reconntecting...")
      bo <- bo+1
      print(bo)
      print(i)
    } else
      break 
  }
y<-y %>% as.data.frame() %>%  mutate(mun=row.names(y),
                                     destination=colnames(y)) %>% select(mun,destination,duration=1)

d_missings<-d_missings %>% bind_rows(y)
}

d_missings<-d_missings %>% dplyr::rename(d_m=duration)

df2<-df2 %>% mutate(destination=substr(destination,2,nchar(destination))) %>% 
  left_join(d_missings,by=c("mun","destination")) %>% 
  mutate(duration=if_else(is.na(duration),d_m,duration)) %>% select(mun,destination,duration)

save(df2,file="C:/Users/pc Luis/Documents/World Bank/Mapas/Travel_duration_municipalities.RData")
df2 %>% fst::write_fst(.,"C:/Users/pc Luis/Documents/World Bank/Mapas/Travel_duration_municipalities.fst")

mun_distance %>% fst::write_fst(.,"C:/Users/pc Luis/Documents/World Bank/Mapas/Travel_distance_municipalities_QGIS.fst")
nearest_200km_mun<-mun_distance %>% mutate(Distance=Distance/1000) %>% group_by(InputID) %>%
  filter(Distance<=200)

save(nearest_200km_mun,
     file="C:/Users/pc Luis/Documents/World Bank/Mapas/Travel_distance_n200_municipalities_QGIS.RData")

nearest_200km_mun %>% data.table::fwrite(.,"C:/Users/pc Luis/Documents/World Bank/Mapas/Travel_distance_n200_municipalities_QGIS.csv")

load(file="Z:/Procesamiento/Trabajo/Luis A/Travel_distance_n200_municipalities_QGIS.RData") # nearest_200km_mun

Market_Potential<-nearest_200km_mun %>% left_join(Emp_mun,by=c("TargetID"="mun")) %>%
  mutate(we=h001a/Distance) %>% group_by(InputID) %>% summarise_at(vars(we),funs(sum(.,na.rm=TRUE)))
