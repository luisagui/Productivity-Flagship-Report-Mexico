# Corrections to compute and extra graphs to be done.

# Structural change

require(dplyr)
require(tidyr)
require(stargazer)

Base_real<-fst::read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Base_cleaning_rse_2.fst", columns = c("anio","CDE","e03","e04","e17","SCIAN2dig","h001a","a131a_real","Vaw"))

Sectors<-data.table::fread("Z:/Procesamiento/Trabajo/Luis A/Sectors.csv")
Sectors %>% mutate(e17=as.character(e17))->Sectors

Base_real %>% left_join(Sectors, by=c("e17"))->Base_real

# sector= 1/ Manufacturing, 3/ Services (4/ tradable, 5/ non tradable)

Base_real %>% group_by(anio,Sector) %>% summarise_at(vars(a131a_real, h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(anio) %>% mutate(share=h001a/sum(h001a,na.rm=TRUE), Vaw=if_else(a131a_real==0 & h001a==0,0,if_else(a131a_real!=0 & h001a==0,-99999,a131a_real/h001a))) %>% na_if(-99999)->Base_anio_sector
Base_real %>% mutate(Manuf=if_else(SCIAN2dig=="30",1,0)) %>% group_by(anio,Manuf) %>% summarise_at(vars(a131a_real, h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(anio) %>% mutate(share=h001a/sum(h001a,na.rm=TRUE), Vaw=if_else(a131a_real==0 & h001a==0,0,if_else(a131a_real!=0 & h001a==0,-99999,a131a_real/h001a))) %>% na_if(-99999)->Base_anio_sector_Manuf

# National descomposition

Base_anio_sector %>% group_by(Sector) %>% mutate(share_t5=dplyr::lag(share),Vaw_t5=dplyr::lag(Vaw),Structural=(share-share_t5)*Vaw,Within=share_t5*(Vaw-Vaw_t5)) %>% group_by(anio) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw))->Structural_change_Manuf
Base_anio_sector %>% group_by(Sector) %>% mutate(share_t12=dplyr::lag(share,4),Vaw_t12=dplyr::lag(Vaw,4),Structural=(share-share_t12)*Vaw,Within=share_t12*(Vaw-Vaw_t12)) %>% group_by(anio) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw,4)) %>% filter(anio==2014)->Structural_change_Manuf_long

# Second split
Base_anio_sector_Manuf %>% group_by(Manuf) %>% mutate(share_t5=dplyr::lag(share),Vaw_t5=dplyr::lag(Vaw),Structural=(share-share_t5)*Vaw,Within=share_t5*(Vaw-Vaw_t5)) %>% group_by(anio) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw))->Structural_change_Manuf_2split
Base_anio_sector_Manuf %>% group_by(Manuf) %>% mutate(share_t12=dplyr::lag(share,4),Vaw_t12=dplyr::lag(Vaw,4),Structural=(share-share_t12)*Vaw,Within=share_t12*(Vaw-Vaw_t12)) %>% group_by(anio) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw,4)) %>% filter(anio==2014)->Structural_change_Manuf_long_2split

# Now we will do the same analysis by region(State).

# sector= 1/ Manufacturing, 2/Commerce, 3/ Services.

Base_real %>% group_by(anio,e03,Sector) %>% summarise_at(vars(a131a_real, h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(anio,e03) %>% mutate(share=h001a/sum(h001a,na.rm=TRUE), Vaw=if_else(a131a_real==0 & h001a==0,0,if_else(a131a_real!=0 & h001a==0,-99999,a131a_real/h001a))) %>% na_if(-99999)->Base_anio_st_sector
Base_real %>% mutate(Manuf=if_else(SCIAN2dig=="30",1,0)) %>%  group_by(anio,e03,Manuf) %>% summarise_at(vars(a131a_real, h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(anio,e03) %>% mutate(share=h001a/sum(h001a,na.rm=TRUE), Vaw=if_else(a131a_real==0 & h001a==0,0,if_else(a131a_real!=0 & h001a==0,-99999,a131a_real/h001a))) %>% na_if(-99999)->Base_anio_st_sector_Manuf
# National descomposition

Base_anio_st_sector %>% group_by(e03,Sector) %>% mutate(share_t5=dplyr::lag(share),Vaw_t5=dplyr::lag(Vaw),Structural=(share-share_t5)*Vaw,Within=share_t5*(Vaw-Vaw_t5)) %>% group_by(anio,e03) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(e03) %>%  mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw))->Structural_change_st_Manuf
Base_anio_st_sector %>% group_by(e03,Sector) %>% mutate(share_t12=dplyr::lag(share,4),Vaw_t12=dplyr::lag(Vaw,4),Structural=(share-share_t12)*Vaw,Within=share_t12*(Vaw-Vaw_t12)) %>% group_by(anio, e03) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(e03) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw,4)) %>% filter(anio==2014)->Structural_change_st_Manuf_long

#Second aplit
Base_anio_st_sector_Manuf %>% group_by(e03,Manuf) %>% mutate(share_t5=dplyr::lag(share),Vaw_t5=dplyr::lag(Vaw),Structural=(share-share_t5)*Vaw,Within=share_t5*(Vaw-Vaw_t5)) %>% group_by(anio,e03) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(e03) %>%  mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw))->Structural_change_st_Manuf_2split
Base_anio_st_sector_Manuf %>% group_by(e03,Manuf) %>% mutate(share_t12=dplyr::lag(share,4),Vaw_t12=dplyr::lag(Vaw,4),Structural=(share-share_t12)*Vaw,Within=share_t12*(Vaw-Vaw_t12)) %>% group_by(anio, e03) %>% summarise_at(vars(Structural,Within,a131a_real,h001a),funs(sum(.,na.rm=TRUE))) %>% group_by(e03) %>% mutate(Vaw=a131a_real/h001a, Vaw_delta=Vaw-dplyr::lag(Vaw,4)) %>% filter(anio==2014)->Structural_change_st_Manuf_long_2split

data.table::fwrite(Structural_change_Manuf,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_1.csv")
data.table::fwrite(Structural_change_Manuf_long,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_1.csv",append = TRUE)
data.table::fwrite(Structural_change_Manuf_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_1.csv",append = TRUE)
data.table::fwrite(Structural_change_Manuf_long_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_1.csv",append = TRUE)
data.table::fwrite(Structural_change_st_Manuf,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_states_1.csv")
data.table::fwrite(Structural_change_st_Manuf_long,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_states_1.csv",append = TRUE)
data.table::fwrite(Structural_change_st_Manuf_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_states_1.csv",append = TRUE)
data.table::fwrite(Structural_change_st_Manuf_long_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Structural_change_states_1.csv",append = TRUE)

#############################################################################################################################################################################
# Static & Dynamic Decompositions

Ent_Surv<-fst::read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Ent_Surv_Exit_new_version_1.fst")
Ent_Surv %>% select(-c(e03,e04,e17,SCIAN2dig,Surv_pos,Surv_neg))%>% mutate(Ent=replace(Ent,which(!is.na(Ent)),1),Surv=replace(Surv,which(!is.na(Surv)),1),Exit=replace(Exit,which(!is.na(Exit)),1))->Ent_Surv_1
Ent_Surv_1 %>% select(anio, CDE,Surv) %>% rename(Surv_next=Surv) %>% mutate(anio=as.character(as.numeric(anio)-5))->Surv
Ent_Surv_1 %>% left_join(Surv,by=c("anio","CDE"))->Ent_Surv_1

Base_real %>% ungroup() %>% select(anio, CDE,e17,SCIAN2dig,Sector,a131a_real,Vaw) %>% left_join(Ent_Surv_1, by=c("anio","CDE"))->Base_Dynamic_Descomposition

Base_Dynamic_Descomposition %>% ungroup() %>% select(-Sector) %>% mutate(Vaw=replace(Vaw,which(h001a==0),NA)) %>% mutate(Manuf=if_else(SCIAN2dig=="30",1,0)) %>% filter(!is.na(Vaw)) %>% group_by(anio,Sector) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_firm=h001a/l_Nation) %>% select(-l_Nation)->Base_Dynamic_Descomposition_2split
Base_Dynamic_Descomposition %>% ungroup() %>% mutate(Vaw=replace(Vaw,which(h001a==0),NA)) %>% filter(!is.na(Vaw)) %>% group_by(anio,Sector) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_firm=h001a/l_Nation) %>% select(-l_Nation)->Base_Dynamic_Descomposition

# By year/ Manufacturing/ Services(Tradable, Non-Tradable)

Base_Dynamic_Descomposition %>% ungroup() %>% filter(Surv==1) %>% group_by(anio,Sector) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_surv=h001a/l_Nation,W=mean(w_surv,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_surv-W)*(Vaw-A)) %>% select(anio,Sector,Surv, a131a_real,h001a,Vaw,w_surv,Prod) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n()))%>% select(anio,Sector,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,Surv_Suma) %>% rename(N=Surv_Suma,Vaw_Cov=Prod_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Cov_Surv
Base_Dynamic_Descomposition %>% ungroup() %>% filter(Surv_next==1) %>% group_by(anio,Sector) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_surv=h001a/l_Nation,W=mean(w_surv,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_surv-W)*(Vaw-A)) %>% select(anio,Sector,Surv_next, a131a_real,h001a,Vaw,w_surv,Prod) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n()))%>% select(anio,Sector,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,Surv_next_Suma) %>% rename(N=Surv_next_Suma,Vaw_Cov=Prod_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Cov_Surv_lag

Cov_Surv_lag %>% ungroup() %>%  rename(a131a_real_Suma_lag=a131a_real_Suma,h001a_Suma_lag=h001a_Suma,Vaw_Avg_lag=Vaw_Avg,Vaw_Cov_lag=Vaw_Cov,N_lag=N,Vaw_lag=Vaw) %>% mutate(anio=as.character(as.numeric(anio)+5))->Cov_Surv_lag_join

Base_Dynamic_Descomposition %>% ungroup() %>% filter(Ent==1) %>% select(anio,Sector,Ent, a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Sector) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE))) %>% select(anio,Sector,a131a_real_Suma,h001a_Suma,w_firm_Suma,Ent_Suma) %>% rename(N_Ent=Ent_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Ent
Ent %>% rename(w_firm_Ent=w_firm_Suma,Vaw_Ent=Vaw) %>%  select(anio, Sector,w_firm_Ent,Vaw_Ent,N_Ent)->Ent_join

Base_Dynamic_Descomposition %>% ungroup() %>% filter(Exit==1) %>% select(anio,Sector,Exit, a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Sector) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE))) %>% select(anio,Sector,a131a_real_Suma,h001a_Suma,w_firm_Suma,Exit_Suma) %>% rename(N_Exit=Exit_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Exit
Exit %>% ungroup() %>% rename(w_firm_Exit=w_firm_Suma,Vaw_Exit=Vaw)%>%  select(anio, Sector,w_firm_Exit,Vaw_Exit,N_Exit) %>% mutate(anio=as.character(as.numeric(anio)+5))->Exit_join

Cov_Surv %>% left_join(Cov_Surv_lag_join,by=c("anio","Sector")) %>% select(anio, Sector,Vaw_Avg,Vaw_Cov,Vaw,N,Vaw_Avg_lag,Vaw_Cov_lag,Vaw_lag,N_lag) %>% mutate(delta_Vaw_Avg=Vaw_Avg-Vaw_Avg_lag,delta_Vaw_Cov=Vaw_Cov-Vaw_Cov_lag) %>% left_join(Ent_join,by=c("anio","Sector")) %>% left_join(Exit_join,by=c("anio","Sector")) %>% mutate(DELTA=delta_Vaw_Avg+delta_Vaw_Cov+w_firm_Ent*(Vaw_Ent-Vaw)+w_firm_Exit*(Vaw_lag-Vaw_Exit))->D_Des_S_E_Ex

Base_Dynamic_Descomposition %>% ungroup() %>% select(anio,Sector,a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Sector) %>% mutate(W=mean(w_firm,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_firm-W)*(Vaw-A)) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n())) %>% select(anio,Sector,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,h001a_N) %>% rename(Vaw_Cov=Prod_Suma,N=h001a_N) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma) %>% group_by(Sector) %>% mutate(Vaw1=Vaw_Avg+Vaw_Cov,DELTA=Vaw-dplyr::lag(Vaw))->Total

data.table::fwrite(Total,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Descomposition_Sector_clean_1.csv")
data.table::fwrite(D_Des_S_E_Ex,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Dynamic_Descomposition_Sector_clean_1.csv")


# Second split

Base_Dynamic_Descomposition_2split %>% ungroup() %>% filter(Surv==1) %>% group_by(anio,Manuf) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_surv=h001a/l_Nation,W=mean(w_surv,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_surv-W)*(Vaw-A)) %>% select(anio,Manuf,Surv, a131a_real,h001a,Vaw,w_surv,Prod) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n()))%>% select(anio,Manuf,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,Surv_Suma) %>% rename(N=Surv_Suma,Vaw_Cov=Prod_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Cov_Surv_2split
Base_Dynamic_Descomposition_2split %>% ungroup() %>% filter(Surv_next==1) %>% group_by(anio,Manuf) %>% mutate(l_Nation=sum(h001a,na.rm=TRUE),w_surv=h001a/l_Nation,W=mean(w_surv,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_surv-W)*(Vaw-A)) %>% select(anio,Manuf,Surv_next, a131a_real,h001a,Vaw,w_surv,Prod) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n()))%>% select(anio,Manuf,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,Surv_next_Suma) %>% rename(N=Surv_next_Suma,Vaw_Cov=Prod_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Cov_Surv_lag_2split

Cov_Surv_lag_2split %>% ungroup() %>%  rename(a131a_real_Suma_lag=a131a_real_Suma,h001a_Suma_lag=h001a_Suma,Vaw_Avg_lag=Vaw_Avg,Vaw_Cov_lag=Vaw_Cov,N_lag=N,Vaw_lag=Vaw) %>% mutate(anio=as.character(as.numeric(anio)+5))->Cov_Surv_lag_join_2split

Base_Dynamic_Descomposition_2split %>% ungroup() %>% filter(Ent==1) %>% select(anio,Manuf,Ent, a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Manuf) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE))) %>% select(anio,Manuf,a131a_real_Suma,h001a_Suma,w_firm_Suma,Ent_Suma) %>% rename(N_Ent=Ent_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Ent_2split
Ent_2split %>% rename(w_firm_Ent=w_firm_Suma,Vaw_Ent=Vaw) %>%  select(anio, Manuf,w_firm_Ent,Vaw_Ent,N_Ent)->Ent_join_2split

Base_Dynamic_Descomposition_2split %>% ungroup() %>% filter(Exit==1) %>% select(anio,Manuf,Exit, a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Manuf) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE))) %>% select(anio,Manuf,a131a_real_Suma,h001a_Suma,w_firm_Suma,Exit_Suma) %>% rename(N_Exit=Exit_Suma) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma)->Exit_2split
Exit_2split %>% ungroup() %>% rename(w_firm_Exit=w_firm_Suma,Vaw_Exit=Vaw)%>%  select(anio, Manuf,w_firm_Exit,Vaw_Exit,N_Exit) %>% mutate(anio=as.character(as.numeric(anio)+5))->Exit_join_2split

Cov_Surv_2split %>% left_join(Cov_Surv_lag_join_2split,by=c("anio","Manuf")) %>% select(anio, Manuf,Vaw_Avg,Vaw_Cov,Vaw,N,Vaw_Avg_lag,Vaw_Cov_lag,Vaw_lag,N_lag) %>% mutate(delta_Vaw_Avg=Vaw_Avg-Vaw_Avg_lag,delta_Vaw_Cov=Vaw_Cov-Vaw_Cov_lag) %>% left_join(Ent_join_2split,by=c("anio","Manuf")) %>% left_join(Exit_join_2split,by=c("anio","Manuf")) %>% mutate(DELTA=delta_Vaw_Avg+delta_Vaw_Cov+w_firm_Ent*(Vaw_Ent-Vaw)+w_firm_Exit*(Vaw_lag-Vaw_Exit))->D_Des_S_E_Ex_2split

Base_Dynamic_Descomposition_2split %>% ungroup() %>% select(anio,Manuf,a131a_real,h001a,Vaw,w_firm) %>% group_by(anio,Manuf) %>% mutate(W=mean(w_firm,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w_firm-W)*(Vaw-A)) %>% summarise_all(funs(Suma=sum(.,na.rm=TRUE),Avg=mean(.,na.rm=TRUE),N=n())) %>% select(anio,Manuf,a131a_real_Suma,h001a_Suma,Vaw_Avg,Prod_Suma,h001a_N) %>% rename(Vaw_Cov=Prod_Suma,N=h001a_N) %>% mutate(Vaw=a131a_real_Suma/h001a_Suma) %>% group_by(Manuf) %>% mutate(Vaw1=Vaw_Avg+Vaw_Cov,DELTA=Vaw-dplyr::lag(Vaw))->Total_2split

data.table::fwrite(Total_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Descomposition_Sector_clean_2split.csv")
data.table::fwrite(D_Des_S_E_Ex_2split,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Dynamic_Descomposition_Sector_clean_2split.csv")

###########################################################################################################################################################################
# Olley pakes Decomposition ( Betweens & Within)

# We can do this decomposition spliting by major sectors ( Manufacturing,Services (Tradable, Non-tradable))

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-c(CDE,e04,e17,Vaw)) %>% group_by(Sector,anio, e03) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod), funs(sum(.,na.rm=TRUE))) %>% rename(Prod_State=Prod)->State_sectors

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-c(CDE,e17,Vaw)) %>% group_by(Sector,anio, e03, e04) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio, e03) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod), funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio) %>% summarise_at(vars(Prod), funs(mean(.,na.rm=TRUE))) %>%  rename(Prod_Mun=Prod)->Mun_sectors

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-e17) %>%  group_by(Sector,anio, e03,e04) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod,Vaw), funs(sum(.,na.rm=TRUE),mean(.,na.rm=TRUE))) %>% select(Sector,anio, e03,e04, Prod_sum, Vaw_mean) %>% rename(Prod=Prod_sum, Vaw=Vaw_mean) %>% group_by(Sector, anio, e03) %>% summarise_at(vars(Prod,Vaw),funs(sum(mean(.,na.rm=TRUE)))) %>% group_by(Sector,anio) %>% summarise_at(vars(Prod,Vaw), funs(mean(.,na.rm=TRUE))) %>% rename(Prod_firm=Prod, Vaw_firm=Vaw)->Firm_sectors

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-e17) %>% group_by(Sector,anio) %>% summarise_at(vars(a131a_real,h001a), funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a) %>% select(Sector,anio, Vaw)->Total_sectors

Total_sectors %>% left_join(State_sectors, by=c("Sector","anio")) %>% left_join(Mun_sectors, by=c("Sector","anio")) %>% left_join(Firm_sectors, by=c("Sector","anio"))->Decomposition_sectors

data.table::fwrite(Decomposition_sectors, "Z:/Resultados/LM-607-PCV-282-CE-2019-07-19/Growth_Decomposition_Sectors_clean_1.csv")


# Second split

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-c(CDE,e04,e17,Vaw)) %>% mutate(Sector=replace(Sector,which(Sector!=1),0)) %>% group_by(Sector,anio, e03) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod), funs(sum(.,na.rm=TRUE))) %>% rename(Prod_State=Prod)->State_sectors_2split

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-c(CDE,e17,Vaw)) %>% mutate(Sector=replace(Sector,which(Sector!=1),0)) %>% group_by(Sector,anio, e03, e04) %>% summarise_all(funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio, e03) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod), funs(sum(.,na.rm=TRUE))) %>% group_by(Sector,anio) %>% summarise_at(vars(Prod), funs(mean(.,na.rm=TRUE))) %>%  rename(Prod_Mun=Prod)->Mun_sectors_2split

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-e17) %>% mutate(Sector=replace(Sector,which(Sector!=1),0)) %>%  group_by(Sector,anio, e03,e04) %>% mutate(w=h001a/sum(h001a,na.rm=TRUE), Vaw=a131a_real/h001a,W=mean(w,na.rm=TRUE),A=mean(Vaw,na.rm=TRUE),Prod=(w-W)*(Vaw-A)) %>% summarise_at(vars(Prod,Vaw), funs(sum(.,na.rm=TRUE),mean(.,na.rm=TRUE))) %>% select(Sector,anio, e03,e04, Prod_sum, Vaw_mean) %>% rename(Prod=Prod_sum, Vaw=Vaw_mean) %>% group_by(Sector, anio, e03) %>% summarise_at(vars(Prod,Vaw),funs(sum(mean(.,na.rm=TRUE)))) %>% group_by(Sector,anio) %>% summarise_at(vars(Prod,Vaw), funs(mean(.,na.rm=TRUE))) %>% rename(Prod_firm=Prod, Vaw_firm=Vaw)->Firm_sectors_2split

Base_real %>% ungroup() %>% filter(!is.na(Vaw) & !is.na(a131a_real)) %>% select(-e17) %>% mutate(Sector=replace(Sector,which(Sector!=1),0)) %>% group_by(Sector,anio) %>% summarise_at(vars(a131a_real,h001a), funs(sum(.,na.rm=TRUE))) %>% mutate(Vaw=a131a_real/h001a) %>% select(Sector,anio, Vaw)->Total_sectors_2split

Total_sectors_2split %>% left_join(State_sectors_2split, by=c("Sector","anio")) %>% left_join(Mun_sectors_2split, by=c("Sector","anio")) %>% left_join(Firm_sectors_2split, by=c("Sector","anio"))->Decomposition_sectors_2split

data.table::fwrite(Decomposition_sectors_2split, "Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Growth_Decomposition_Sectors_clean_2split.csv")

############################################################################################################################################################################
# Herfindahl

Base_real<-fst::read.fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Base_cleaning_rse_2.fst", columns=c("anio","e03","e04","e17","SCIAN2dig","a131a_real","h001a","q000a_real","q000c_real"))
Base_real %>% left_join(Sectors, by="e17")->Base_real

Base_real %>% select(-SCIAN2dig) %>% group_by(anio,Sector) %>% mutate(s_emp=h001a/sum(h001a,na.rm=TRUE),s_stock=q000a_real/sum(q000a_real,na.rm=TRUE),s_inv=q000c_real/sum(q000c_real,na.rm=TRUE)) %>% summarise_each(funs(sum(.^2,na.rm=TRUE),N=n()),starts_with("s")) %>% mutate_each(funs(.*10000), ends_with("sum")) %>% rename(Herf_emp=s_emp_sum,Herf_stock=s_stock_sum,Herf_inv=s_inv_sum)->Herfindahl_anual
Herfindahl_anual %>% rename(N=s_emp_N) %>% select(-starts_with("s_"))->Herfindahl_anual_res
data.table::fwrite(Herfindahl_anual_res,file="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_anual_1.csv")               

Base_real %>% select(-c(e17,SCIAN2dig)) %>% group_by(anio,e03,e04,Sector) %>% mutate(s_emp=h001a/sum(h001a,na.rm=TRUE),s_stock=q000a_real/sum(q000a_real,na.rm=TRUE),s_inv=q000c_real/sum(q000c_real,na.rm=TRUE)) %>% summarise_each(funs(sum(.^2,na.rm=TRUE),N=n()),starts_with("s")) %>% mutate_each(funs(.*10000), ends_with("sum")) %>% rename(Herf_emp=s_emp_sum,Herf_stock=s_stock_sum,Herf_inv=s_inv_sum)->Herfindahl_mun
Herfindahl_mun %>% filter(s_emp_N>4) %>% rename(N=s_emp_N) %>% select(-starts_with("s_"))->Herfindahl_mun_res
data.table::fwrite(Herfindahl_mun_res,file="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_municipality_1.csv")

Base_real %>% select(-c(SCIAN2dig)) %>% group_by(anio,e03,e04,e17) %>% mutate(s_emp=h001a/sum(h001a,na.rm=TRUE),s_stock=q000a_real/sum(q000a_real,na.rm=TRUE),s_inv=q000c_real/sum(q000c_real,na.rm=TRUE)) %>% summarise_each(funs(sum(.^2,na.rm=TRUE),N=n()),starts_with("s")) %>% mutate_each(funs(.*10000), ends_with("sum")) %>% rename(Herf_emp=s_emp_sum,Herf_stock=s_stock_sum,Herf_inv=s_inv_sum)->Herfindahl_mun_sector
Herfindahl_mun_sector %>% rename(N=s_emp_N) %>% select(-starts_with("s"))->Herfindahl_mun_sector

# Regresiones de Herfindahl por Sector a 6 dígitos.

Herfindahl_mun_sector %>% left_join(Sectors,by="e17")->Herfindahl_mun_sector
#ols1<-lm(Herf_emp~D_Manuf*as.factor(anio),data=Herfindahl_mun_sector)
#ols2<-lm(Herf_va~D_Manuf*as.factor(anio),data=Herfindahl_mun_sector_res)
#ols3<-lm(Herf_stock~D_Manuf*as.factor(anio),data=Herfindahl_mun_sector)
#ols4<-lm(Herf_inv~D_Manuf*as.factor(anio),data=Herfindahl_mun_sector)


ols5<-felm(Herf_emp~as.factor(Sector)*as.factor(anio)| 0| 0 | e17,data=Herfindahl_mun_sector)
ols7<-felm(Herf_stock~as.factor(Sector)*as.factor(anio)| 0| 0 | e17,data=Herfindahl_mun_sector)
ols8<-felm(Herf_inv~as.factor(Sector)*as.factor(anio)| 0| 0 | e17,data=Herfindahl_mun_sector)

#stargazer(ols1,ols2,ols3,ols4,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-19/Herfindahl_sector.txt")
#stargazer(ols1,ols2,ols3,ols4,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-06-19/Herfindahl.tex")

stargazer(ols5,ols7,ols8,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_sector_1_1.txt")
stargazer(ols5,ols7,ols8,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_sector_1_1.tex")

############################################

# Herfindahl regressions, municipality level.

ols1<-lm(Herf_emp~as.factor(Sector)*as.factor(anio),data=Herfindahl_mun)
ols3<-lm(Herf_stock~as.factor(Sector)*as.factor(anio),data=Herfindahl_mun)
ols4<-lm(Herf_inv~as.factor(Sector)*as.factor(anio),data=Herfindahl_mun)


ols5<-felm(Herf_emp~as.factor(Sector)*as.factor(anio)| 0| 0 |Sector,data=Herfindahl_mun)
ols7<-felm(Herf_stock~as.factor(Sector)*as.factor(anio)| 0| 0 |Sector,data=Herfindahl_mun)
ols8<-felm(Herf_inv~as.factor(Sector)*as.factor(anio)| 0| 0 |Sector ,data=Herfindahl_mun)

stargazer(ols1,ols3,ols4,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl.txt")
stargazer(ols1,ols3,ols4,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl.tex")

stargazer(ols5,ols7,ols8,type="text",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_1_1.txt")
stargazer(ols5,ols7,ols8,type="latex",title = "Herfindahl Index Regressions",align=TRUE,omit.stat = c("ser") ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Herfindahl_1_1.tex")

############################################################################################################################################################################
# Descriptive Statistics 

Base_real<-fst::read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Base_cleaning_rse_2.fst")

Base_real %>% left_join(Sectors, by=c("e17"))->Base_real

# Tabstat, adding extra descriptive statistics. (SD, max, min )// by sector

Base_real %>% rename(h001a_real=h001a,h001d_real=h001d, Vaw_real=Vaw) %>%  group_by(anio,Sector) %>%  summarise_each(funs(Avg=mean(.,na.rm = TRUE),Sd=sd(.,na.rm=TRUE),Min=min(.,na.rm=TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),Max=max(.,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),ends_with("real")) %>% gather(key="Variable",value="Value",-c(anio, Sector)) %>% separate(col=Variable,into=c("Variable",NA,"stat"),sep="_")  %>% spread(key="Variable",value="Value")%>% arrange(anio, Sector)->tabstatreal

data.table::fwrite(tabstatreal,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Descriptives_sector_1.csv")

# Second split

Base_real %>% mutate(Sector=replace(Sector,which(Sector!=1),0)) %>% filter(Sector==0) %>% rename(h001a_real=h001a,h001d_real=h001d, Vaw_real=Vaw) %>%  group_by(anio,Sector) %>%  summarise_each(funs(Avg=mean(.,na.rm = TRUE),Sd=sd(.,na.rm=TRUE),Min=min(.,na.rm=TRUE),p1=quantile(.,.01,na.rm=TRUE),p10=quantile(.,.1,na.rm=TRUE),p25=quantile(.,.25,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p75=quantile(.,.75,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p99=quantile(.,.99,na.rm=TRUE),Max=max(.,na.rm=TRUE),N=n(),"NA's"=sum(is.na(.))),ends_with("real")) %>% gather(key="Variable",value="Value",-c(anio, Sector)) %>% separate(col=Variable,into=c("Variable",NA,"stat"),sep="_")  %>% spread(key="Variable",value="Value")%>% arrange(anio, Sector)->tabstatreal1

data.table::fwrite(tabstatreal1,"Z:/Resultados/LM-607-PCV-282-CE-2019-07-25/Descriptives_sector_2split.csv")


############################################################################################################################################################################

# New plots

#%>% gather(key="Variable",value="Value",-anio)) %>% separate(col=Variable,into=c("Variable",NA,"stat"),sep="_")

# Indices by decile (10th, 50th, 90th, 95th) for different variables (Vaw, Sales, Investment, Employees, Capital stock( machinery only), Capital stock(ICT equipment))
# at different levels(Whole Economy,Manufacturing vs Commerce & Services, Manufacturing vs Tradable Services vs Non tradable Services ) . Sales has to be defined

Base_real %>% ungroup() %>% mutate(sales_real=m010a_real+m020a_real+m030a_real) %>%  select(anio,h001a,q000c_real,sales_real,q010a_real,q400a_real,Vaw) %>% rename(h001a_real=h001a,Vaw_real=Vaw) %>% group_by(anio) %>%  summarise_all(funs(p10=quantile(.,.10,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p95=quantile(.,.95,na.rm=TRUE))) %>% ungroup() %>% mutate_all(funs(Index=./first(.)-1))

# Manufacturing vs Commerce & Services
Base_real %>% ungroup() %>% mutate(sales_real=m010a_real+m020a_real+m030a_real) %>%  mutate(Manuf=if_else(SCIAN2dig=="30",1,0)) %>% select(anio,Manuf,h001a,q000c_real,sales_real,q010a_real,q400a_real,Vaw) %>% rename(h001a_real=h001a,Vaw_real=Vaw) %>% group_by(anio,Manuf) %>%  summarise_all(funs(p10=quantile(.,.10,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p95=quantile(.,.95,na.rm=TRUE))) %>%  group_by(Manuf) %>% mutate_all(funs(Index=./first(.)-1))

# Manufacturing vs Tradable Services vs Non-tradable Services
Base_real %>% ungroup() %>%  mutate(sales_real=m010a_real+m020a_real+m030a_real) %>% left_join(Sectors,by="e17") %>% filter(Sector!=1) %>% select(anio,Sector,h001a,q000c_real,sales_real,q010a_real,q400a_real,Vaw) %>% rename(h001a_real=h001a,Vaw_real=Vaw) %>% group_by(anio, Sector) %>%  summarise_all(funs(p10=quantile(.,.10,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p95=quantile(.,.95,na.rm=TRUE))) %>% group_by(Sector) %>% mutate_all(funs(Index=./first(.)-1))

# Manufacturing vs Knowledge Based Services vs Non-Knowledge Based Services}

Base_real %>% ungroup() %>% mutate(sales_real=m010a_real+m020a_real+m030a_real) %>% filter(SCIAN!="30") %>% mutate(Kbs=if_else(SCIAN2dig=="51" | SCIAN2dig=="52" | SCIAN2dig=="54" | SCIAN2dig=="55" | SCIAN2dig=="61" | SCIAN2dig=="62" | SCIAN2dig=="71",1,0)) %>% select(anio,Kbs,h001a,q000c_real,sales_real,q010a_real,q400a_real,Vaw) %>% rename(h001a_real=h001a,Vaw_real=Vaw) %>% group_by(anio, Kbs) %>%  summarise_all(funs(p10=quantile(.,.10,na.rm=TRUE),p50=quantile(.,.50,na.rm=TRUE),p90=quantile(.,.9,na.rm=TRUE),p95=quantile(.,.95,na.rm=TRUE)))%>% group_by(Kbs) %>% mutate_all(funs(Index=./first(.)-1))