# Script to compute probit about change in importance of an industry(6dig, 4dig, 2dig) within municipality(State).

# After discussing about the models to ususe for the regressins, we decided to drop size dummies and capital, the interaction between share_ex with   will be included.

require(dplyr)
require(tidyr)
require(stargazer)

loghyper<-function(k){
  a<-log(k+sqrt(k^2+1))
  return(a)
}

Base_real<-fst::read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Base_cleaning_rse_3.fst", columns = c("anio","CDE","e03","e04","e17","SCIAN2dig","h001a","a131a_real","sales","q000a_real","q000c_real","q400c_real"))
Base_exports<-fst::read_fst("Z:/Procesamiento/Trabajo/Luis A/Panel_Censos/Base_cleaning_rse_exports.fst")
Base_exports %>% select(-c(e03,e04,e17,SCIAN2dig))->Base_exports

# Distance to US border

dist_us<-haven::read_dta("Z:/Procesamiento/Trabajo/Luis A/Drive time to the border municipality (hours).dta")
dist_us %>% select(-ent)->dist_us


# We need to compute total sales and intermidiates spliting between foreign and domestic.

Base_exports %>% replace_na(list(m010b_real=0,m020b_real=0,m030b_real=0,k010b_real=0,k020b_real=0,k030b_real=0,m010c_real=0,m020c_real=0,m030c_real=0,k010c_real=0,k020c_real=0,k030c_real=0)) %>% mutate(sales_nac=m010b_real+m020b_real+m030b_real,sales_ex=m010c_real+m020c_real+m030c_real,Intermidiate_nac=k010b_real+k020b_real+k030b_real,Intermidiate_ex=k010c_real+k020c_real+k030c_real) %>% select(anio, CDE,sales_nac,sales_ex,Intermidiate_nac,Intermidiate_ex)->Base_exports_2merge

Base_real %>% left_join(Base_exports_2merge, by=c("anio","CDE"))->Base_real

# Transition probilities are compute at municipality-industry level. Transition is measured in the long run, i.e. 1994-2014, so only two years are needed.
# Herfindahl can be computed separately

Base_real %>% filter((anio==1994 | anio==2014) & h001a!=0) %>% group_by(anio, e03, e04, e17) %>% summarise_at(vars(-c(CDE,SCIAN2dig)),funs(sum(.,na.rm=TRUE))) %>% group_by(anio,e03,e04) %>% mutate(share=h001a/sum(h001a,na.rm=TRUE)) %>%  mutate(Decile1=quantile(share,.1,na.rm=TRUE),Decile2=quantile(share,.2,na.rm=TRUE),Decile3=quantile(share,.3,na.rm=TRUE),Decile4=quantile(share,.4,na.rm=TRUE),Decile5=quantile(share,.5,na.rm=TRUE),Decile6=quantile(share,.6,na.rm=TRUE),Decile7=quantile(share,.7,na.rm=TRUE),Decile8=quantile(share,.8,na.rm=TRUE),Decile9=quantile(share,.9,na.rm=TRUE),D=if_else(share<=Decile1,1,if_else(share>Decile1 & share<=Decile2,2,if_else(share>Decile2 & share<=Decile3,3,if_else(share>Decile3 & share<=Decile4,4,if_else(share>Decile4 & share<=Decile5,5,if_else(share>Decile5 & share<=Decile6,6,if_else(share>Decile6 & share<=Decile7,7,if_else(share>Decile7 & share<=Decile8,8,if_else(share>Decile8 & share<=Decile9,9,10))))))))),share_ex=sales_ex/(sales_nac+sales_ex),K_2_sales=if_else(q000a_real!=0 & sales==0,-9999,if_else(q000a_real==0 & sales==0,0,q000a_real/sales))) %>% na_if(-9999)  %>% select(-starts_with("Decile"))->Base_Mun_6

Base_Mun_6 %>% ungroup() %>%  group_by(anio, e17) %>% summarise_at(vars(q000a_real,sales,sales_ex,sales_nac), funs(sum(.,na.rm=TRUE))) %>% mutate(share_ex_country=sales_ex/(sales_nac+sales_ex),K_2_sales_country=if_else(q000a_real!=0 & sales==0,-9999,if_else(q000a_real==0 & sales==0,0,q000a_real/sales)))->Variables_country
Variables_country %>% filter(anio==2014) %>% rename_at(vars(-c(anio,e17)),funs(paste(.,"_2014",sep=""))) %>% ungroup() %>%  select(e17,ends_with("_2014"))->Variables_country_2014
Variables_country %>% filter(anio==1994) %>% left_join(Variables_country_2014, by=c("e17")) %>% mutate(D.share_ex=share_ex_2014-share_ex, D.l.K_2_sales=loghyper(K_2_sales_2014)-loghyper(K_2_sales))->Variables_Country

Base_Mun_6 %>% filter(anio==2014) %>% rename_at(vars(-c(anio,e03,e04,e17)),funs(paste(.,"_2014",sep=""))) %>% ungroup() %>%  select(e03,e04,e17,ends_with("_2014"))-> Base_Mun_2014

Base_Mun_6 %>% filter(anio==1994) %>% left_join(Base_Mun_2014, by=c("e03","e04","e17")) %>% rename(D_1994=D) %>% mutate(Jump=if_else(abs(D_2014-D_1994)>=2,1,0),Jump_up=if_else((D_2014-D_1994)>=2,1,0),Jump_down=if_else((D_2014-D_1994)<=-2,1,0))->Base_Mun_6

# Herfindahl

Base_real %>% filter(anio==1994 | anio==2014) %>% group_by(anio,e03,e04,e17) %>% mutate(s_emp=h001a/sum(h001a,na.rm=TRUE)) %>% summarise_each(funs(sum(.^2,na.rm=TRUE),N=n()),s_emp) %>% rename(Herf_emp=sum)->Herfindahl
Herfindahl %>% ungroup() %>% filter(anio==2014) %>% rename(Herf_emp_14=Herf_emp,N_14=N) %>% select(-anio)->Herfindahl_14
Herfindahl %>% filter(anio==1994) %>% left_join(Herfindahl_14, by=c("e03","e04","e17"))->Herfindahl

# "Basic instruments" for selecterd variables, basically dependent variables will be computed at country level instead of municipality level
# Once computed they have to be multiplied by employment share of industry within municipality.
# I need to compute: Exports_share, Capital/sales, Herfindahl and their Deltas as well.(Those are computed in Variables_country)

  # Herfindahl

  Base_real %>% filter(anio==1994 | anio==2014) %>% group_by(anio,e17) %>% mutate(s_emp=h001a/sum(h001a,na.rm=TRUE)) %>% summarise_each(funs(sum(.^2,na.rm=TRUE),N=n()),s_emp) %>% rename(Herf_emp_country=sum)->Herfindahl_country
  Herfindahl_country %>% ungroup() %>% filter(anio==2014) %>% rename(Herf_emp_country_14=Herf_emp_country,N_14=N) %>% select(-anio)->Herfindahl_country_14
  Herfindahl_country %>% filter(anio==1994) %>% left_join(Herfindahl_country_14, by=c("e17")) %>% mutate(D.Herf_emp_country=Herf_emp_country_14-Herf_emp_country)->Herfindahl_country

# Final database for regressions
  # Exports share has to be multiplied by 100.
  
Base_Mun_6 %>% unite(id_mun,e03,e04,sep="",remove=FALSE) %>% left_join(dist_us,by=c("id_mun")) %>% left_join(Herfindahl,by=c("anio","e03","e04","e17")) %>% left_join(Herfindahl_country, by=c("anio","e17")) %>% left_join(Variables_country,by=c("anio","e17")) %>% group_by(anio,e03,e04) %>% mutate(share_emp=h001a/sum(h001a,na.rm=TRUE),SCIAN2dig=substr(e17,1,2),SCIAN2dig=if_else(SCIAN2dig=="31" | SCIAN2dig=="32" |SCIAN2dig=="33","30",SCIAN2dig)) %>% mutate_at(vars(contains("share_ex")),funs(.*100))->Base_Mun_6

# Regressions (Levels and changes), with controls

# Deciles

  # Levels
  
    # Jump
    lpm.model1<-lm(Jump~share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2<-lm(Jump~loghyper(K_2_sales)+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3<-lm(Jump~durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4<-lm(Jump~Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5<-lm(Jump~share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6<-lm(Jump~share_ex+loghyper(K_2_sales)+durat_border_hrs+Herf_emp+share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1,lpm.model2,lpm.model3,lpm.model4,lpm.model5,lpm.model6,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_Deciles_1_1.txt")
    stargazer(lpm.model1,lpm.model2,lpm.model3,lpm.model4,lpm.model5,lpm.model6,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_Deciles_1_1.tex")
    
    # Jump up
    
    lpm.model1_up<-lm(Jump_up~share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2_up<-lm(Jump_up~loghyper(K_2_sales)+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3_up<-lm(Jump_up~durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_up<-lm(Jump_up~Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5_up<-lm(Jump_up~share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6_up<-lm(Jump_up~share_ex+loghyper(K_2_sales)+durat_border_hrs+Herf_emp+share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1_up,lpm.model2_up,lpm.model3_up,lpm.model4_up,lpm.model5_up,lpm.model6_up,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_Deciles_1_1.txt")
    stargazer(lpm.model1_up,lpm.model2_up,lpm.model3_up,lpm.model4_up,lpm.model5_up,lpm.model6_up,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_Deciles_1_1.tex")
    
    # Jump down
    
    lpm.model1_down<-lm(Jump_down~share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2_down<-lm(Jump_down~loghyper(K_2_sales)+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3_down<-lm(Jump_down~durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_down<-lm(Jump_down~Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_down<-lm(Jump_down~share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6_down<-lm(Jump_down~share_ex+loghyper(K_2_sales)+durat_border_hrs+Herf_emp+share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1_down,lpm.model2_down,lpm.model3_down,lpm.model4_down,lpm.model5_down,lpm.model6_down,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_Deciles_1_1.txt")
    stargazer(lpm.model1_down,lpm.model2_down,lpm.model3_down,lpm.model4_down,lpm.model5_down,lpm.model6_down,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_Deciles_1_1.tex")
    
    # Changes
    Base_Mun_6 %>% mutate(D.share_ex=share_ex_2014-share_ex,D.l.K=loghyper(q000a_real_2014)-loghyper(q000a_real),D.l.K_2_sales=loghyer(K_2_sales_2014)-loghyper(K_2_sales),D.Herf_emp=Herf_emp_14-Herf_emp)->Base_Mun_6
  
    # Jump
    lpm.model1_changes<-lm(Jump~D.share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2_changes<-lm(Jump~D.l.K_2_sales+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3_changes<-lm(Jump~D.Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_changes<-lm(Jump~D.share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5_changes<-lm(Jump~D.share_ex+D.l.K_2_sales+durat_border_hrs+Herf_emp+D.share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1_changes,lpm.model2_changes,lpm.model3_changes,lpm.model4_changes,lpm.model5_changes,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_changes_Deciles_1_1.txt")
    stargazer(lpm.model1_changes,lpm.model2_changes,lpm.model3_changes,lpm.model4_changes,lpm.model5_changes,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_changes_Deciles_1_1.tex")
    
    # Jump up
    
    lpm.model1_up_changes<-lm(Jump_up~D.share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2_up_changes<-lm(Jump_up~D.l.K_2_sales+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3_up_changes<-lm(Jump_up~D.Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_up_changes<-lm(Jump_up~D.share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5_up_changes<-lm(Jump_up~D.share_ex+D.l.K_2_sales+durat_border_hrs+Herf_emp+D.share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1_up_changes,lpm.model2_up_changes,lpm.model3_up_changes,lpm.model4_up_changes,lpm.model5_up_changes,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_changes_Deciles_1_1.txt")
    stargazer(lpm.model1_up_changes,lpm.model2_up_changes,lpm.model3_up_changes,lpm.model4_up_changes,lpm.model5_up_changes,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_changes_Deciles_1_1.tex")
    
    # Jump down
    
    lpm.model1_down_changes<-lm(Jump_down~D.share_ex+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2_down_changes<-lm(Jump_down~D.l.K_2_sales+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3_down_changes<-lm(Jump_down~D.Herf_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4_down_changes<-lm(Jump_down~D.share_ex:durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5_down_changes<-lm(Jump_down~D.share_ex+D.l.K_2_sales+durat_border_hrs+Herf_emp+D.share_ex*durat_border_hrs+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1_down_changes,lpm.model2_down_changes,lpm.model3_down_changes,lpm.model4_down_changes,lpm.model5_down_changes,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_changes_Deciles_1_1.txt")
    stargazer(lpm.model1_down_changes,lpm.model2_down_changes,lpm.model3_down_changes,lpm.model4_down_changes,lpm.model5_down_changes,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_changes_Deciles_1_1.tex")

    ## New regressions
    
    # Jump
    lpm.model1.bi<-lm(Jump~share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2.bi<-lm(Jump~D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3.bi<-lm(Jump~share_ex_country:share_emp+D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4.bi<-lm(Jump~loghyper(K_2_sales_country):share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5.bi<-lm(Jump~D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6.bi<-lm(Jump~loghyper(K_2_sales_country):share_emp+D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model7.bi<-lm(Jump~Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model8.bi<-lm(Jump~D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model9.bi<-lm(Jump~Herf_emp_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model10.bi<-lm(Jump~share_ex_country:share_emp+loghyper(K_2_sales_country):share_emp+Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model11.bi<-lm(Jump~D.share_ex_country:share_emp+D.l.K_2_sales_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1.bi,lpm.model2.bi,lpm.model3.bi,lpm.model4.bi,lpm.model5.bi,lpm.model6.bi,lpm.model7.bi,lpm.model8.bi,lpm.model9.bi,lpm.model10.bi,lpm.model11.bi,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_Deciles_bi.txt")
    stargazer(lpm.model1.bi,lpm.model2.bi,lpm.model3.bi,lpm.model4.bi,lpm.model5.bi,lpm.model6.bi,lpm.model7.bi,lpm.model8.bi,lpm.model9.bi,lpm.model10.bi,lpm.model11.bi,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_Deciles_bi.tex")
    
    # Jump up
    
    lpm.model1.bi_up<-lm(Jump_up~share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2.bi_up<-lm(Jump_up~D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3.bi_up<-lm(Jump_up~share_ex_country:share_emp+D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4.bi_up<-lm(Jump_up~loghyper(K_2_sales_country):share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5.bi_up<-lm(Jump_up~D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6.bi_up<-lm(Jump_up~loghyper(K_2_sales_country):share_emp+D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model7.bi_up<-lm(Jump_up~Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model8.bi_up<-lm(Jump_up~D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model9.bi_up<-lm(Jump_up~Herf_emp_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model10.bi_up<-lm(Jump_up~share_ex_country:share_emp+loghyper(K_2_sales_country):share_emp+Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model11.bi_up<-lm(Jump_up~D.share_ex_country:share_emp+D.l.K_2_sales_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1.bi_up,lpm.model2.bi_up,lpm.model3.bi_up,lpm.model4.bi_up,lpm.model5.bi_up,lpm.model6.bi_up,lpm.model7.bi_up,lpm.model8.bi_up,lpm.model9.bi_up,lpm.model10.bi_up,lpm.model11.bi_up,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_Deciles_bi.txt")
    stargazer(lpm.model1.bi_up,lpm.model2.bi_up,lpm.model3.bi_up,lpm.model4.bi_up,lpm.model5.bi_up,lpm.model6.bi_up,lpm.model7.bi_up,lpm.model8.bi_up,lpm.model9.bi_up,lpm.model10.bi_up,lpm.model11.bi_up,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_up_Deciles_bi.tex")
    
    # Jump down
    
    lpm.model1.bi_down<-lm(Jump_down~share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model2.bi_down<-lm(Jump_down~D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model3.bi_down<-lm(Jump_down~share_ex_country:share_emp+D.share_ex_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model4.bi_down<-lm(Jump_down~loghyper(K_2_sales_country):share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model5.bi_down<-lm(Jump_down~D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model6.bi_down<-lm(Jump_down~loghyper(K_2_sales_country):share_emp+D.l.K_2_sales_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model7.bi_down<-lm(Jump_down~Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model8.bi_down<-lm(Jump_down~D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model9.bi_down<-lm(Jump_down~Herf_emp_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model10.bi_down<-lm(Jump_down~share_ex_country:share_emp+loghyper(K_2_sales_country):share_emp+Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    lpm.model11.bi_down<-lm(Jump_down~D.share_ex_country:share_emp+D.l.K_2_sales_country:share_emp+D.Herf_emp_country:share_emp+factor(SCIAN2dig)+factor(e03),data=Base_Mun_6)
    
    stargazer(lpm.model1.bi_down,lpm.model2.bi_down,lpm.model3.bi_down,lpm.model4.bi_down,lpm.model5.bi_down,lpm.model6.bi_down,lpm.model7.bi,lpm.model8.bi_down,lpm.model9.bi_down,lpm.model10.bi_down,lpm.model11.bi_down,align = TRUE, type="text",out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_Deciles_bi.txt")
    stargazer(lpm.model1.bi_down,lpm.model2.bi_down,lpm.model3.bi_down,lpm.model4.bi_down,lpm.model5.bi,lpm.model6.bi_down,lpm.model7.bi_down,lpm.model8.bi_down,lpm.model9.bi_down,lpm.model10.bi_down,lpm.model11.bi_down,align = TRUE ,out="Z:/Resultados/LM-607-PCV-282-CE-2019-08-27/P_Jump_down_Deciles_bi.tex")
    
    