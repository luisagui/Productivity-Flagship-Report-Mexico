# Scripto to plot TFP and Value added mesures by entry-surv -exit.

require(dplyr)
require(tidyr)
require(ggplot2)

#### Productivity Means and Deviations plots ####

Prod_Ent_Surv<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Mean_dev_Productivity_by_Entry_Surv_Exit.csv")

# TFP sm and wm
Prod_Ent_Surv %>%
  select(anio,TFP_Ent_sm,TFP_Ent_wm,TFP_Surv_sm,TFP_Surv_wm,TFP_Exit_sm,TFP_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure","Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge2")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+ggtitle("TFP")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/TFP_means_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )
  
# TFP deviations sm and wm (year-state-sector-size mean)
Prod_Ent_Surv %>%
  select(anio,TFP_dev_Ent_sm,TFP_dev_Ent_wm,TFP_dev_Surv_sm,TFP_dev_Surv_wm,TFP_dev_Exit_sm,TFP_dev_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("TFP, mean deviations from simple and weighted mean",subtitle = "State-Sector-size mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/TFP_means_deviations_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )

# TFP deviations sm and wm (year-state-sector mean)
Prod_Ent_Surv %>%
  select(anio,TFP_dev_st_se_Ent_sm,TFP_dev_st_se_Ent_wm,TFP_dev_st_se_Surv_sm,TFP_dev_st_se_Surv_wm,
         TFP_dev_st_se_Exit_sm,TFP_dev_st_se_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,NA,NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("TFP, mean deviations from simple and weighted mean",subtitle = "State-Sector mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/TFP_means_deviations_st_se_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )


# TFP deviations sm and wm (year-sector mean)
Prod_Ent_Surv %>%
  select(anio,TFP_dev_se_Ent_sm,TFP_dev_se_Ent_wm,TFP_dev_se_Surv_sm,TFP_dev_se_Surv_wm,
         TFP_dev_se_Exit_sm,TFP_dev_se_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("TFP, mean deviations from simple and weighted mean",subtitle = "Sector mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/TFP_means_deviations_se_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )


# Vaw sm and wm
Prod_Ent_Surv %>%
  select(anio,Vaw_Ent_sm,Vaw_Ent_wm,Vaw_Surv_sm,Vaw_Surv_wm,Vaw_Exit_sm,Vaw_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure","Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("Value added per worker")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/Vaw_means_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )


# Vaw deviation sm and wm

Prod_Ent_Surv %>%
  select(anio,Vaw_dev_Ent_sm,Vaw_dev_Ent_wm,Vaw_dev_Surv_sm,Vaw_dev_Surv_wm,Vaw_dev_Exit_sm,Vaw_dev_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("Value added per worker, mean deviations from simple and weighted mean",subtitle = "State-Sector-size mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/Vaw_means_deviations_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )


# Vaw deviations sm and wm (year-state-sector mean)
Prod_Ent_Surv %>%
  select(anio,Vaw_dev_st_se_Ent_sm,Vaw_dev_st_se_Ent_wm,Vaw_dev_st_se_Surv_sm,Vaw_dev_st_se_Surv_wm,
         Vaw_dev_st_se_Exit_sm,Vaw_dev_st_se_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,NA,NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("Vaw, mean deviations from simple and weighted mean",subtitle = "State-Sector mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/Vaw_means_deviations_st_se_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )

# Vaw deviations sm and wm (year-sector mean)
Prod_Ent_Surv %>%
  select(anio,Vaw_dev_se_Ent_sm,Vaw_dev_se_Ent_wm,Vaw_dev_se_Surv_sm,Vaw_dev_se_Surv_wm,
         Vaw_dev_se_Exit_sm,Vaw_dev_se_Exit_wm) %>%
  gather(key,value,-anio) %>% separate(key,c("Measure",NA,NA,"Ent","type")) %>%
  mutate(type=if_else(type=="sm","Simple mean","Weighted mean")) %>% 
  ggplot(aes(factor(anio),value,fill=Ent,label=round(value,2)))+
  geom_bar(stat="identity",position="dodge")+geom_text(position = position_dodge2(1),vjust=-.5)+facet_wrap(.~type)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Entrants","Exitors","Survivors"))+
  theme_bw()+theme(legend.position = "bottom")+
  ggtitle("Vaw, mean deviations from simple and weighted mean",subtitle = "Sector mean")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Productivity Means and deviations/Vaw_means_deviations_se_by_Ent_surv_Exit.jpg",
       width =10,height = 7, units = "in",dpi = "print" )

#### Herfindahl updated ####

herf<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/LM 607 Revisión 20 marzo 2020 (2)/Herfindahl_updated.csv")

# Tradable ans Non Tradable Services
herf %>% select(-ends_with("N")) %>% gather(key,value,-c(anio,Sector)) %>%
  filter(Sector=="NT_Services" | Sector=="T_Services") %>%
  mutate(Sector=if_else(Sector=="NT_Services","Non Tradable Services","Tradable Services")) %>%  
  ggplot(aes(factor(anio),value,fill=key,label=round(value,2)))+
  geom_bar(stat = "identity",position = "dodge2")+geom_text(position = position_dodge2(.9),size=2.5,vjust=-.5)+
  facet_wrap(.~Sector)+labs(x="",fill="")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+
  theme_bw()+theme(legend.position = "bottom")+ggtitle("Herfindahl")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Herfindahl_Services_updated.jpg",
       width =10,height = 7, units = "in",dpi = "print" )

# Manufacturing vs Commerce & Services
herf %>% select(-ends_with("N")) %>% gather(key,value,-c(anio,Sector)) %>%
  filter(Sector=="Manufacturing" | Sector=="Commerce & Services") %>%
  ggplot(aes(factor(anio),value,fill=key,label=round(value,2)))+
  geom_bar(stat = "identity",position = "dodge2")+geom_text(position = position_dodge2(.9),size=2.5,vjust=-.5)+
  facet_wrap(.~Sector)+labs(x="",fill="")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+
  theme_bw()+theme(legend.position = "bottom")+ggtitle("Herfindahl")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Herfindahl_Manuf_vs_Services_updated.jpg",
       width =10,height = 7, units = "in",dpi = "print" )
# All 
herf %>% select(-ends_with("N")) %>% gather(key,value,-c(anio,Sector)) %>%
  filter(Sector=="") %>%
  ggplot(aes(factor(anio),value,fill=key,label=round(value,2)))+
  geom_bar(stat = "identity",position = "dodge2")+geom_text(position = position_dodge2(.9),size=2.5,vjust=-.5)+
  #facet_wrap(.~Sector)+
  labs(x="",fill="")+scale_fill_discrete(labels=c("Employment","Investment","Stock"))+
  theme_bw()+theme(legend.position = "bottom")+ggtitle("Herfindahl")

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Herfindahl_updated.jpg",
       width =5,height = 5, units = "in",dpi = "print" )


#### Graphs by Size (Includes sales) ####

vars<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/LM 607 Revisión 20 marzo 2020 (2)/Vars_by_size_updated.csv")

# Employees
vars %>% filter(size!=0) %>% select(anio,size,Emp) %>% group_by(anio) %>%
  mutate(Emp=Emp/1000000,pct=Emp*100/sum(Emp)) %>%
  ggplot(aes(factor(anio),Emp,fill=factor(size),label=paste(round(pct,2),"%")))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
                    caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI. Value in millions.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))
  
ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Employment_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )

# Firms
vars %>% filter(size!=0) %>% select(anio,size,N) %>% group_by(anio) %>%
  mutate(pct=N*100/sum(N)) %>%
  ggplot(aes(factor(anio),N,fill=factor(size),label=paste(round(pct,2),"%")))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Firms_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )
# Sales
vars %>% filter(size!=0) %>% select(anio,size,sales) %>% group_by(anio) %>%
  mutate(sales=sales/1000000,pct=sales*100/sum(sales)) %>%
  ggplot(aes(factor(anio),sales,fill=factor(size),label=paste(round(pct,2),"%")))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI. Values in 2012 millions of thousands pesos.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Sales_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )

# Va/w
vars %>% filter(size!=0) %>% select(anio,size,Vaw) %>% group_by(anio) %>%
  ggplot(aes(factor(anio),Vaw,fill=factor(size),label=round(Vaw,2)))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI. Values in 2012 thousands pesos.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Vaw_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )

# Investment
vars %>% filter(size!=0) %>% select(anio,size,Inv) %>% group_by(anio) %>%
  mutate(Inv=Inv/1000000,pct=Inv*100/sum(Inv)) %>%
  ggplot(aes(factor(anio),Inv,fill=factor(size),label=paste(round(pct,2),"%")))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI. Values in 2012 millions of thousands pesos.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Investment_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )

# Stock
vars %>% filter(size!=0) %>% select(anio,size,Stock) %>% group_by(anio) %>%
  mutate(Stock=Stock/1000000,pct=Stock*100/sum(Stock)) %>%
  ggplot(aes(factor(anio),Stock,fill=factor(size),label=paste(round(pct,2),"%")))+
  geom_bar(stat="identity",position = "dodge2")+geom_text(size=3,position=position_dodge2(.9))+
  scale_fill_discrete(labels = c("1", "2-5", "6-10","11-20", "21-50", "51-100","101-250", "251+"))+
  coord_flip()+
  labs(x="",y="value",fill="Size",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI. Values in 2012 millions of thousands pesos.")+
  theme(legend.position="bottom",
        plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

  ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Graphs by size 2303/Stock_by_size.jpg",
       width=7,height =7,dpi = "print",units = "in" )

#### Capital per worker Decomposition ####
kw<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Decomposition_Kw.csv")  

kw %>% rename(a_lKw_wm=lKw_wm,b_lKw_mean=lKw_mean,c_Prodl_sum=Prodl_sum) %>%
  gather(key,value,-anio) %>% group_by(anio) %>% mutate(pct=value*200/sum(value)) %>% 
  ggplot(aes(factor(anio),value,fill=key,label=paste0(round(pct,2),"%")))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="",fill="",caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")+
  scale_fill_discrete(labels=c("Total","Within","Between"))+
  geom_text(size = 3, position = position_dodge(0.9),vjust=-.1)+
  ggtitle("Capital per worker Decomposition")+
  theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decompositions/Capital per worker/K_per_worker_Decomposition.jpg",
       dpi="print",width=7,height=7,units="in")

# Manuf vs Services

kw_sec<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Decomposition_Kw_sector.csv")  

kw_sec %>% rename(a_lKw_wm=lKw_wm,b_lKw_mean=lKw_mean,c_Prodl_sum=Prodl_sum) %>%
  mutate(Sector=if_else(Sector==1,"Manufacturing","Commerce & Services")) %>% 
  gather(key,value,-c(anio,Sector)) %>% group_by(anio,Sector) %>% mutate(pct=value*200/sum(value)) %>% 
  ggplot(aes(factor(anio),value,fill=key,label=paste0(round(pct,2),"%")))+
  geom_bar(stat="identity",position="dodge2")+facet_wrap(.~Sector)+
  labs(x="",fill="",caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")+
  scale_fill_discrete(labels=c("Total","Within","Between"))+
  geom_text(size = 2.5, position = position_dodge2(0.9),vjust=-.1)+
  ggtitle("Capital per worker Decomposition")+
  theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decompositions/Capital per worker/K_per_worker_sectors_Decomposition.jpg",
       dpi="print",width=10,height=7,units="in")


## Olley-Pakes
kw_op<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Growth_Decomposition_lKw_clean.csv")  

kw_op %>% 
  rename(a_lKw=lKw,b_Prodl_State_mean=Prodl_State,c_Prodl_Mun=Prodl_Mun,d_Prodl_firm=Prodl_firm,
         e_lKw_firm=lKw_firm) %>% 
  gather(key,value,-anio) %>%# group_by(anio) %>% mutate(pct=value*200/sum(value)) %>% 
  ggplot(aes(factor(anio),value,fill=key,label=round(value,3)))+
  geom_bar(stat="identity",position="dodge")+
  labs(x="",fill="",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")+
  scale_fill_discrete(labels=c("Total","Between States","Between Municipality","Between Firms","Within"))+
  geom_text(size = 3, position = position_dodge(0.9),vjust=-.1)+
  ggtitle("Capital per worker, Olley-Pakes Decomposition")+
  theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decompositions/Capital per worker/K_per_worker_OP_Decomposition.jpg",
       dpi="print",width=7,height=7,units="in")


# Manuf vs Services
kw_op_sec<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Growth_Decomposition_lKw_clean_sectors.csv")  

kw_op_sec %>% 
  rename(a_lKw=lKw,b_Prodl_State_mean=Prodl_State,c_Prodl_Mun=Prodl_Mun,d_Prodl_firm=Prodl_firm,
         e_lKw_firm=lKw_firm) %>% mutate(Sector=if_else(Sector==1,"Manufacturing","Commerce & Services")) %>%  
  gather(key,value,-c(anio,Sector)) %>%# group_by(anio,Sector) %>% mutate(pct=value*200/sum(value)) %>% 
  ggplot(aes(factor(anio),value,fill=key,label=round(value,3)))+
  geom_bar(stat="identity",position="dodge2")+facet_wrap(.~Sector)+
  labs(x="",fill="",
       caption = "Source: Author calculations with data from Mexico´s Economic Censuses 1994-2014, INEGI.")+
  scale_fill_discrete(labels=c("Total","Between States","Between Municipality","Between Firms","Within"))+
  geom_text(size = 2.2, position = position_dodge2(1),vjust=-.1)+
  ggtitle("Capital per worker, Olley-Pakes Decomposition")+
  theme(legend.position = "bottom",plot.caption=element_text(margin=margin(t=15),face="italic", size=8))

ggsave("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Decompositions/Capital per worker/K_per_worker_sectors_OP_Decomposition.jpg",
       dpi="print",width=10,height=7,units="in")
