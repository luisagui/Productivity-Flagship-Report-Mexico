
# Script to compute  rank order weighted mean by state.

Emp<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Share_emp_Mun_SCIAN4.csv")
Emp %>% group_by(anio, e03, e04) %>% summarise_at(vars(h001a), funs(sum(.,na.rm=TRUE))) %>% group_by(anio,e03) %>% mutate(share_mun=h001a/sum(h001a,na.rm=TRUE))->Emp

rank_order<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/RAnk_order_Mun_6.csv")
rank_order %>% left_join(Emp, by=c("anio","e03","e04")) %>% mutate(rxs=rank_order*share_mun) %>% group_by(anio, e03) %>% summarise_at(vars(rxs),funs(sum(.,na.rm=TRUE))) %>% rename(rank_order=rxs)->rank_order_weighted

data.table::fwrite(rank_order_weighted,"C:/Users/pc Luis/Documents/World Bank/Resultados/Rank_order_weighted.csv")


rank_order %>% select(-starts_with("N")) %>% group_by(anio) %>% mutate(p1=quantile(rank_order,.10,na.rm=TRUE),p2=quantile(rank_order,.2,na.rm=TRUE),p3=quantile(rank_order,.3,na.rm=TRUE),p4=quantile(rank_order,.40,na.rm=TRUE),p5=quantile(rank_order,.50,na.rm=TRUE),p6=quantile(rank_order,.60,na.rm=TRUE),p7=quantile(rank_order,.70,na.rm=TRUE),p8=quantile(rank_order,.80,na.rm=TRUE),p9=quantile(rank_order,.90,na.rm=TRUE))->rank_order
rank_order %>% mutate(decil=1,decil=if_else(rank_order>p1 & rank_order<=p2,2,if_else(rank_order>p2 & rank_order<=p3,3,if_else(rank_order>p3 & rank_order<=p4,4,if_else(rank_order>p4 & rank_order<=p5,5,if_else(rank_order>p5 & rank_order<=p6,6,if_else(rank_order>p6 & rank_order<=p7,7,if_else(rank_order>p7 & rank_order<=p8,8,if_else(rank_order>p8 & rank_order<=p9,9,10)))))))))->rank_order
rank_order %>% group_by(anio,e03,decil) %>% summarise_at(vars(rank_order),funs(n())) %>% group_by(anio,e03) %>% mutate(share=rank_order/sum(rank_order,na.rm=TRUE))->rank_order_share

data.table::fwrite(rank_order_share,"C:/Users/pc Luis/Documents/World Bank/Resultados/Rank_order_share.csv")
