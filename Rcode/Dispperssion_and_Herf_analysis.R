# Dispperssion and Herfindahl analyis.

require(dplyr)
require(tidyr)
require(ggplot2)

binscatter <- function(formula, key_var, data, bins=20, partial=FALSE){
  
  require(lfe)
  require(ggplot2)
  
  if(partial==TRUE){
    y <- unlist(strsplit(formula, "~"))[1]
    x <- unlist(strsplit(formula, "~"))[2]
    controls <- gsub(paste("[[:punct:]]*",key_var,"[[:space:]]*[[:punct:]]*",sep=""),
                     "",x)
    
    reg_all <- felm(formula(formula),data=data)
    reg_y <- felm(formula(paste(y, "~", controls, sep="")), data=data)
    reg_x <- felm(formula(paste(key_var, "~", controls, sep="")), data=data)
    resid_all <- resid(reg_all)
    resid_y <- resid(reg_y)
    resid_x <- resid(reg_x)
    df <- data.frame(resid_y, resid_x)
    cluster_grp <- trimws(unlist(strsplit(formula, "\\|"))[4])
    if(is.na(cluster_grp)){
      reg <- felm(resid_y ~ resid_x)  
    } else{
      data$resid_y <- resid_y
      data$resid_x <- resid_x
      reg <- felm(formula(paste("resid_y ~ resid_x | 0 | 0 |",
                                cluster_grp, sep="")), data=data)
    }
    newdata <- df
    colnames(df) <- c(paste("residual",names(df)[1]), paste("residual",names(df)[2]))
    
  } else if(partial==FALSE){
    reg <- felm(formula(formula),data=data)
    y <- trimws(unlist(strsplit(formula, "~"))[1])
    df <- data[,c(y,key_var)]
    newdata <- df  # To calculate CI of predicted mean
    #  colnames(df) <- c("resid_y", "resid_x")
  }
  intercept <- coef(reg)[1]
  slope <- coef(reg)[2]
  
  ### Use cluster vcov from the correct model, if not available, use robust
  if(is.null(reg$clustervcv)){
    vcov <- reg$robustvcv
    se_type <- "robust"
  } else {
    vcov <- reg$clustervcv
    se_type <- "cluster"
  }
  
  Terms <- terms(reg)
  m.mat <- model.matrix(Terms,data=newdata)
  fit <- as.vector(m.mat %*% coef(reg))
  se.fit <- sqrt(rowSums((m.mat %*% vcov) * m.mat)) 
  ## Much faster alternative to sqrt(diag(m.mat%*%vcov%*%t(m.mat))) and works fine since
  ## we only want the diagonal
  df$upper_ci <- fit + 1.96*se.fit
  df$lower_ci <- fit - 1.96*se.fit
  
  min_x <- min(df[,2])
  max_x <- max(df[,2])
  min_y <- intercept + min_x*slope
  max_y <- intercept + max_x*slope
  
  df_bin <- aggregate(df,by=list(cut(as.matrix(df[,2]),bins)), mean)
  
  ggplot(data=df, aes(x=df[,2], y=df[,1])) +
    #geom_point(alpha=0.2) + 
    geom_segment(aes(x = min_x, y = min_y, xend = max_x, yend = max_y),
                 color="blue", size=1) +
    geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci),alpha=0.18) +
    geom_point(data=df_bin, aes(x=df_bin[,3], y=df_bin[,2]), color="orange", size=2) +
    labs(caption = paste(" slope = ", signif(slope,2), sep=""),
         x = names(df)[2], y = names(df)[1]) +
    theme_classic()
}

Disp<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Data_for_dispperssion.csv")

# Measures of Dispperssion

Disp %>% mutate(Disp1=Vaw_P90-Vaw_P10,Disp2=Vaw_P75-Vaw_P25,Disp3=Vaw_sd/Vaw_mean)->Disp

# Gini measure supposed to be between 0-1, but it falls out this range if there are negative values in the variable
# we want to compute Gni, Vaw has negative valued so we cannot get a valid Gini.

# Measures to compare (We wil use Value added per worker growth , Vaw levels, Vaw initial levels, Change in Dispperssion measure, Within & Between component of Vaw growth)

# First I will plot scatters, If it is neccessary I will plot binscatter.

# Every measure in 1994 against Vaw growth

Disp %>% group_by(e17) %>% mutate(Vaw_g_94_14=if_else(lag(Vaw,4)!=0,Vaw/lag(Vaw,4)-1,-9999)) %>% na_if(-9999) %>% mutate_at(vars(starts_with("Disp"),Vaw_Gini),funs(l94=lag(.,4)))->Disp


Disp %>% ungroup() %>%  filter(Disp1_l94>quantile(Disp1_l94,.025,na.rm=TRUE) & Disp1_l94<quantile(Disp1_l94,.975,na.rm=TRUE)) %>% ggplot(aes(Disp1_l94,Vaw_g_94_14))+geom_point()+labs(x="P90-P10 1994",y="Vaw growth")
Disp %>% ungroup() %>%  filter(Disp2_l94>quantile(Disp2_l94,.025,na.rm=TRUE) & Disp2_l94<quantile(Disp2_l94,.975,na.rm=TRUE))%>% ggplot(aes(Disp2_l94,Vaw_g_94_14))+geom_point()+labs(x="P75-P25 1994",y="Vaw growth")
Disp %>% ungroup() %>%  filter(Disp3_l94>quantile(Disp3_l94,.025,na.rm=TRUE) & Disp3_l94<quantile(Disp3_l94,.975,na.rm=TRUE)) %>% ggplot(aes(Disp3_l94,Vaw_g_94_14))+geom_point()+labs(x="sd/Mean 1994",y="Vaw growth")
#Disp %>% ungroup() %>%  filter(Vaw_Gini_l94>quantile(Vaw_Gini_l94,.025,na.rm=TRUE) & Vaw_Gini_l94<quantile(Vaw_Gini_l94,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini_l94,Vaw_g_94_14))+geom_point()+labs(x="Gini 1994",y="Vaw growth")

# Every measure at time t with Vaw at time t.

Disp %>% ungroup() %>%  filter(Disp1>quantile(Disp1,.025,na.rm=TRUE) & Disp1<quantile(Disp1,.975,na.rm=TRUE)) %>% ggplot(aes(Disp1,Vaw))+geom_point()+labs(x="P90-P10",y="Vaw")
Disp %>% ungroup() %>%  filter(Disp2>quantile(Disp2,.025,na.rm=TRUE) & Disp2<quantile(Disp2,.975,na.rm=TRUE))%>% ggplot(aes(Disp2,Vaw))+geom_point()+labs(x="P75-P25",y="Vaw")
Disp %>% ungroup() %>%  filter(Disp3>quantile(Disp3,.025,na.rm=TRUE) & Disp3<quantile(Disp3,.975,na.rm=TRUE)) %>% ggplot(aes(Disp3,Vaw))+geom_point()+labs(x="sd/Mean",y="Vaw")
#Disp %>% ungroup() %>%  filter(Vaw_Gini>quantile(Vaw_Gini,.025,na.rm=TRUE) & Vaw_Gini<quantile(Vaw_Gini,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini,Vaw))+geom_point()+labs(x="Gini",y="Vaw")

# Every measure with its long change (94-14)

Disp %>% group_by(e17) %>% mutate_at(vars(starts_with("Disp"),Vaw_Gini),funs(Delta=.-lag(.,4)))->Disp

Disp %>% ungroup() %>%  filter(Disp1_Delta>quantile(Disp1_Delta,.025,na.rm=TRUE) & Disp1_Delta<quantile(Disp1_Delta,.975,na.rm=TRUE)) %>% ggplot(aes(Disp1,Disp1_Delta))+geom_point()+labs(x="P90-P10",y="Delta P90-P10")
Disp %>% ungroup() %>%  filter(Disp2_Delta>quantile(Disp2_Delta,.025,na.rm=TRUE) & Disp2_Delta<quantile(Disp2_Delta,.975,na.rm=TRUE))%>% ggplot(aes(Disp2,Disp2_Delta))+geom_point()+labs(x=" P75-P25",y="Delta P75-P25")
Disp %>% ungroup() %>%  filter(Disp3_Delta>quantile(Disp3_Delta,.025,na.rm=TRUE) & Disp3_Delta<quantile(Disp3_Delta,.975,na.rm=TRUE)) %>% ggplot(aes(Disp3,Disp3_Delta))+geom_point()+labs(x="sd/Mean",y="Delta sd/Mean")
#Disp %>% ungroup() %>%  filter(Vaw_Gini_Delta>quantile(Vaw_Gini_Delta,.025,na.rm=TRUE) & Vaw_Gini_Delta<quantile(Vaw_Gini_Delta,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini,Vaw_Gini_Delta))+geom_point()+labs(x="Gini",y="Delta Gini")

# Every measure with between and within component of Vaw

Disp %>% group_by(anio) %>% filter(Disp1>quantile(Disp1,.025,na.rm=TRUE) & Disp1<quantile(Disp1,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_mean,Disp1))+geom_point() + labs(x="Within",y="P90-P10")
Disp %>% group_by(anio) %>% filter(Disp2>quantile(Disp2,.025,na.rm=TRUE) & Disp2<quantile(Disp2,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_mean,Disp2))+geom_point() + labs(x="Within",y="P75-P25")
Disp %>% ungroup() %>% filter(Disp3>quantile(Disp3,.1,na.rm=TRUE) & Disp3<quantile(Disp3,.9,na.rm=TRUE)) %>%  ggplot(aes(Vaw_mean,Disp3))+geom_point() + labs(x="Within",y="sd/Mean")
#Disp %>% ungroup() %>% filter(Vaw_mean<quantile(Vaw_mean,.99,na.rm=TRUE) & Vaw_Gini>0  & Vaw_Gini<10) %>%  ggplot(aes(Vaw_mean,Vaw_Gini))+geom_point() + labs(x="Within",y="Gini")

# Every measure with between and Between component of Vaw

Disp %>% group_by(anio) %>% filter(Disp1>quantile(Disp1,.025,na.rm=TRUE) & Disp1<quantile(Disp1,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Cov,Disp1))+geom_point() + labs(x="Between",y="P90-P10")
Disp %>% group_by(anio) %>% filter(Disp2>quantile(Disp2,.025,na.rm=TRUE) & Disp2<quantile(Disp2,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Cov,Disp2))+geom_point() + labs(x="Between",y="P75-P25")
Disp %>% ungroup() %>% filter(Disp3>quantile(Disp3,.1,na.rm=TRUE) & Disp3<quantile(Disp3,.9,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Cov,Disp3))+geom_point() + labs(x="Between",y="sd/Mean")
#Disp %>% ungroup() %>% filter(Vaw_mean<quantile(Vaw_mean,.99,na.rm=TRUE) & Vaw_Gini>0  & Vaw_Gini<10) %>%  ggplot(aes(Vaw_Cov,Vaw_Gini))+geom_point() + labs(x="Between",y="Gini")

# We can do the same with binscatter.....

# Every measure in 1994 against Vaw growth

Disp %>% group_by(e17) %>% mutate(Vaw_g_94_14=if_else(lag(Vaw,4)!=0,Vaw/lag(Vaw,4)-1,-9999)) %>% na_if(-9999) %>% mutate_at(vars(starts_with("Disp"),Vaw_Gini),funs(l94=lag(.,4)))->Disp

my.plots<-list()
Disp %>% ungroup() %>%  filter(Disp1_l94>quantile(Disp1_l94,.005,na.rm=TRUE) & Disp1_l94<quantile(Disp1_l94,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw_g_94_14~Disp1_l94", key_var="Disp1_l94", bins=100 ,partial=FALSE)+labs(x="P90-P10 1994",y="Va/w growth")
my.plots[[1]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp2_l94>quantile(Disp2_l94,.005,na.rm=TRUE) & Disp2_l94<quantile(Disp2_l94,.995,na.rm=TRUE))%>% as.data.frame(.) %>% binscatter(formula="Vaw_g_94_14~Disp2_l94", key_var="Disp2_l94", bins=100 ,partial=FALSE)+labs(x="P75-P25 1994",y="Va/w growth")
my.plots[[2]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp3_l94>quantile(Disp3_l94,.005,na.rm=TRUE) & Disp3_l94<quantile(Disp3_l94,.995,na.rm=TRUE)) %>% as.data.frame(.) %>%  binscatter(formula="Vaw_g_94_14~Disp3_l94", key_var="Disp3_l94", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(NA, 1), breaks = seq(-1, 1, by = .5)) +labs(x="sd/Mean 1994",y="Va/w growth")
my.plots[[3]]<-recordPlot()
#Disp %>% ungroup() %>%  filter(Vaw_Gini_l94>quantile(Vaw_Gini_l94,.025,na.rm=TRUE) & Vaw_Gini_l94<quantile(Vaw_Gini_l94,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini_l94,Vaw_g_94_14))+geom_point()+labs(x="Gini 1994",y="Vaw growth")

# Every measure at time t with Vaw at time t.

Disp %>% ungroup() %>%  filter(Disp1>quantile(Disp1,.005,na.rm=TRUE) & Disp1<quantile(Disp1,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw~Disp1", key_var="Disp1", bins=100 ,partial=FALSE) +labs(x="P90-P10",y="Vaw")
my.plots[[4]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp2>quantile(Disp2,.005,na.rm=TRUE) & Disp2<quantile(Disp2,.995,na.rm=TRUE))%>% as.data.frame(.) %>% binscatter(formula="Vaw~Disp2", key_var="Disp2", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(NA, 75), breaks = seq(-25, 75, by = 25))+labs(x="P75-P25",y="Vaw")
my.plots[[5]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp3>quantile(Disp3,.005,na.rm=TRUE) & Disp3<quantile(Disp3,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw~Disp3", key_var="Disp3", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(NA, 40), breaks = seq(-10, 40, by = 10))+labs(x="sd/Mean",y="Vaw")
my.plots[[6]]<-recordPlot()
#Disp %>% ungroup() %>%  filter(Vaw_Gini>quantile(Vaw_Gini,.025,na.rm=TRUE) & Vaw_Gini<quantile(Vaw_Gini,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini,Vaw))+geom_point()+labs(x="Gini",y="Vaw")

# Every measure with its long change (94-14)

Disp %>% group_by(e17) %>% mutate_at(vars(starts_with("Disp"),Vaw_Gini),funs(Delta=.-lag(.,4)))->Disp

Disp %>% ungroup() %>%  filter(Disp1_Delta>quantile(Disp1_Delta,.005,na.rm=TRUE) & Disp1_Delta<quantile(Disp1_Delta,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Disp1_Delta~Disp1_l94", key_var="Disp1_l94", bins=100 ,partial=FALSE)+labs(x="P90-P10, 1994",y="Delta P90-P10")
my.plots[[7]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp2_Delta>quantile(Disp2_Delta,.005,na.rm=TRUE) & Disp2_Delta<quantile(Disp2_Delta,.995,na.rm=TRUE))%>%  as.data.frame(.) %>% binscatter(formula="Disp2_Delta~Disp2_l94", key_var="Disp2_l94", bins=100 ,partial=FALSE)+scale_y_continuous(limits=c(-30,10),breaks = seq(-30,10,by=10))+labs(x="P75-P25, 1994",y="Delta P75-P25")
my.plots[[8]]<-recordPlot()
Disp %>% ungroup() %>%  filter(Disp3_Delta>quantile(Disp3_Delta,.005,na.rm=TRUE) & Disp3_Delta<quantile(Disp3_Delta,.995,na.rm=TRUE)) %>%  as.data.frame(.) %>% binscatter(formula="Disp3_Delta~Disp3_l94", key_var="Disp3_l94", bins=100 ,partial=FALSE)+labs(x="sd/Mean, 1994",y="Delta sd/Mean")
my.plots[[9]]<-recordPlot()
#Disp %>% ungroup() %>%  filter(Vaw_Gini_Delta>quantile(Vaw_Gini_Delta,.025,na.rm=TRUE) & Vaw_Gini_Delta<quantile(Vaw_Gini_Delta,.975,na.rm=TRUE)) %>%  ggplot(aes(Vaw_Gini,Vaw_Gini_Delta))+geom_point()+labs(x="Gini",y="Delta Gini")

# Every measure with between and within component of Vaw

Disp %>% group_by(anio) %>% filter(Disp1>quantile(Disp1,.005,na.rm=TRUE) & Disp1<quantile(Disp1,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw_mean~Disp1", key_var="Disp1", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(NA, 200), breaks = seq(-50, 200, by = 50)) + labs(y="Within",x="P90-P10")
my.plots[[10]]<-recordPlot()
Disp %>% group_by(anio) %>% filter(Disp2>quantile(Disp2,.005,na.rm=TRUE) & Disp2<quantile(Disp2,.995,na.rm=TRUE)) %>%  as.data.frame(.) %>% binscatter(formula="Vaw_mean~Disp2", key_var="Disp2", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(NA, 150), breaks = seq(-50, 150, by = 50)) +labs(y="Within",x="P75-P25")
my.plots[[11]]<-recordPlot()
Disp %>% ungroup() %>% filter(Disp3>quantile(Disp3,.005,na.rm=TRUE) & Disp3<quantile(Disp3,.995,na.rm=TRUE)) %>%  as.data.frame(.) %>% binscatter(formula="Vaw_mean~Disp3", key_var="Disp3", bins=100 ,partial=FALSE) +scale_y_continuous(limits = c(NA, 550), breaks = seq(-50, 550, by = 50))+ labs(y="Within",x="sd/Mean")
my.plots[[12]]<-recordPlot()
#Disp %>% ungroup() %>% filter(Vaw_mean<quantile(Vaw_mean,.99,na.rm=TRUE) & Vaw_Gini>0  & Vaw_Gini<10) %>%  ggplot(aes(Vaw_mean,Vaw_Gini))+geom_point() + labs(x="Within",y="Gini")

# Every measure with between and Between component of Vaw

Disp %>% group_by(anio) %>% filter(Disp1>quantile(Disp1,.005,na.rm=TRUE) & Disp1<quantile(Disp1,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw_Cov~Disp1", key_var="Disp1", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(-150, 100), breaks = seq(-150, 100, by = 50))+ labs(y="Between",x="P90-P10")
my.plots[[13]]<-recordPlot()
Disp %>% group_by(anio) %>% filter(Disp2>quantile(Disp2,.005,na.rm=TRUE) & Disp2<quantile(Disp2,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw_Cov~Disp2", key_var="Disp2", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(-100, 50), breaks = seq(-100, 100, by = 50)) + labs(y="Between",x="P75-P25")
my.plots[[14]]<-recordPlot()
Disp %>% ungroup() %>% filter(Disp3>quantile(Disp3,.005,na.rm=TRUE) & Disp3<quantile(Disp3,.995,na.rm=TRUE)) %>% as.data.frame(.) %>% binscatter(formula="Vaw_Cov~Disp3", key_var="Disp3", bins=100 ,partial=FALSE)+scale_y_continuous(limits = c(-500, NA), breaks = seq(-550,50, by = 100)) + labs(y="Between",x="sd/Mean")
my.plots[[15]]<-recordPlot()
#Disp %>% ungroup() %>% filter(Vaw_mean<quantile(Vaw_mean,.99,na.rm=TRUE) & Vaw_Gini>0  & Vaw_Gini<10) %>%  ggplot(aes(Vaw_Cov,Vaw_Gini))+geom_point() + labs(x="Between",y="Gini")

# Analysis with Herfindahl

Disp %>% mutate_at(vars(starts_with("Herf")), funs(g_94_14=if_else(lag(.,4)!=0,./lag(.,4)-1,-9999),l94=lag(.,4))) %>% na_if(-9999)->Disp
#Disp %>% filter(anio==1994) %>%  ggplot(aes(Herf_sales*100,Vaw))+geom_point() + labs(y="Vaw",x="Herfindahl")
#Disp %>% ggplot(aes(l94,Vaw_g_94_14))+geom_point() + labs(y="Vaw growth",x="Herfindahl 94")
#Disp %>% ggplot(aes(g_94_14,Vaw_g_94_14))+geom_point() + labs(y="Vaw growth",x="Herfindahl growth")

prueba<-as.data.frame(Disp)
prueba %>% filter(!is.na(Vaw_g_94_14) & !is.na(g_94_14))->prueba1
binscatter(formula="Vaw~Herf_sales", key_var="Herf_sales",data=prueba, bins=100 ,partial=FALSE)+scale_y_continuous(limits=c(NA,50),breaks = seq(-10,50,by=10))+labs(x="Herfindahl",y="Va/w")
my.plots[[16]]<-recordPlot()
binscatter(formula="Vaw_g_94_14~l94", key_var="l94",data=prueba1, bins=100 ,partial=FALSE)+scale_y_continuous(limits=c(NA,2),breaks = seq(-1.5,2,by=.5))+labs(x="Herfindahl 1994",y="Va/w long run growth")
my.plots[[17]]<-recordPlot()
binscatter(formula="Vaw_g_94_14~g_94_14", key_var="g_94_14",data=prueba1, bins=100 ,partial=FALSE)+scale_y_continuous(limits=c(NA,1),breaks = seq(-1,1,by=.5))+labs(x="Herfindahl long run growth",y="Va/w long run growth")
my.plots[[18]]<-recordPlot()

pdf("C:/Users/pc Luis/Documents/World Bank/Resultados/Graphs/Dispersion_graphs.pdf", onefile = TRUE)
for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()

