# Script to compute Herfilndahl analysis.

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

loghyper<-function(k){
  a<-log(k+sqrt(k^2+1))
  return(a)
}

Disp_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Data_for_dispperssion_mun.csv")
Disp_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Data_for_dispperssion_state.csv")
Disp_state_sector<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Data_for_dispperssion_state_sector.csv")

my.plots<-list()

# Municipality level

#1
res_mun<-Disp_mun %>% select(anio,Vaw,h001a_sum,Herf_sales) %>% mutate(h001a_sum=loghyper(h001a_sum),Vaw=loghyper(Vaw)) %>%
  lm(Vaw~h001a_sum,data=.) %>% residuals.lm()


my.plots[[1]]<-Disp_mun %>% cbind(res_mun) %>% as.data.frame() %>% binscatter(formula="res_mun~Herf_sales",key_var="Herf_sales",bins=100,partial = FALSE)+
  #ggplot(aes(Herf_sales,res_mun))+
  geom_smooth(method="loess")+labs(x="Herfindahl",y="Residuals")

#2
my.plots[[2]]<-Disp_mun %>%
  #mutate(Herf_sales=loghyper(Herf_sales*100),Vaw=loghyper(Vaw)) %>%
  ggplot(aes(Herf_sales,Vaw))+geom_smooth(method="loess")+scale_x_continuous(breaks = seq(0,1,by=.1))+labs(x="Herfindahl",y="Va/w")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Herf_Vaw_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# State level

#1
res_state<-Disp_state %>% select(anio,Vaw,h001a_sum,Herf_sales) %>% mutate(h001a_sum=loghyper(h001a_sum),Vaw=loghyper(Vaw)) %>%
  lm(Vaw~h001a_sum,data=.) %>% residuals.lm()

my.plots[[1]]<-Disp_state %>% cbind(res_state) %>% as.data.frame() %>% binscatter(formula="res_state~Herf_sales",key_var="Herf_sales",bins=100,partial = FALSE)+
  #ggplot(aes(Herf_sales,res_mun))+
  geom_smooth(method="loess")+labs(x="Herfindahl",y="Residuals")

#2
my.plots[[2]]<-Disp_state %>%
  #mutate(Herf_sales=loghyper(Herf_sales*100),Vaw=loghyper(Vaw)) %>%
  ggplot(aes(Herf_sales,Vaw))+geom_smooth(method="loess")+scale_x_continuous(breaks = seq(0,1,by=.1))+labs(x="Herfindahl",y="Va/w")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Herf_Vaw_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# State-Sector level

#1
res_state_sector<-Disp_state_sector %>% select(anio,Vaw,h001a_sum,Herf_sales) %>% mutate(h001a_sum=loghyper(h001a_sum),Vaw=loghyper(Vaw)) %>%
  lm(Vaw~h001a_sum,data=.) %>% residuals.lm()

my.plots[[1]]<-Disp_state_sector %>% cbind(res_state_sector) %>% as.data.frame() %>% binscatter(formula="res_state_sector~Herf_sales",key_var="Herf_sales",bins=100,partial = FALSE)+
  #ggplot(aes(Herf_sales,res_mun))+
  geom_smooth()+labs(x="Herfindahl",y="Residuals")

#2
my.plots[[2]]<-Disp_state_sector %>%
  #mutate(Herf_sales=loghyper(Herf_sales*100),Vaw=loghyper(Vaw)) %>%
  ggplot(aes(Herf_sales,Vaw))+geom_smooth()+scale_x_continuous(breaks = seq(0,1,by=.1))+labs(x="Herfindahl",y="Va/w")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Herf_Vaw_state_sector.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

## State-Sector-Manufacturing level

#1
res_state_sector_manuf<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig=="31" | SCIAN2dig=="32" | SCIAN2dig=="33") %>%
  select(anio,Vaw,h001a_sum,Herf_sales) %>% mutate(h001a_sum=loghyper(h001a_sum),Vaw=loghyper(Vaw)) %>%
  lm(Vaw~h001a_sum,data=.) %>% residuals.lm()

my.plots[[1]]<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig=="31" | SCIAN2dig=="32" | SCIAN2dig=="33") %>%
  cbind(res_state_sector_manuf) %>% as.data.frame() %>% binscatter(formula="res_state_sector_manuf~Herf_sales",key_var="Herf_sales",bins=100,partial = FALSE)+
  #ggplot(aes(Herf_sales,res_mun))+
  geom_smooth()+labs(x="Herfindahl",y="Residuals")

#2
my.plots[[2]]<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig=="31" | SCIAN2dig=="32" | SCIAN2dig=="33") %>%
  #mutate(Herf_sales=loghyper(Herf_sales*100),Vaw=loghyper(Vaw)) %>%
  #mutate(Vaw=loghyper(Vaw)) %>%
  ggplot(aes(Herf_sales,Vaw))+geom_smooth()+scale_x_continuous(breaks = seq(0,1,by=.1))+labs(x="Herfindahl",y="Va/w")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Herf_Vaw_state_manuf.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

## State-Sector-Services-level

#1
res_state_sector_serv<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig!="31" & SCIAN2dig!="32" & SCIAN2dig!="33") %>%
  select(anio,Vaw,h001a_sum,Herf_sales) %>% mutate(h001a_sum=loghyper(h001a_sum),Vaw=loghyper(Vaw)) %>%
  lm(Vaw~h001a_sum,data=.) %>% residuals.lm()

my.plots[[1]]<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig!="31" & SCIAN2dig!="32" & SCIAN2dig!="33") %>%
  cbind(res_state_sector_serv) %>% as.data.frame() %>% binscatter(formula="res_state_sector_serv~Herf_sales",key_var="Herf_sales",bins=100,partial = FALSE)+
  #ggplot(aes(Herf_sales,res_mun))+
  geom_smooth()+labs(x="Herfindahl",y="Residuals")

#2
my.plots[[2]]<-Disp_state_sector %>% mutate(SCIAN2dig=substr(as.character(e17),1,2)) %>% filter(SCIAN2dig!="31" & SCIAN2dig!="32" & SCIAN2dig!="33") %>%
  #mutate(Herf_sales=loghyper(Herf_sales*100),Vaw=loghyper(Vaw)) %>%
  #mutate(Vaw=loghyper(Vaw)) %>%
  ggplot(aes(Herf_sales,Vaw))+geom_smooth()+scale_x_continuous(breaks = seq(0,1,by=.1))+labs(x="Herfindahl",y="Va/w")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Herf_Vaw_state_serv.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()
