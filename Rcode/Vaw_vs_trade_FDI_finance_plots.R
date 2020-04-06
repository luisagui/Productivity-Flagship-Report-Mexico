# Script to plot change in trade integration against long run change in value added per worker.
# I began to adjust the code to compute delta logs instead of just deltas.
require(dplyr)
require(tidyr)

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
  

Base_trade_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_trade_mun.csv")
Vaw_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_Mun.csv")

Trade_integration_mun<-Vaw_mun %>% left_join(Base_trade_mun,by=c("anio","e03","e04"))

Trade_2plot<-Trade_integration_mun %>% filter(anio==1994 | anio==2014) %>% arrange(e03,e04,anio) %>% group_by(e03,e04) %>% select(-c(h001a_suma,a131a_real_suma,ends_with("P10"),
                                                                                                                        ends_with("P25"),ends_with("P50"),ends_with("P75"),ends_with("P90"),starts_with("N"),ends_with("_nas"))) %>% 
  mutate_all(funs(D=log(.)-lag(log(.)))) %>%  select(anio,e03,e04,ends_with("_D")) %>% filter(anio==2014)

my.plots<-list()

my.plots[[1]]<-Trade_2plot %>%filter(!is.na(Trade_1_D) & !is.na(P5_D)) %>% mutate(Trade_1_D=Trade_1_D*100) %>% as.data.frame() %>% binscatter(formula="P5_D~ Trade_1_D",key_var = "Trade_1_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Exports/sales")
my.plots[[2]]<-Trade_2plot %>%filter(!is.na(Trade_1_D) & !is.na(Vaw_D)) %>% mutate(Trade_1_D=Trade_1_D*100) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ Trade_1_D",key_var = "Trade_1_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Exports/sales")
my.plots[[3]]<-Trade_2plot %>%filter(!is.na(Trade_1_D) & !is.na(avg_D)) %>% mutate(Trade_1_D=Trade_1_D*100) %>% as.data.frame() %>% binscatter(formula="avg_D~ Trade_1_D",key_var = "Trade_1_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x=" Delta Exports/sales")
my.plots[[4]]<-Trade_2plot %>%filter(!is.na(Trade_1_D) & !is.na(P95_D)) %>% mutate(Trade_1_D=Trade_1_D*100) %>% as.data.frame() %>% binscatter(formula="P95_D~ Trade_1_D",key_var = "Trade_1_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Exports/sales")
 
jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Trade_integration_1_1.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()
  
my.plots[[1]]<-Trade_2plot %>%filter(!is.na(Trade_2_D) & !is.na(P5_D)) %>% mutate(Trade_2_D=Trade_2_D*100) %>%  as.data.frame() %>% binscatter(formula="P5_D~ Trade_2_D",key_var = "Trade_2_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Imports/Intermidiate")
my.plots[[2]]<-Trade_2plot %>%filter(!is.na(Trade_2_D) & !is.na(Vaw_D)) %>% mutate(Trade_2_D=Trade_2_D*100) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ Trade_2_D",key_var = "Trade_2_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Imports/Intermidiate")
my.plots[[3]]<-Trade_2plot %>%filter(!is.na(Trade_2_D) & !is.na(avg_D)) %>% mutate(Trade_2_D=Trade_2_D*100) %>%  as.data.frame() %>% binscatter(formula="avg_D~ Trade_2_D",key_var = "Trade_2_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Imports/Intermidiate")
my.plots[[4]]<-Trade_2plot %>%filter(!is.na(Trade_2_D) & !is.na(P95_D)) %>% mutate(Trade_2_D=Trade_2_D*100) %>%  as.data.frame() %>% binscatter(formula="P95_D~ Trade_2_D",key_var = "Trade_2_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Imports/Intermidiate")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Trade_integration_2_1.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-Trade_2plot %>%filter(!is.na(Trade_3_D) & !is.na(P5_D)) %>% mutate(Trade_3_D=Trade_3_D*100) %>%  as.data.frame() %>% binscatter(formula="P5_D~ Trade_3_D",key_var = "Trade_3_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta (Ex+Im)/(sales+Int)")
my.plots[[2]]<-Trade_2plot %>%filter(!is.na(Trade_3_D) & !is.na(Vaw_D)) %>% mutate(Trade_3_D=Trade_3_D*100) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ Trade_3_D",key_var = "Trade_3_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta (Ex+Im)/(sales+Int)")
my.plots[[3]]<-Trade_2plot %>%filter(!is.na(Trade_3_D) & !is.na(avg_D)) %>% mutate(Trade_3_D=Trade_3_D*100) %>%  as.data.frame() %>% binscatter(formula="avg_D~ Trade_3_D",key_var = "Trade_3_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta (Ex+Im)/(sales+Int)")
my.plots[[4]]<-Trade_2plot %>%filter(!is.na(Trade_3_D) & !is.na(P95_D)) %>% mutate(Trade_3_D=Trade_3_D*100) %>%  as.data.frame() %>% binscatter(formula="P95_D~ Trade_3_D",key_var = "Trade_3_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta (Ex+Im)/(sales+Int)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Trade_integration_3_1.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-Trade_2plot %>%filter(!is.na(Trade_4_D) & !is.na(P5_D)) %>% mutate(Trade_4_D=Trade_4_D*100) %>%  as.data.frame() %>% binscatter(formula="P5_D~ Trade_4_D",key_var = "Trade_4_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta (Ex+Im)/sales")
my.plots[[2]]<-Trade_2plot %>%filter(!is.na(Trade_4_D) & !is.na(Vaw_D)) %>% mutate(Trade_4_D=Trade_4_D*100) %>%   as.data.frame() %>% binscatter(formula="Vaw_D~ Trade_4_D",key_var = "Trade_4_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta (Ex+Im)/sales")
my.plots[[3]]<-Trade_2plot %>%filter(!is.na(Trade_4_D) & !is.na(avg_D)) %>% mutate(Trade_4_D=Trade_4_D*100) %>%  as.data.frame() %>% binscatter(formula="avg_D~ Trade_4_D",key_var = "Trade_4_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta (Ex+Im)/sales")
my.plots[[4]]<-Trade_2plot %>%filter(!is.na(Trade_4_D) & !is.na(P95_D)) %>% mutate(Trade_3_D=Trade_4_D*100) %>%  as.data.frame() %>% binscatter(formula="P95_D~ Trade_4_D",key_var = "Trade_4_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta (Ex+Im)/sales")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Trade_integration_4_1.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()


# Now, we are gonna relate value added with FDI measures

Base_fdi_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_mun.csv")

FDI_2plot<-Vaw_mun %>% left_join(Base_fdi_mun,by=c("anio","e03","e04")) %>% select(anio,e03,e04,P5,Vaw,avg,P95,h001a_Fo_suma,sales_Fo_suma,h001a_suma.y,sales_suma,d311_suma,N.y) %>% 
  mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma.y,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_Fo=d311_suma*100/N.y) %>% arrange(e03,e04,anio) %>% 
  select(-c(h001a_Fo_suma,h001a_suma.y,sales_Fo_suma,sales_suma,d311_suma,N.y)) %>% filter(anio==1994 | anio==2014) %>% group_by(e03,e04) %>% mutate_all(funs(D=.-lag(.))) %>% 
  select(anio,e03,e04,ends_with("_D")) %>% filter(anio==2014)

my.plots[[1]]<-FDI_2plot %>%filter(!is.na(emp_share_fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo Emp share")
my.plots[[2]]<-FDI_2plot %>%filter(!is.na(emp_share_fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo Emp share")
my.plots[[3]]<-FDI_2plot %>%filter(!is.na(emp_share_fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo Emp share")
my.plots[[4]]<-FDI_2plot %>%filter(!is.na(emp_share_fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo Emp share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_emp_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-FDI_2plot %>%filter(!is.na(sales_share_fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo sales share")
my.plots[[2]]<-FDI_2plot %>%filter(!is.na(sales_share_fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo sales share")
my.plots[[3]]<-FDI_2plot %>%filter(!is.na(sales_share_fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo sales share")
my.plots[[4]]<-FDI_2plot %>%filter(!is.na(sales_share_fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo sales share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sales_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-FDI_2plot %>%filter(!is.na(N_share_Fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo firms share")
my.plots[[2]]<-FDI_2plot %>%filter(!is.na(N_share_Fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo firms share")
my.plots[[3]]<-FDI_2plot %>%filter(!is.na(N_share_Fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo firms share")
my.plots[[4]]<-FDI_2plot %>%filter(!is.na(N_share_Fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo firms share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_firms_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()


Base_fdi_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Fo_states.csv")
Vaw_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_state.csv")

FDI_2plot_state<-Vaw_state %>% left_join(Base_fdi_state,by=c("anio","e03")) %>% select(anio,e03,P5,Vaw,avg,P95,h001a_Fo_suma,sales_Fo_suma,h001a_suma.y,sales_suma,d311_suma,N.y) %>% 
  mutate(emp_share_fo=h001a_Fo_suma*100/h001a_suma.y,sales_share_fo=sales_Fo_suma*100/sales_suma,N_share_Fo=d311_suma*100/N.y) %>% arrange(e03,anio) %>% 
  select(-c(h001a_Fo_suma,h001a_suma.y,sales_Fo_suma,sales_suma,d311_suma,N.y)) %>% filter(anio==1994 | anio==2014) %>% group_by(e03) %>% mutate_all(funs(D=.-lag(.))) %>% 
  select(anio,e03,ends_with("_D")) %>% filter(anio==2014)

my.plots[[1]]<-FDI_2plot_state %>%filter(!is.na(emp_share_fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo Emp share")
my.plots[[2]]<-FDI_2plot_state %>%filter(!is.na(emp_share_fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo Emp share")
my.plots[[3]]<-FDI_2plot_state %>%filter(!is.na(emp_share_fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo Emp share")
my.plots[[4]]<-FDI_2plot_state %>%filter(!is.na(emp_share_fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ emp_share_fo_D",key_var = "emp_share_fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo Emp share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_emp_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-FDI_2plot_state %>%filter(!is.na(sales_share_fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo sales share")
my.plots[[2]]<-FDI_2plot_state %>%filter(!is.na(sales_share_fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo sales share")
my.plots[[3]]<-FDI_2plot_state %>%filter(!is.na(sales_share_fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo sales share")
my.plots[[4]]<-FDI_2plot_state %>%filter(!is.na(sales_share_fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ sales_share_fo_D",key_var = "sales_share_fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo sales share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_sales_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

my.plots[[1]]<-FDI_2plot_state %>%filter(!is.na(N_share_Fo_D) & !is.na(P5_D)) %>%  as.data.frame() %>% binscatter(formula="P5_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Fo firms share")
my.plots[[2]]<-FDI_2plot_state %>%filter(!is.na(N_share_Fo_D) & !is.na(Vaw_D)) %>%  as.data.frame() %>% binscatter(formula="Vaw_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Fo firms share")
my.plots[[3]]<-FDI_2plot_state %>%filter(!is.na(N_share_Fo_D) & !is.na(avg_D)) %>%  as.data.frame() %>% binscatter(formula="avg_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Fo firms share")
my.plots[[4]]<-FDI_2plot_state %>%filter(!is.na(N_share_Fo_D) & !is.na(P95_D)) %>%  as.data.frame() %>% binscatter(formula="P95_D~ N_share_Fo_D",key_var = "N_share_Fo_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Fo firms share")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/FDI_firms_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()
