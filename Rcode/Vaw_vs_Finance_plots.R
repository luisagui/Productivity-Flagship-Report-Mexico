# Script to plot finance variables against Value added per worker.

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

Base_Finance_Mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Finance_Mun.csv")
Vaw_mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_Mun.csv")

# I need agregated info about sales, at municipal level.I recall I have if in trade info.

sales_Mun<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_trade_mun.csv") %>% select(anio,e03,e04,Exports,sales_nac) %>%
replace_na(sales_nac=0,Exports=0) %>% mutate(sales=Exports+sales_nac) %>% select(-c(sales_nac,Exports)) 

Finance_mun<-Vaw_mun %>% left_join(Base_Finance_Mun,by=c("anio","e03","e04")) %>% left_join(sales_Mun,by=c("anio","e03","e04")) %>% 
  mutate(a2f_w=sum/h001a_suma,a2f_s=sum/sales)


# I am interested in how a change in Finance will affect different measures of Value added per worker.
# The thing is, I am not sure which is the best way to measure "Access to finance". It could be as a mean (weihted or unweighted) of the total amount
# of interest paid, or as a ratio of interests over sales or employment. 

Finance_2plot<-Finance_mun %>% filter(anio==1994 | anio==2014) %>% arrange(e03,e04,anio) %>% group_by(e03,e04) %>% select(-c(h001a_suma,a131a_real_suma,ends_with("P10"),
                                                                                                                                     ends_with("P25"),ends_with("P50"),ends_with("P75"),ends_with("P90"),starts_with("N"),ends_with("_nas"))) %>% 
  mutate_all(funs(D=.-lag(.))) %>%  select(anio,e03,e04,ends_with("_D")) %>% filter(anio==2014)

my.plots<-list()

# Finance measure unweighted mean

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Unweighted mean")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Unweighted mean")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Unweighted mean")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Unweighted mean")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure weighted mean (sales)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Weighted mean(sales)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_sales_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure weighted mean (employment)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Weighted mean(workers)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_workers_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure (/sales) weighted mean (sales)
# This measure coul be think as a percentage (How much of my income goes on credits repayment?)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(a2f_s_D) & !is.na(P5_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="P5_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Finance/sales")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(a2f_s_D) & !is.na(Vaw_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Finance/sales")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(a2f_s_D) & !is.na(avg_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="avg_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Finance/sales")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(a2f_s_D) & !is.na(P95_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="P95_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Finance/sales")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_over_sales_mean_sales_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure (/employment) weighted mean (employment)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(a2f_w_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Finance/workers")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(a2f_w_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Finance/workers")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(a2f_w_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Finance/workers")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(a2f_w_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Finance/workers")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_over_workers_mean_workers_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

##### 
# State level

Base_Finance_State<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Finance_state.csv")
Vaw_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_Vaw_state.csv")

# I need agregated info about sales, at municipal level.I recall I have if in trade info.

sales_state<-data.table::fread("C:/Users/pc Luis/Documents/World Bank/Resultados/Base_trade_state.csv") %>% select(anio,e03,Exports,sales_nac) %>%
  replace_na(sales_nac=0,Exports=0) %>% mutate(sales=Exports+sales_nac) %>% select(-c(sales_nac,Exports)) 

Finance_state<-Vaw_state %>% left_join(Base_Finance_State,by=c("anio","e03")) %>% left_join(sales_state,by=c("anio","e03")) %>% 
  mutate(a2f_w=sum/h001a_suma,a2f_s=sum/sales)


Finance_2plot_state<-Finance_state %>% filter(anio==1994 | anio==2014) %>% arrange(e03,anio) %>% group_by(e03) %>% select(-c(h001a_suma,a131a_real_suma,ends_with("P10"),
                                                                                                                             ends_with("P25"),ends_with("P50"),ends_with("P75"),ends_with("P90"),starts_with("N"),ends_with("_nas"))) %>% 
  mutate_all(funs(D=.-lag(.))) %>%  select(anio,e03,ends_with("_D")) %>% filter(anio==2014)

my.plots<-list()

# Finance measure unweighted mean

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Unweighted mean")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Unweighted mean")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Unweighted mean")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(mean_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ mean_D",key_var = "mean_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Unweighted mean")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure weighted mean (sales)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Weighted mean(sales)")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(m_ws_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ m_ws_D",key_var = "m_ws_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Weighted mean(sales)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_sales_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure weighted mean (employment)

my.plots[[1]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[2]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[3]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Weighted mean(workers)")
my.plots[[4]]<-Finance_2plot %>%filter(!is.na(m_ww_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ m_ww_D",key_var = "m_ww_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Weighted mean(workers)")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_mean_workers_mun.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure (/sales) weighted mean (sales)
# This measure coul be think as a percentage (How much of my income goes on credits repayment?)

my.plots[[1]]<-Finance_2plot_state %>%filter(!is.na(a2f_s_D) & !is.na(P5_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="P5_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Finance/sales")
my.plots[[2]]<-Finance_2plot_state %>%filter(!is.na(a2f_s_D) & !is.na(Vaw_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Finance/sales")
my.plots[[3]]<-Finance_2plot_state %>%filter(!is.na(a2f_s_D) & !is.na(avg_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="avg_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Finance/sales")
my.plots[[4]]<-Finance_2plot_state %>%filter(!is.na(a2f_s_D) & !is.na(P95_D)) %>% mutate(a2f_s_D=a2f_s_D*100) %>% as.data.frame() %>% binscatter(formula="P95_D~ a2f_s_D",key_var = "a2f_s_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Finance/sales")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_over_sales_mean_sales_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()

# Finance measure (/employment) weighted mean (employment)

my.plots[[1]]<-Finance_2plot_state %>%filter(!is.na(a2f_w_D) & !is.na(P5_D)) %>% as.data.frame() %>% binscatter(formula="P5_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("P5")+labs(y="",x="Delta Finance/workers")
my.plots[[2]]<-Finance_2plot_state %>%filter(!is.na(a2f_w_D) & !is.na(Vaw_D)) %>% as.data.frame() %>% binscatter(formula="Vaw_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("Weighted mean")+labs(y="",x="Delta Finance/workers")
my.plots[[3]]<-Finance_2plot_state %>%filter(!is.na(a2f_w_D) & !is.na(avg_D)) %>% as.data.frame() %>% binscatter(formula="avg_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("Unweighted mean")+labs(y="",x="Delta Finance/workers")
my.plots[[4]]<-Finance_2plot_state %>%filter(!is.na(a2f_w_D) & !is.na(P95_D)) %>% as.data.frame() %>% binscatter(formula="P95_D~ a2f_w_D",key_var = "a2f_w_D",bins=100,partial=FALSE)+ggtitle("P95")+labs(y="",x="Delta Finance/workers")

jpeg("C:/Users/pc Luis/Documents/World Bank/Resultados/Finance_measure_over_workers_mean_workers_state.jpg")
multiplot(plotlist = my.plots,cols = 2)
graphics.off()
