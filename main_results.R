## ----warning=F,message=F------------------------------------------------------------------------------------------------------
library(quantmod)
library(lubridate)
library(xtable)
library(parallel)
library(PerformanceAnalytics)
library(plyr)

rm(list = ls())


########### get the spy data and inflation 
{
  SPY <- get(getSymbols("SPY",from = "1990-01-01"))
  P <- SPY$SPY.Adjusted
  
  R <- na.omit(P/lag(P))-1
  R <- R["1994-01-01/2020-12-31",]
  plot(cumprod(R+1), main = "Cumulative Return")
  
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  CPI <-  get(getSymbols("CPIAUCSL",from = "1990-01-01", src = "FRED"))
  INF <- na.omit((CPI/lag(CPI))  -1)
  INF <- INF["1994-01-01/2020-12-31",]
  
  ds1 <- data.frame(Date = date(INF), Inflation = as.numeric(INF) )
  ds1$Date <- ceiling_date(ds1$Date,"m") - 1
  
  R_m <- apply.monthly(R,function(x) prod(x+1) - 1  )
  ds2 <- data.frame(Date = date(R_m), SPY = as.numeric(R_m) )
  ds2$Date <- ceiling_date(ds2$Date,"m") - 1
  
  ds <- merge(ds1,ds2)
  rownames(ds) <- ds$Date
  ds$Date <- NULL
  ds <- as.xts(ds)
  
  
  passive_wealth <- (prod(ds$SPY_inf_adj+1))
  ds$SPY_inf_adj <- (1+ds$SPY)/(1+ds$Inflation) - 1
  plot(cumprod(ds$SPY_inf_adj+1), main = "Inflation-adjusted Cumulative Return")
  
  
  
  ### add both plots using ggplot
  bar.col <- "gray"
  
  ggplot_recession4 <- geom_rect(fill = bar.col,col = bar.col,
                                 aes(xmin=date("2001-03-31"), 
                                     xmax=date("2001-11-30"), 
                                     ymin=-Inf, ymax=Inf))
  ggplot_recession5 <- geom_rect(fill = bar.col,col = bar.col,
                                 aes(xmin=date("2007-12-31"), 
                                     xmax=date("2009-06-30"), 
                                     ymin=-Inf, ymax=Inf))
  
  ggplot_recession6 <- geom_rect(fill = bar.col,col = bar.col,
                                 aes(xmin=date("2020-02-01"), 
                                     xmax=date("2020-12-31"), 
                                     ymin=-Inf, ymax=Inf))
  
  
  ds.plot <- data.frame(date = date(ds), CumRet =  as.numeric(cumprod(ds$SPY+1)) )
  p <- ggplot(ds.plot, aes(date, CumRet))
  p <- p +  ggplot_recession4 + ggplot_recession5 + ggplot_recession6
  p <- p + geom_line(alpha = 0.4)
  p <- p + xlab("Date") + ylab("Cumulative Return")
  p <- p + geom_abline(intercept = 1, slope = 0, color="black",  linetype="dashed", size=0.2)
  p <- p + ylim(c(1,14))
  p1 <- p
  
  ds.plot <- data.frame(date = date(ds), CumRet =  as.numeric(cumprod(ds$SPY_inf_adj+1)) )
  p <- ggplot(ds.plot, aes(date, CumRet))
  p <- p +  ggplot_recession4 + ggplot_recession5 + ggplot_recession6
  p <- p + geom_line(alpha = 0.4)
  p <- p + xlab("Date") + ylab("Cumulative Return")
  p <- p + geom_abline(intercept = 1, slope = 0, color="black",  linetype="dashed", size=0.2)
  p <- p + ylim(c(1,14))
  p2 <- p
 
  
}


############################################################################################

####################################
##### MONTHLY ######################
####################################

## -----------------------------------------------------------------------------------------------------------------------------
btd_function <- function(k,tau,year1 = 1994,year2 = 2020) {
  
  ds <- ds[year(ds) >= year1,]
  ds <- ds[year(ds) <= year2,]
  
  passive_wealth <- (prod(ds$SPY_inf_adj+1))
  
  
  k_i <- 1 # first investment
  i <- 1 # first month
  
  ret_i <- ds$SPY_inf_adj[i] # \tilde{r}_t
  inf_i <- ds$Inflation[i] # i_t
  
  # adjust stock and cash
  stock <- (1/k)*(1+ret_i)
  cash <- (1-k_i/k)/(1+inf_i)
  
  # stack each value in a sequence
  stock_seq <- as.numeric(stock)
  cash_seq <- as.numeric(cash)
  
  # loop it for the full period
  for (i in 2:nrow(ds)) {
    ret_current <- as.numeric(ds$SPY_inf_adj[i-1])
    
    # condition if current return is less than tau
    # and if the investor still holding cash
    if(ret_current < tau & k_i < k) {
      stock <- stock + 1/k
      k_i <- k_i + 1   
      cash <- (1-k_i/k)
    }
    
    # next period return
    ret_next <- as.numeric(ds$SPY_inf_adj[i])
    inf_next <- as.numeric(ds$Inflation[i])
    
    # adjust stock and cash
    stock <- stock*(1+ret_next)
    cash <- cash/(1+inf_next)   
    
    # stack in a sequence
    stock_seq <- c(stock_seq,as.numeric(stock))
    cash_seq <- c(cash_seq,as.numeric(cash))
    
  } 
  
  # finally wealth is stock and cash from last period
  wealth <- stock + cash
  wealth_over_time <- c(1,stock_seq + cash_seq)
  ret_over_time <- wealth_over_time[-1]/wealth_over_time[-length(wealth_over_time)] - 1
  
  SR <- sqrt(12)*mean(ret_over_time)/sd(ret_over_time)
  SR2 <- sqrt(12)*mean(ret_over_time)/sd(ret_over_time[ret_over_time<0])
  
  list(wealth,stock_seq,cash_seq,passive_wealth,SR,SR2)
}



## -----------------------------------------------------------------------------------------------------------------------------
mean(ds$SPY_inf_adj < -0.01)
mean(ds$SPY_inf_adj < -0.05)


{
  k <- 20
  tau <- -0.05
  btd_try <- btd_function(k,tau)
  stock_try <- btd_try[[2]]
  cash_try <- btd_try[[3]]
  names(cash_try) <- date(ds)
  cash_try <- as.xts(cash_try)
  
  ds.plot <- data.frame(date = date(cash_try), CumRet =  as.numeric(cash_try) )
  
  p <- ggplot(ds.plot, aes(date, CumRet))
  p <- p +  ggplot_recession4 + ggplot_recession5 + ggplot_recession6
  p <- p + geom_line(alpha = 1)
  p <- p + xlab("Date") + ylab("Cash Holdings")
  p <- p + ylim(c(0,1))
  p <- p + geom_abline(intercept = 0, slope = 0, color="black",  linetype="dashed", size=0.2)
  p
  
 
  
  
  k <- 20
  tau <- -0.06
  btd_try <- btd_function(k,tau)
  stock_try <- btd_try[[2]]
  cash_try <- btd_try[[3]]
  names(cash_try) <- date(ds)
  cash_try <- as.xts(cash_try)
  
  ds.plot <- data.frame(date = date(cash_try), CumRet =  as.numeric(cash_try) )
  
  p <- ggplot(ds.plot, aes(date, CumRet))
  p <- p +  ggplot_recession4 + ggplot_recession5 + ggplot_recession6
  p <- p + geom_line(alpha = 1)
  p <- p + xlab("Date") + ylab("Cash Holdings")
  p <- p + ylim(c(0,1))
  p <- p + geom_abline(intercept = 0, slope = 0, color="black",  linetype="dashed", size=0.2)
  p
  
  
 
  
}

## -----------------------------------------------------------------------------------------------------------------------------
as.numeric(btd_try[[1]])




######################################
##### run for different values #######

{
  year1 <- 1994
  year2 <- 2020
  k_seq <-  seq(1,10,by = 1)
  tau_seq <- seq(-0.05,0,by = 0.01)
  
  k_tau_grid <- expand.grid(k_seq,tau_seq)
  Z <- Z2 <- Z3 <- c()
  
  for (j in 1:nrow(k_tau_grid)) {
    k_j <- k_tau_grid[j,1]
    tau_j <- k_tau_grid[j,2]
    results_j <- btd_function(k_j,tau_j,year1,year2)
    wealth_k_tau <-results_j[[1]]
    Z <- c(Z,wealth_k_tau)
    Z2 <- c(Z2,results_j[[5]])
    Z3 <- c(Z3,results_j[[6]])
    
  }
  
  
  ds_plot <- data.frame( k = k_tau_grid[,1], tau = k_tau_grid[,2], Wealth = Z)
  v <- ggplot(ds_plot) + 
    aes(x = k, y = tau, z = Wealth, fill = Wealth) + 
    geom_tile() +
    geom_contour(color = "black", alpha = 0) + 
    xlab("k") + ylab(expression(tau)) + 
    scale_fill_distiller(palette="Spectral", na.value="white") + 
    theme_bw()
  
 
  
  
  Z <- matrix(Z,length(k_seq))
  rownames(Z) <- paste("k",k_seq,sep = "=")
  colnames(Z) <-  paste("tau",abs(tau_seq*100),sep = "_")
  
  passive_wealth <-  Z[1,1]
  Z_1994 <- data.frame(round(Z,2))
  Z_1994 <- data.frame(k = k_seq,Z_1994)
  rownames(Z_1994) <- NULL
  
  ## ----warning=F,message=F------------------------------------------------------------------------------------------------------
  library(plotly)
  p <- plot_ly(z = Z,x = k_seq, y = tau_seq, type = "surface")
  p
  
  
  # ## -----------------------------------------------------------------------------------------------------------------------------
  # SR <- matrix(Z2,length(k_seq))
  # rownames(SR) <- paste("k",k_seq,sep = "=")
  # colnames(SR) <-  paste("tau",abs(tau_seq*100),sep = "_")
  # SR <- SR - SR[1,1]
  # SR1 <- data.frame(round(SR,2))
  # SR_1994 <- data.frame(k_seq,SR1)
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  SR2 <- matrix(Z3,length(k_seq))
  rownames(SR2) <- paste("k",k_seq,sep = "=")
  colnames(SR2) <-  paste("tau",abs(tau_seq*100),sep = "_")
  SR2 <- SR2 - SR2[1,1]
  SR2 <- data.frame(round(SR2,2))
  SR_1994 <- data.frame(k_seq,SR2)
  
  
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  year1 <- 2008
  year2 <- 2020
  k_seq <-  seq(1,10,by = 1)
  tau_seq <- seq(-0.05,0,by = 0.01)
  
  k_tau_grid <- expand.grid(k_seq,tau_seq)
  Z <- Z2 <- Z3 <- c()
  
  for (j in 1:nrow(k_tau_grid)) {
    k_j <- k_tau_grid[j,1]
    tau_j <- k_tau_grid[j,2]
    results_j <- btd_function(k_j,tau_j,year1,year2)
    wealth_k_tau <-results_j[[1]]
    Z <- c(Z,wealth_k_tau)
    Z2 <- c(Z2,results_j[[5]])
    Z3 <- c(Z3,results_j[[6]])
    
  }
  
  ds_plot <- data.frame( k = k_tau_grid[,1], tau = k_tau_grid[,2], Wealth = Z)
  v <- ggplot(ds_plot) + 
    aes(x = k, y = tau, z = Wealth, fill = Wealth) + 
    geom_tile() +
    geom_contour(color = "black", alpha = 0) + 
    xlab("k") + ylab(expression(tau)) + 
    scale_fill_distiller(palette="Spectral", na.value="white") + 
    theme_bw()
  
  
  
  Z <- matrix(Z,length(k_seq))
  rownames(Z) <- paste("k",k_seq,sep = "=")
  colnames(Z) <-  paste("tau",abs(tau_seq*100),sep = "_")
  
  passive_wealth <-  Z[1,1]
  Z_2008 <- data.frame(round(Z,2))
  Z_2008 <- data.frame(k = k_seq,Z_2008)
  rownames(Z_2008) <- NULL
  
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  SR2 <- matrix(Z3,length(k_seq))
  rownames(SR2) <- paste("k",k_seq,sep = "=")
  colnames(SR2) <-  paste("tau",abs(tau_seq*100),sep = "_")
  SR2 <- SR2 - SR2[1,1]
  SR2 <- data.frame(round(SR2,2))
  SR_2008 <- data.frame(k_seq,SR2)
  
  
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  year1 <- 2000
  year2 <- 2020
  k_seq <-  seq(1,10,by = 1)
  tau_seq <- seq(-0.05,0,by = 0.01)
  
  k_tau_grid <- expand.grid(k_seq,tau_seq)
  Z <- Z2 <- Z3 <- c()
  
  for (j in 1:nrow(k_tau_grid)) {
    k_j <- k_tau_grid[j,1]
    tau_j <- k_tau_grid[j,2]
    results_j <- btd_function(k_j,tau_j,year1,year2)
    wealth_k_tau <-results_j[[1]]
    Z <- c(Z,wealth_k_tau)
    Z2 <- c(Z2,results_j[[5]])
    Z3 <- c(Z3,results_j[[6]])
    
  }
  
  ds_plot <- data.frame( k = k_tau_grid[,1], tau = k_tau_grid[,2], Wealth = Z)
  v <- ggplot(ds_plot) + 
    aes(x = k, y = tau, z = Wealth, fill = Wealth) + 
    geom_tile() +
    geom_contour(color = "black", alpha = 0) + 
    xlab("k") + ylab(expression(tau)) + 
    scale_fill_distiller(palette="Spectral", na.value="white") + 
    theme_bw()
  
 
  
  
  Z <- matrix(Z,length(k_seq))
  rownames(Z) <- paste("k",k_seq,sep = "=")
  colnames(Z) <-  paste("tau",abs(tau_seq*100),sep = "_")
  
  p <- plot_ly(z = ds_plot$Z,x = ds_plot$k, y = ds_plot$Z, type = "surface")
  p <- layout(p, scene = list(xaxis = list(title = "A", range = c(min(k_seq),max(k_seq))), 
                              yaxis = list(title = "B", range = c(min(tau_seq),max(tau_seq))), 
                              zaxis = list(title = "C", range = range(Z) )))
  
  
  
  passive_wealth <-  Z[1,1]
  Z_2000 <- data.frame(round(Z,2))
  names(Z_2000) <- paste(names(Z_2000),year1,sep = "_")
  Z_2000 <- data.frame(k = k_seq,Z_2000)
  rownames(Z_2000) <- NULL
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  SR2 <- matrix(Z3,length(k_seq))
  rownames(SR2) <- paste("k",k_seq,sep = "=")
  colnames(SR2) <-  paste("tau",abs(tau_seq*100),sep = "_")
  SR2 <- SR2 - SR2[1,1]
  SR2 <- data.frame(round(SR2,2))
  SR_2000 <- data.frame(k_seq,SR2)
  
  
  
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  year1 <- 2010
  year2 <- 2020
  k_seq <-  seq(1,10,by = 1)
  tau_seq <- seq(-0.05,0,by = 0.01)
  
  k_tau_grid <- expand.grid(k_seq,tau_seq)
  Z <- Z2 <- Z3 <- c()
  
  for (j in 1:nrow(k_tau_grid)) {
    k_j <- k_tau_grid[j,1]
    tau_j <- k_tau_grid[j,2]
    results_j <- btd_function(k_j,tau_j,year1,year2)
    wealth_k_tau <-results_j[[1]]
    Z <- c(Z,wealth_k_tau)
    Z2 <- c(Z2,results_j[[5]])
    Z3 <- c(Z3,results_j[[6]])
    
  }
  
  ds_plot <- data.frame( k = k_tau_grid[,1], tau = k_tau_grid[,2], Wealth = Z)
  v <- ggplot(ds_plot) + 
    aes(x = k, y = tau, z = Wealth, fill = Wealth) + 
    geom_tile() +
    geom_contour(color = "black", alpha = 0) + 
    xlab("k") + ylab(expression(tau)) + 
    scale_fill_distiller(palette="Spectral", na.value="white") + 
    theme_bw()
  

  
  Z <- matrix(Z,length(k_seq))
  rownames(Z) <- paste("k",k_seq,sep = "=")
  colnames(Z) <-  paste("tau",abs(tau_seq*100),sep = "_")
  
  passive_wealth <-  Z[1,1]
  Z_2010 <- data.frame(round(Z,2))
  Z_2010 <- data.frame(k = k_seq,Z_2010)
  rownames(Z_2010) <- NULL
  
  ## -----------------------------------------------------------------------------------------------------------------------------
  SR2 <- matrix(Z3,length(k_seq))
  rownames(SR2) <- paste("k",k_seq,sep = "=")
  colnames(SR2) <-  paste("tau",abs(tau_seq*100),sep = "_")
  SR2 <- SR2 - SR2[1,1]
  SR2 <- data.frame(round(SR2,2))
  SR_2010 <- data.frame(k_seq,SR2)
  
  Z_bull <- merge(Z_1994,Z_2010, by = "k")
  M <- print(xtable(Z_bull, digits = c(0,0,rep(2,ncol(Z_mat)-1))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:18]
  
  
  
  
  Z_bear <- merge(Z_2000,Z_2008, by = "k")
  M <- print(xtable(Z_bear, digits = c(0,0,rep(2,ncol(Z_mat)-1))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:18]
 
  
  
  ### report the same for SR
  SR2_bull <- merge(SR_1994,SR_2010, by = "k_seq")
  M <- print(xtable(SR2_bull, digits = c(0,0,rep(2,ncol(Z_mat)-1))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:18]
  
  
  SR2_bear <- merge(SR_2000,SR_2008, by = "k_seq")
  M <- print(xtable(SR2_bear, digits = c(0,0,rep(2,ncol(Z_mat)-1))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:18]
  
 
}





#########################################################################################

####################################################
########### daily and monthly cashflows ############


## -----------------------------------------------------------------------------------------------------------------------------
btd_function_d <- function(tau,year1 = 1994,year2 = 2020) {
  
  ds <- R
  ds <- ds[year(ds) >= year1,]
  ds <- ds[year(ds) <= year2,]
  
  tot_payments <- 12*length(unique(year(ds)))
  
  ds$Inflation <- 0
  ds$SPY_inf_adj <- (1+ds$SPY.Adjusted)/(1+ds$Inflation) - 1
  
  cash <- 1 # first income
  i <- 1 # first month
  
  ret_i <- ds$SPY_inf_adj[i] # \tilde{r}_t
  
  # adjust stock and cash
  stock <- 0*(1+ret_i)
  
  # stack each value in a sequence
  stock_seq <- as.numeric(stock)
  cash_seq <- as.numeric(cash)
  
  prev_month <- (ceiling_date(date(ds),"m")-1)[i]
  
  # loop it for the full period
  for (i in 2:nrow(ds)) {
    ret_current <- as.numeric(ds$SPY_inf_adj[i-1])
    
    
    
    current_month <- (ceiling_date(date(ds),"m")-1)[i]
    month_constraint <- current_month == prev_month
    if(!month_constraint)  
      cash <- cash + 1 # update cash when new month arrives
    
    if(ret_current < tau & cash > 0) {
      # cat("Invest more ",as.character(date(ds)[i]) ,"\n")
      stock <- stock + 1
      cash <- cash - 1 # adjust cash level
    }
    
    prev_month <- current_month
    
    # next period return
    ret_next <- as.numeric(ds$SPY_inf_adj[i])
    inf_next <- as.numeric(ds$Inflation[i])
    
    # adjust stock and cash
    stock <- stock*(1+ret_next)
    
    # stack in a sequence
    stock_seq <- c(stock_seq,as.numeric(stock))
    cash_seq <- c(cash_seq,as.numeric(cash))
    
  } 
  
  # finally wealth is stock and cash from last period
  wealth <- stock + cash
  wealth_over_time <- stock_seq + cash_seq
  names(wealth_over_time) <- date(ds)
  wealth_over_time <- as.xts(wealth_over_time)
  wealth_over_time <- apply.monthly(wealth_over_time,last)
  # wealth_over_time <- wealth_over_time/(1:tot_payments)
  
  ret_over_time <- na.omit(wealth_over_time/lag(wealth_over_time))-1
  
  # adjust returns for inflation
  x <- ret_over_time
  ds2 <- data.frame(Date = date(x), Port = as.numeric(x) )
  ds2$Date <- ceiling_date(ds2$Date,"m") - 1
  ds <- merge(ds1,ds2)
  rownames(ds) <- ds$Date
  ds$Date <- NULL
  ds <- as.xts(ds)
  ret_over_time <- (1+ds$Port)/(1+ds$Inflation) - 1
  
  list(wealth,stock_seq,cash_seq,ret_over_time)
}




## ----warning=F,message=F------------------------------------------------------------------------------------------------------
tau_seq <- seq(-0.02,0,by = 0.005)
tau_seq <- c(tau_seq,100) # 100 for invest at the beginning of the month
year_seq <- c(1994,2010,2000,2008)
year_tau_seq <- expand.grid(tau_seq,year_seq)

btd_d_list <- mclapply(1:nrow(year_tau_seq),
                       function(i)  btd_function_d( tau = year_tau_seq[i,"Var1"], year1 =  year_tau_seq[i,"Var2"]),
                       mc.cores = detectCores())


W_d <- sapply(btd_d_list,function(x) x[[1]] )
W_d <- round(data.frame(matrix(W_d,length(year_seq),byrow = T)),2)
W_d <- data.frame(c(1,1,0,0),year_seq,W_d)

M <- print(xtable(W_d,digits = c(0,0,0,rep(2,length(tau_seq)))),include.rownames = F)
M <- strsplit(M,"\n")[[1]]
M <- M[9:12]


## -----------------------------------------------------------------------------------------------------------------------------
{
  my_sum_f <- function(x) {
    m <- mean(x)*12
    s <- sd(x)*sqrt(12)
    s2 <- sd(x[x<0])*sqrt(12)
    sr1 <- m/s
    sr2 <- m/s2
    
    VaR <- mean(x) - quantile(x,0.01)
    result <- data.frame(Mean = m*100, Volatility = s*100, Sharpe = sr1, Sortino = sr2,VaR = VaR*100)
  }
  
  
  ret_list <- lapply(btd_d_list,function(x) x[[4]])
  
  
  perf_table <- lapply(ret_list, my_sum_f)
  perf_table <- sapply(perf_table,function(x) x[1,"Sortino"])
  
  W_d <- round(data.frame(matrix(perf_table,length(year_seq),byrow = T)),2)
  W_d <- data.frame(c(1,1,0,0),year_seq,W_d)
  M <- print(xtable(W_d,digits = c(0,0,0,rep(2,length(tau_seq)))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:12]
  
  
  
  
  perf_table <- lapply(ret_list, my_sum_f)
  perf_table <- sapply(perf_table,function(x) x[1,"VaR"])
  
  W_d <- round(data.frame(matrix(perf_table,length(year_seq),byrow = T)),2)
  W_d <- data.frame(c(1,1,0,0),year_seq,W_d)
  M <- print(xtable(W_d,digits = c(0,0,0,rep(2,length(tau_seq)))),include.rownames = F)
  M <- strsplit(M,"\n")[[1]]
  M <- M[9:12]
  
  
}



############################################################
################## MAXIMUM DRAWDOWN CASE ###################
MDD_3M <- na.omit(rollapply(R,21*3,maxDrawdown))
MDD_6M <- na.omit(rollapply(R,21*6,maxDrawdown))
MDD_12M <- na.omit(rollapply(R,21*12,maxDrawdown))

length(unique(as.numeric(MDD_3M[MDD_3M > 0.2,])))
length(unique(as.numeric(MDD_6M[MDD_6M > 0.2,])))
length(unique(as.numeric(MDD_12M[MDD_12M > 0.2,])))




MDD_list <- list(MDD_3M,MDD_6M,MDD_12M)

btd_function_mdd <- function(tau,MDD_select,year1 = 1995,year2 = 2020) {
  
  MDD <- MDD_list[[MDD_select]]
  
  ds <- R
  ds <- ds[year(ds) >= year1,]
  ds <- ds[year(ds) <= year2,]
  
  MDD <- MDD[year(MDD) >= year1,]
  MDD <- MDD[year(MDD) <= year2,]
  
  tot_payments <- 12*length(unique(year(ds)))
  
  ds$Inflation <- 0
  ds$SPY_inf_adj <- (1+ds$SPY.Adjusted)/(1+ds$Inflation) - 1
  ds$MDD <- MDD
  cash <- 12 # first income
  i <- 1 # first month
  
  ret_i <- ds$SPY_inf_adj[i] # \tilde{r}_t
  
  # adjust stock and cash
  stock <- 0*(1+ret_i)
  
  # stack each value in a sequence
  stock_seq <- as.numeric(stock)
  cash_seq <- as.numeric(cash)
  
  prev_month <- (ceiling_date(date(ds),"m")-1)[i]
  
  # loop it for the full period
  for (i in 2:nrow(ds)) {
    ret_current <- as.numeric(ds$SPY_inf_adj[i-1])
    mdd_current <- as.numeric(ds$MDD[i-1])
    
    current_month <- (ceiling_date(date(ds),"m")-1)[i]
    
    month_constraint <- current_month == prev_month
    if(!month_constraint)  
      cash <- cash + 1 # update cash when new month arrives
    
    if(mdd_current > -tau & cash > 0) {
      # cat("Invest more ",as.character(date(ds)[i]) ,"\n")
      stock <- stock + cash # unload all cash 
      cash <- 0 # adjust cash level
    }
    
    prev_month <- current_month
    
    # next period return
    ret_next <- as.numeric(ds$SPY_inf_adj[i])
    inf_next <- as.numeric(ds$Inflation[i])
    
    # adjust stock and cash
    stock <- stock*(1+ret_next)
    
    # stack in a sequence
    stock_seq <- c(stock_seq,as.numeric(stock))
    cash_seq <- c(cash_seq,as.numeric(cash))
    
    
  } 
  
  # finally wealth is stock and cash from last period
  wealth <- stock + cash
  wealth_over_time <- stock_seq + cash_seq
  names(cash_seq) <- date(ds)
  cash_seq <- as.xts(cash_seq)
  # plot(cash_seq)
  
  names(wealth_over_time) <- date(ds)
  wealth_over_time <- as.xts(wealth_over_time)
  wealth_over_time <- apply.monthly(wealth_over_time,last)
  # wealth_over_time <- wealth_over_time/(1:tot_payments)
  
  ret_over_time <- na.omit(wealth_over_time/lag(wealth_over_time))-1
  
  # adjust returns for inflation
  x <- ret_over_time
  ds2 <- data.frame(Date = date(x), Port = as.numeric(x) )
  ds2$Date <- ceiling_date(ds2$Date,"m") - 1
  ds <- merge(ds1,ds2)
  rownames(ds) <- ds$Date
  ds$Date <- NULL
  ds <- as.xts(ds)
  ret_over_time <- (1+ds$Port)/(1+ds$Inflation) - 1
  
  list(wealth,stock_seq,cash_seq,ret_over_time)
}


### summarize results
tau_seq <- c(-0.2,-0.1,-0.05,100)
MDD_seq <- 1:3
year_seq <- c(1995,2010,2000,2008)
year_tau_seq <- expand.grid(tau_seq,MDD_seq,year_seq)

run_mdd_f <- function(i) {
  tau_i <- year_tau_seq[i,"Var1"]
  MDD_i <-  year_tau_seq[i,"Var2"]
  year1_i <-  year_tau_seq[i,"Var3"]
  run_i <- btd_function_mdd(tau = tau_i , MDD_select = MDD_i,year1 = year1_i)
  return(run_i)
}

btd_mdd_list <- mclapply(1:nrow(year_tau_seq),run_mdd_f, mc.cores = detectCores())


W_d <- sapply(btd_mdd_list,function(x) x[[1]] )
mdd_results <- data.frame(year_tau_seq,W = W_d)

ret_list <- lapply(btd_mdd_list,function(x) x[[4]])
perf_table <- lapply(ret_list, my_sum_f)
perf_table <- ldply(perf_table,data.frame)

mdd_results <- data.frame(mdd_results,perf_table)
names(mdd_results)[1:3] <- c("tau","MDD","Year_Start")

# WEALTH
W_d <- matrix(mdd_results$W,ncol = nrow(mdd_results)/length(year_seq),byrow = T)
W_d <- W_d[,c(1:3,5:7,9:12)]
W_d <- round(W_d,2)
W_d <- data.frame(Bull = c(1,1,0,0),year_seq,W_d)

M <- print(xtable(W_d,digits = c(0,0,0,rep(2, ncol(W_d)-2 ))),include.rownames = F)
M <- strsplit(M,"\n")[[1]]
M <- M[9:12]

# SORTINO
W_d <- matrix(mdd_results$Sortino,ncol = nrow(mdd_results)/length(year_seq),byrow = T)
W_d <- W_d[,c(1:3,5:7,9:12)]
W_d <- round(W_d,2)
W_d <- data.frame(Bull = c(1,1,0,0),year_seq,W_d)

M <- print(xtable(W_d,digits = c(0,0,0,rep(2, ncol(W_d)-2 ))),include.rownames = F)
M <- strsplit(M,"\n")[[1]]
M <- M[9:12]



# VALUE-AT-RISK
W_d <- matrix(mdd_results$VaR,ncol = nrow(mdd_results)/length(year_seq),byrow = T)
W_d <- W_d[,c(1:3,5:7,9:12)]
W_d <- round(W_d,2)
W_d <- data.frame(Bull = c(1,1,0,0),year_seq,W_d)

M <- print(xtable(W_d,digits = c(0,0,0,rep(2, ncol(W_d)-2 ))),include.rownames = F)
M <- strsplit(M,"\n")[[1]]
M <- M[9:12]




#####################################
########### U-SHAPED SORTINO ########
### repeat for a wider spectrum of tau for MDD to see how SR changes.
### summarize results
tau_seq <- seq(-0.2,0,by = 0.01)
MDD_seq <- 1
year_seq <- 2000
year_tau_seq <- expand.grid(tau_seq,MDD_seq,year_seq)

run_mdd_f <- function(i) {
  tau_i <- year_tau_seq[i,"Var1"]
  MDD_i <-  year_tau_seq[i,"Var2"]
  year1_i <-  year_tau_seq[i,"Var3"]
  run_i <- btd_function_mdd(tau = tau_i , MDD_select = MDD_i,year1 = year1_i)
  return(run_i)
}

btd_mdd_list <- mclapply(1:nrow(year_tau_seq),run_mdd_f, mc.cores = detectCores())

W_d <- sapply(btd_mdd_list,function(x) x[[1]] )
mdd_results <- data.frame(year_tau_seq,W = W_d)

ret_list <- lapply(btd_mdd_list,function(x) x[[4]])
perf_table <- lapply(ret_list, my_sum_f)
perf_table <- ldply(perf_table,data.frame)
perf_table <- data.frame(tau = year_tau_seq$Var1,perf_table)
plot(Sortino~tau,data = perf_table)



