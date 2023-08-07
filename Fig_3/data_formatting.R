#############################
# Data Formatting Functions #
#                           #
# nicole.nova@stanford.edu  #
#############################
# Load data and remove outlier
#加载数据并移除异常值
#dataset <- "02.dengue-sst_monthly_2014-2019_sel"
data.clean <- function(dataset,i){
  
  # We load the data and create our main dataframe df,
  file_name = paste("data/", dataset, ".csv", sep="")
  df   <- read.csv(file_name, header = T)
  countrys <- unique(df$country)
  df$date <- as.Date(df$date)
  
  df <- df %>%
    filter(country == countrys[i]) %>% # Change here to dfoduce results for other countries
  return(df)
}


# Add lagged and averaged lagged variables 
#添加滞后和平均滞后变量
data.process <- function(PR){
  
  # Next, we add new lagged variables
  # 接下来，我们添加新的滞后变量
  for (i in 0:5){
    PR[[paste("ir.lag_", i, sep="")]] <- data.table::shift(PR$ir, i)
  }
  
  for (i in 0:5){
    PR[[paste("t2m.lag_", i, sep="")]] <- data.table::shift(PR$t2m, i)
  }
  
  for (i in 0:5){
    PR[[paste("tp.lag_", i, sep="")]] <- data.table::shift(PR$tp, i)
  }
  
  for (i in 0:5){
    PR[[paste("rh.lag_", i, sep="")]] <- data.table::shift(PR$rh, i)
  }
  
  for (i in 0:5){
    PR[[paste("svpd.lag_", i, sep="")]] <- data.table::shift(PR$svpd, i)
  }  

  return(PR)
}


# Create a dataframe with seasonal trends
# 创建一个带有季节性趋势的数据帧
data.season <- function(PR,a,b,c,d) {
  # The seasonal variables of ir, t2m, tp, and rh are averages over all years
  # Create a vector with 12 weeks of a year
  #季节变量ir, t2m, tp和rh是所有年份的平均值
  #创建一个一年12周的向量
  year.month <- c(1:12)
  
  # Create new seasonal (empty) vectors 
  ir.season <- rep(0,12)
  t2m.season <- rep(0,12) 
  tp.season <- rep(0,12)
  rh.season <- rep(0,12)
  svpd.season <- rep(0,12)  
  # Calculating year-week averages for all year for ir, t2m, tp and rh
  for(i in 1:12){
    data.sub <- subset(PR, month == i)
    ir.season[i] <- mean(data.sub$ir)
    t2m.season[i] <- mean(data.sub$t2m)
    tp.season[i] <- mean(data.sub$tp)
    rh.season[i] <- mean(data.sub$rh)
    svpd.season[i] <- mean(data.sub$svpd)
  }
  

  # Create new seasonal dataframe
  list.season <- list(year.month, ir.season, t2m.season, tp.season, 
                      rh.season, svpd.season)
  PR.season <- setNames(do.call(data.frame, list.season), 
                        c("month","ir.season", "t2m.season",
                          "tp.season", "rh.season", "svpd.season"))
  
  # Add new variables: lagged t2merature, tpipitation and rh
  for (i in 0:5){
    PR.season[[paste("ir.season.lag_", i, sep="")]] <- data.table::shift(PR.season$ir.season, i)
  }
  
  for (i in 0:5){
    PR.season[[paste("t2m.season.lag_", i, sep="")]] <- data.table::shift(PR.season$t2m.season, i)
  }
  
  for (i in 0:5){
    PR.season[[paste("tp.season.lag_", i, sep="")]] <- data.table::shift(PR.season$tp.season, i)
  }
  
  for (i in 0:5){
    PR.season[[paste("rh.season.lag_", i, sep="")]] <- data.table::shift(PR.season$rh.season, i)
  }
 
  for (i in 0:5){
    PR.season[[paste("svpd.season.lag_", i, sep="")]] <- data.table::shift(PR.season$svpd.season, i)
  }
  
  #手动改lag_
  a_1 <- 12-a+1
  a_2 <- a+1
  b_1 <- 12-b+1
  b_2 <- b+1
  c_1 <- 12-c+1
  c_2 <- c+1
  d_1 <- 12-d+1
  d_2 <- d+1
  # Get the last 3 data points (rows 44 - 12) from t2m.season, tp.season and append in the begginning
  t2m.rest <- PR.season$t2m.season[a_1:12]
  t2m.other <- PR.season$t2m.season.lag_3[a_2:12]
  t2m.season.all <- c(t2m.rest, t2m.other) 
  
  tp.rest <- PR.season$tp.season[b_1:12]
  tp.other <- PR.season$tp.season.lag_2[b_2:12]
  tp.season.all <- c(tp.rest,tp.other) 
  #tp.season.all <- c(tp.other) 
  
  rh.rest <- PR.season$rh.season[c_1:12]
  rh.other <- PR.season$rh.season.lag_1[c_2:12]
  #rh.season.all <- c(rh.other) 
  rh.season.all <- c(rh.rest,rh.other) 
  
  svpd.rest <- PR.season$svpd.season[d_1:12]
  svpd.other <- PR.season$svpd.season.lag_5[d_2:12]
  svpd.season.all <- c(svpd.rest, svpd.other) 
  #svpd.season.all <- c(svpd.other)   
  
  # Add to seasonal dataframe
  PR.season$t2m.season.all <- t2m.season.all
  PR.season$tp.season.all <- tp.season.all
  PR.season$rh.season.all <- rh.season.all
  PR.season$svpd.season.all <- svpd.season.all  
  return(PR.season)
}




# END OF SCRIPT