#############################
# Data Formatting Functions #
#############################


# Load data--------------------------------------------
data.clean <- function(dataset,i){
  
  # We load the data and create our main dataframe df,
  # dataset  <- "01_dengue-climate_monthly_2014-2019_24c"
  file_name      = paste("01_data/", dataset, ".csv", sep="")
  data           <- read.csv(file_name, header = T)
  data$date      <- as.Date(data$date)
  country_name   <- unique(data$country)
  data_country   <- subset(data,data$country== country_name[i])
  print(paste0(i,"_",country_name[i]))
  return(data_country)
}


# Add lagged and averaged lagged variables  -------------------------------
data.process <- function(data_country){
  
  vbs          <- c("ir", "case","t2m", "tp", "rh", "svpd","iobw")
  data_country <- data_country[, c("date", "month", vbs)]
  # Next, we add new lagged variables
  for (i in 0:6){
    data[[paste("ir.lag_", i, sep="")]] <- data.table::shift(data$ir, i)
  }
  
  for (i in 0:6){
    data[[paste("t2m.lag_", i, sep="")]] <- data.table::shift(data$t2m, i)
  }
  
  for (i in 0:6){
    data[[paste("tp.lag_", i, sep="")]] <- data.table::shift(data$tp, i)
  }
  
  for (i in 0:6){
    data[[paste("rh.lag_", i, sep="")]] <- data.table::shift(data$rh, i)
  }
  
  for (i in 0:6){
    data[[paste("svpd.lag_", i, sep="")]] <- data.table::shift(data$svpd, i)
  }  

  # Then, we add new averaged lagged variables
  for (i in 0:19){
    PR[[paste("cases.avglag_", i, sep="")]] <- (PR[[paste("cases.lag_", i, sep="")]] +
                                                  PR[[paste("cases.lag_", i+1, sep="")]] +
                                                  PR[[paste("cases.lag_", i+2, sep="")]] +
                                                  PR[[paste("cases.lag_", i+3, sep="")]] +
                                                  PR[[paste("cases.lag_", i+4, sep="")]] +
                                                  PR[[paste("cases.lag_", i+5, sep="")]] +
                                                  PR[[paste("cases.lag_", i+6, sep="")]])/7
  }
  
  for (i in 0:19){
    PR[[paste("temp.avglag_", i, sep="")]] <- (PR[[paste("temp.lag_", i, sep="")]] +
                                                 PR[[paste("temp.lag_", i+1, sep="")]] +
                                                 PR[[paste("temp.lag_", i+2, sep="")]] +
                                                 PR[[paste("temp.lag_", i+3, sep="")]] +
                                                 PR[[paste("temp.lag_", i+4, sep="")]] +
                                                 PR[[paste("temp.lag_", i+5, sep="")]] +
                                                 PR[[paste("temp.lag_", i+6, sep="")]])/7
  }
  
  for (i in 0:19){
    PR[[paste("prec.avglag_", i, sep="")]] <- (PR[[paste("prec.lag_", i, sep="")]] +
                                                 PR[[paste("prec.lag_", i+1, sep="")]] +
                                                 PR[[paste("prec.lag_", i+2, sep="")]] +
                                                 PR[[paste("prec.lag_", i+3, sep="")]] +
                                                 PR[[paste("prec.lag_", i+4, sep="")]] +
                                                 PR[[paste("prec.lag_", i+5, sep="")]] +
                                                 PR[[paste("prec.lag_", i+6, sep="")]])/7
  }
  
  for (i in 0:19){
    PR[[paste("mu.avglag_", i, sep="")]] <- (PR[[paste("mu.lag_", i, sep="")]] +
                                               PR[[paste("mu.lag_", i+1, sep="")]] +
                                               PR[[paste("mu.lag_", i+2, sep="")]] +
                                               PR[[paste("mu.lag_", i+3, sep="")]] +
                                               PR[[paste("mu.lag_", i+4, sep="")]] +
                                               PR[[paste("mu.lag_", i+5, sep="")]] +
                                               PR[[paste("mu.lag_", i+6, sep="")]])/7
  }
  
  return(data_country)
}


# Create a dataframe with seasonal trends
data.season <- function(PR,a,b,c,d) {
  # The seasonal variables of ir, t2m, tp, and rh are averages over all years
  # Create a vector with 12 weeks of a year
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
  for (i in 0:6){
    PR.season[[paste("ir.season.lag_", i, sep="")]] <- data.table::shift(PR.season$ir.season, i)
  }
  
  for (i in 0:6){
    PR.season[[paste("t2m.season.lag_", i, sep="")]] <- data.table::shift(PR.season$t2m.season, i)
  }
  
  for (i in 0:6){
    PR.season[[paste("tp.season.lag_", i, sep="")]] <- data.table::shift(PR.season$tp.season, i)
  }
  
  for (i in 0:6){
    PR.season[[paste("rh.season.lag_", i, sep="")]] <- data.table::shift(PR.season$rh.season, i)
  }
 
  for (i in 0:6){
    PR.season[[paste("svpd.season.lag_", i, sep="")]] <- data.table::shift(PR.season$svpd.season, i)
  }
  
  #change lag_
  a_1 <- 12-a+1
  a_2 <- a+1
  b_1 <- 12-b+1
  b_2 <- b+1
  c_1 <- 12-c+1
  c_2 <- c+1
  d_1 <- 12-d+1
  d_2 <- d+1
  # Get the last 3 data points (rows 44 - 12) from t2m.season, tp.season and append in the beginning
  t2m.rest <- PR.season$t2m.season[a_1:12]
  t2m.other <- PR.season$t2m.season.lag_3[a_2:12]
  t2m.season.all <- c(t2m.rest, t2m.other) 
  
  tp.rest <- PR.season$tp.season[b_1:12]
  tp.other <- PR.season$tp.season.lag_2[b_2:12]
  tp.season.all <- c(tp.rest,tp.other) 
  
  rh.rest <- PR.season$rh.season[c_1:12]
  rh.other <- PR.season$rh.season.lag_1[c_2:12]
  rh.season.all <- c(rh.rest,rh.other) 
  
  svpd.rest <- PR.season$svpd.season[d_1:12]
  svpd.other <- PR.season$svpd.season.lag_5[d_2:12]
  svpd.season.all <- c(svpd.rest, svpd.other) 
  
  # Add to seasonal data frame
  PR.season$t2m.season.all <- t2m.season.all
  PR.season$tp.season.all <- tp.season.all
  PR.season$rh.season.all <- rh.season.all
  PR.season$svpd.season.all <- svpd.season.all  
  return(PR.season)
}