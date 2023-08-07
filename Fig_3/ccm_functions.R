#########################################################
# Functions for Convergent Cross-Mapping (CCM) Analyses #
#                                                       #
# nicole.nova@stanford.edu                              #
#########################################################
#nullMethod <- "seasonal"
#Tperiod <- 12
# Obtain seasonal or ebisuzaki surrogate time series for null distributions
# #为空分布获取季节性或ebisuzaki代理时间序列
getCCMresults <- function(nullMethod, Tperiod){
  # We first create surrogates, run CCM with the surrogates, and collect null distributions
  
  # Create surrogates
  
  # For Figure 8
  # driver xmap ir surrogates
  #make_surrogate_data: 这是一个包装器函数，用于使用几个不同的空模型生成代理时间序列。
  ir.sur <- make_surrogate_data(PR$ir, method = nullMethod, num_surr = num_sur, T_period = Tperiod)
  
  # Add the ir surrogates to PR
  for (j in 1:num_sur){
    PR[[paste("ir.sur_", j, sep="")]] <- ir.sur[,j]
  }
  
  # For Figure 7 or 9
  # ir xmap driver surrogates
  # 生成气候随机季节性变量
  for (i in 2:5) {
    driver.sur <- make_surrogate_data(PR[[i]], method = nullMethod, num_surr = num_sur, T_period = 12)
    assign(paste(names(PR[i]), ".sur", sep=""), driver.sur)
    
    # Add the driver surrogates to PR
    for (j in 1:num_sur){
      PR[[paste(names(PR[i]), ".sur_", j, sep="")]] <- driver.sur[,j]
    }
  }
  
  
  # CCM with null distributions
  
  # For Figure 8
  # Run CCM with each driver and surrogate ir
  # Run CCM for multiple surrogates and bootstraps: driver xmap ir
  #用每个驱动和代理案例运行CCM
  #为多个代理和bootstrap运行CCM:驱动xmap案例
  
  for (k in 2:5) {
    # Perform CCM for each driver xmap ir
    for(j in 1:num_sur){
      # Perform CCM for each surrogate
      c <- ccm(PR, E = get(paste("E_", names(PR[k]), sep="")), lib_column = names(PR[k]), 
               target_column = paste("ir.sur_", j, sep=""), lib_sizes = libs, RNGseed = 2301,
               num_samples = bootrun,
               random_libs = T, replace = F, stats_only = F,
               tau = Tau, exclusion_radius = excl_rad, tp = TP)
      
      assign(paste("ccmb.", names(PR[k]), "_xmap_ir.sur_", j, sep=""), c, envir = .GlobalEnv)
    }
  }
  
  ################################################################################################################# 
  # For Figure 7 or 9
  # Run CCM with ir and each surrogate driver
  # Run CCM for multiple surrogates and bootstraps: ir xmap driver
  for (k in 2:5) {
    # Perform CCM for each ir xmap driver
    for(j in 1:num_sur){
      # Perform CCM for each surrogate
      c <- ccm(PR, E = E_ir, lib_column = "ir", 
               target_column = paste(names(PR[k]), ".sur_", j, sep=""), lib_sizes = libs, 
               RNGseed = 2301, 
               num_samples = bootrun,
               random_libs = T, replace = F, stats_only = F,
               tau = Tau, exclusion_radius = excl_rad, tp = TP)
      
      assign(paste("ccmb.ir_xmap_", names(PR[k]), ".sur_", j, sep=""), c, envir = .GlobalEnv)
    }
  }
  
  save.image(file=paste("cache/CCM_null_", nullMethod, ".RData", sep=""))
  
  
  # Compute percentiles for seasonal null distributions
  # For Figure 8
  for (k in 2:5) {
    # Collect CCM results of all surrogates to obtain null distribution: ir
    null_ir = NULL
    for (j in 1:num_sur){
      local <- eval(parse(text = paste("ccmb.", names(PR[k]), "_xmap_ir.sur_", j, sep="")))
      null_ir <- rbind(null_ir, local[["CCM1_PredictStat"]])
    }
    
    # Sort by time series library size
    null_ir <- null_ir[order(null_ir$LibSize),]
    
    # Obtain 2.5%, 50% and 97.5% percentiles from null distribution  
    null_ir_q025 <- NULL
    null_ir_q500 <- NULL
    null_ir_q975 <- NULL
    for (i in 1:length(libs)){
      n <- subset(null_ir, LibSize == libs[i])    # quantile(null_ir[,i], 0.025)
      q025 <- quantile(n$rho, 0.025)
      q500 <- quantile(n$rho, 0.500)
      q975 <- quantile(n$rho, 0.975)
      
      
      null_ir_q025 <- rbind(null_ir_q025, q025)
      null_ir_q500 <- rbind(null_ir_q500, q500)
      null_ir_q975 <- rbind(null_ir_q975, q975)
    }
    assign(paste("null.", names(PR[k]), "_xmap_ir.q025", sep=""), null_ir_q025, envir = .GlobalEnv)
    assign(paste("null.", names(PR[k]), "_xmap_ir.q500", sep=""), null_ir_q500, envir = .GlobalEnv)
    assign(paste("null.", names(PR[k]), "_xmap_ir.q975", sep=""), null_ir_q975, envir = .GlobalEnv)
  }
  
  
  # For Figure 7 or 9
  for (k in 2:5) {
    # Collect CCM results of all surrogates to obtain null distribution: driver
    null_driver = NULL
    for (j in 1:num_sur){
      local <- eval(parse(text = paste("ccmb.ir_xmap_", names(PR[k]), ".sur_", j, sep="")))
      null_driver <- rbind(null_driver, local[["CCM1_PredictStat"]])
    }
    
    # Sort by time series library size
    null_driver <- null_driver[order(null_driver$LibSize),]
    
    # Obtain 2.5%, 50% and 97.5% percentiles from null distribution  
    null_driver_q025 <- NULL
    null_driver_q500 <- NULL
    null_driver_q975 <- NULL
    for (i in 1:length(libs)){
      n <- subset(null_driver, LibSize == libs[i])  # quantile(null_driver[,i], 0.025)
      q025 <- quantile(n$rho, 0.025)
      q500 <- quantile(n$rho, 0.500)
      q975 <- quantile(n$rho, 0.975)
      null_driver_q025 <- rbind(null_driver_q025, q025)
      null_driver_q500 <- rbind(null_driver_q500, q500)
      null_driver_q975 <- rbind(null_driver_q975, q975)
    }
    assign(paste("null.ir_xmap_", names(PR[k]), ".q025", sep=""), null_driver_q025, envir = .GlobalEnv)
    assign(paste("null.ir_xmap_", names(PR[k]), ".q500", sep=""), null_driver_q500, envir = .GlobalEnv)
    assign(paste("null.ir_xmap_", names(PR[k]), ".q975", sep=""), null_driver_q975, envir = .GlobalEnv)
  }
  
  save.image(file=paste("cache/CCM_null_", nullMethod, ".RData", sep=""))
}

#i<-2

# Plot the subplots for CCM figures 3 and S8
plot.ccm <- function(fig){
  
  var.col <- c("black", "red", "green3", "goldenrod2", "skyblue2")
  
  for (i in 2:5) {
    ir_driver_q <- get(paste("ir_", names(PR[i]), "_q", sep=""))
    null.ir_xmap_driver.q025 <- get(paste("null.ir_xmap_", names(PR[i]), ".q025", sep=""))
    null.ir_xmap_driver.q500 <- get(paste("null.ir_xmap_", names(PR[i]), ".q500", sep=""))
    null.ir_xmap_driver.q975 <- get(paste("null.ir_xmap_", names(PR[i]), ".q975", sep=""))
    
    # Plot forecast skill vs library size 
    # Plot driver xmap ir 
    plot(ir_driver_q[,2] ~ libs, col = var.col[i], type="l", lty = 1, ylim = c(0,1), lwd = 5, 
         cex.axis = 1.3, cex.lab = 1.1, cex.sub = 1.3,
         xlab="", 
         ylab="")
    #xlab="Number of data points included in cross-mapping", 
    #ylab=expression(paste("Correlation coefficient (",rho,")"))) 
    # median predictive skill vs library size 
    
    # Null model
    # Plot ir xmap driver surrogates (95% CI)
    polygon(c(libs, rev(libs)), c(null.ir_xmap_driver.q975, rev(null.ir_xmap_driver.q025)), 
            col = rgb(115/255, 115/255, 115/255, 0.4), border = NA)
    lines(null.ir_xmap_driver.q500 ~ libs, col = rgb(115/255, 115/255, 115/255, 0.4), 
          lwd = 5, lty = 1) # median
    
    # Plot ir cross-mapping driver
    polygon(c(libs, rev(libs)), c(ir_driver_q[,1], rev(ir_driver_q[,3])), 
            col = adjustcolor(var.col[i], alpha.f = 0.4), border = NA)
    lines(ir_driver_q[,2] ~ libs, col = var.col[i], lwd = 5, lty = 1) # median on top
    
    print(get(paste(names(PR[i]), "_ir_MK", sep=""))[[2]]) # Nonsensical (t<0)
    print(get(paste("ir_", names(PR[i]), "_MK", sep=""))[[2]]) # Sensical (t>0)
    
    nam <- c("Incidence","Temperature", "Precipitation","Relative humidity", "Saturation vapor pressure deficit")
    
    legend("topleft", cex = 1.3, pt.cex = 1, bty = "n",
           c(paste(nam[i], " drives incidence?", sep=""),
             paste(nam[i], " null model", sep="")), 
           lty=c(1,1), lwd = c(5,5), col=c(var.col[i], rgb(115/255, 115/255, 115/255, 0.8)))
    
    file_name = paste("output/",country_name, "/", fig,"/ccm_ir_", names(PR[i]), ".pdf", sep="")
    dev.copy(pdf, file=file_name)
    dev.off()
    
    
    
    print(names(PR[i]))
    print(paste("max predictive skill = ", max(ir_driver_q[,2])))
    print(paste("library size = ", (which(ir_driver_q[,2]==max(ir_driver_q[,2]),n))))
    print(paste("null_",names(PR[i])))
    print(paste("max predictive skill = ", max(null.ir_xmap_driver.q500)))
    print(paste("library size = ", (which(null.ir_xmap_driver.q500==max(null.ir_xmap_driver.q500),n))))
    
  }
}



# Plot the subplots for CCM Figure 8
plot.ccm.nonsense <- function(fig){
  var.col <- c("black", "red", "green3", "goldenrod2", "skyblue2")
  for (i in 2:5) {
    
    driver_ir_q <- get(paste(names(PR[i]), "_ir_q", sep=""))
    null.driver_xmap_ir.q025 <- get(paste("null.", names(PR[i]), "_xmap_ir.q025", sep=""))
    null.driver_xmap_ir.q500 <- get(paste("null.", names(PR[i]), "_xmap_ir.q500", sep=""))
    null.driver_xmap_ir.q975 <- get(paste("null.", names(PR[i]), "_xmap_ir.q975", sep=""))
    
    
    # Plot forecast skill vs library size 
    # Plot driver cross-mapping ir 
    plot(driver_ir_q[,2] ~ libs, col = "black", type = "l", lty = 1, ylim = c(0,1), lwd = 5, 
         cex.axis = 1.3, cex.lab = 1.1, cex.sub = 1.3,
         xlab="Number of data points included in cross-mapping", 
         ylab="Cross-map skill")
    #xlab="Number of data points included in cross-mapping", 
    #ylab=expression(paste("Cross-map skill (",rho,")"))) 
    # median predictive skill vs library size
    
    # Null model
    polygon(c(libs, rev(libs)), c(null.driver_xmap_ir.q975, rev(null.driver_xmap_ir.q025)), 
            col = rgb(115/255, 115/255, 115/255, 0.4), border = NA)
    lines(null.driver_xmap_ir.q500 ~ libs, col = rgb(115/255, 115/255, 115/255, 0.4), 
          lwd = 5, lty = 1) # median
    
    # Plot ir cross-mapping driver
    polygon(c(libs, rev(libs)), c(driver_ir_q[,1], rev(driver_ir_q[,3])), 
            col =  rgb(0, 0, 0, 0.4), border = NA)
    lines(driver_ir_q[,2] ~ libs, col = "black", lwd = 5, lty = 1) # median on top
    
    print(get(paste(names(PR[i]), "_ir_MK", sep=""))[[2]]) # Nonsensical (t<0)
    print(get(paste("ir_", names(PR[i]), "_MK", sep=""))[[2]]) # Sensical (t>0)
    
    nam <- c("Incidence","Temperature", "Precipitation","Relative humidity", "Saturation vapor pressure deficit" )
    
    
    legend("topleft", cex = 1.3, pt.cex = 1, bty = "n",
           c(paste("incidence drives ", nam[i],"?", sep=""), 
             paste(nam[i], " null model", sep="")),  
           lty=c(1,1), lwd = c(5,5), col=c("black", rgb(115/255, 115/255, 115/255, 0.8)))
    
    file_name = paste("output/",country_name, "/", fig,"/ccm_ir_", names(PR[i]), ".pdf", sep="") 
    dev.copy(pdf, file=file_name)
    dev.off()
  }
}



# Perform Kolmogorov-Smirnov tests between CCM of observations and CCM of null
ks.results <- function(driverName, test_num) {
  # Collect CCM results of all surrogates to obtain null distribution
  null_driver = NULL
  for (j in 1:num_sur){
    local <- eval(parse(text = paste("ccmb.ir_xmap_", driverName, ".sur_", j, sep="")))
    names(local[["LibMeans"]]) <- c("LibSize", "rho", paste(names(PR[i]),".sur",":ir", sep=""),"E","tau","tp","nn")
    null_driver <- rbind(null_driver, local[["LibMeans"]])
  }
  
  # Obtain distribution at 54 time series length for null
  n <- subset(null_driver, LibSize == test_num)
  null.ds <- n$rho
  
  # Get the distribution at 54 time series length for ir_xmap_driver
  ir_xmap_driver <- eval(parse(text = paste("ir_xmap_", driverName, sep="")))
  d <- subset(ir_xmap_driver[["LibMeans"]], LibSize == test_num)
  names(d) <- c("LibSize", "rho", paste(names(PR[i]),".sur",":ir", sep=""),"E","tau","tp","nn")
  driver.ds <- d$rho
  
  print(driverName)
  
  
  # KS tests
  r <- stats::ks.test(driver.ds, null.ds) # Compare distributions
  p_val <- round(r$p.value, 10)  # P values of KS tests
  print(p_val)
  
  d <- round(r$statistic, 3)   # D statistics of KS tests
  print(d)
  
  
}



# Plot the subplots for CCM figures 3 and S8
# Perform Kolmogorov-Smirnov tests between CCM of observations and CCM of null
plot.ccm.box <- function(fig,test_num) {
  null.ds <- matrix(NA,nrow=num_sur,ncol=4)
  colnames(null.ds) <- c("t2m", "tp", "rh", "svpd")
  driver.ds <- matrix(NA,nrow=1,ncol=4)
  colnames(driver.ds) <- c("t2m", "tp", "rh", "svpd")
  nam <-
  for (i in 2:5) {
    driverName <- names(PR[i])
    # Collect CCM results of all surrogates to obtain null distribution
    null_driver = NULL

    for (j in 1:num_sur){
      local <- eval(parse(text = paste("ccmb.ir_xmap_", driverName, ".sur_", j, sep="")))
      names(local[["LibMeans"]]) <- c("LibSize", "rho", paste(names(PR[i]),".sur",":ir", sep=""),"E","tau","tp","nn")
      null_driver <- rbind(null_driver, local[["LibMeans"]])
      }
    
    # Obtain distribution at 54 time series length for null
    n <- subset(null_driver, LibSize == test_num)
    null.ds[,(i-1)] <- n$rho
    
    
    # Get the distribution at 54 time series length for ir_xmap_driver
    ir_xmap_driver <- eval(parse(text = paste("ir_xmap_", driverName, sep="")))
    d <- subset(ir_xmap_driver[["LibMeans"]], LibSize == test_num)
    names(d) <- c("LibSize", "rho", paste(names(PR[i]),".sur",":ir", sep=""),"E","tau","tp","nn")
    driver.ds[i-1] <- d$rho
    
  }
    ##########plot

  
    boxplot(null.ds,
          ylim=c(0,1),
          ylab='Cross-map skill',
          xlab='Thailand')
          #main =paste("Dengue cross-map",nam[i]) )
    
    points(driver.ds[,1:4],
         pch=8,
         col='red',
         cex=2,
         lwd=2)
    
    file_name = paste("output/",country_name, "/", fig,"/ccm_box_ir_climate.pdf", sep="") 
    dev.copy(pdf, file=file_name)
    dev.off()
    
    write.csv(null.ds, paste("output/",fig,"/",country_name, "_ccm_box_ir_null-ds.csv", sep="") )
    write.csv(driver.ds, paste("output/",fig,"/",country_name, "_ccm_box_ir_driver-ds.csv", sep="") )
    
  
}


# END OF SCRIPT