###############################################
# Functions for Scenario Exploration Analyses #
#                                             #
# nicole.nova@stanford.edu                    #
###############################################
#data <- PR
#cols <- c( rep('cases', E.max+1), 'temp', 'prec', 'mu')  #c( rep('ir', E.max+1), 't2m', 'tp', 'rh', 'svpd')
#delays <-c( 0, seq(0, by = -tau, length.out = E.max),-lag.temp, -lag.prec, 0) 
#c( 0, seq(0, by = -tau, length.out = E.max),-lag.t2m, -lag.tp, -lag.rh, -lag.svpd)
#diff_col <- c(TRUE, rep(FALSE, E.max + 3))
  #c(TRUE, rep(FALSE, E.max + 4))

# Takes an input matirx or data frame and creates a block of lag coordinates for state space reconstruction
make_block <- function(data, cols, delays, 
                       lib = c(1, NROW(data)), 
                       diff_col = rep(FALSE, length(cols)))
{ 
  # INPUTS:
  #   data - matrix, array, or data.frame with time series variables arranged in columns
  #   cols - vector indices or names of the columns of 'data' to use for each column of block
  #   delays - vector with same length as cols specifying the time displacement for that column
  #   diff_col - vector of logical on whether to apply first differencing to this lag coordinate variable
  #
  # OUTPUT:
  #   block - array with length(cols) of columns, and NROW(data) rows
  #输入:
  # data -将时间序列变量按列排列的矩阵、数组或数据帧
  # cols - vector索引或'data'的列的名称，用于块的每个列
  # delays -与cols相同长度的向量，指定该列的时间位移
  # diff_col - vector逻辑上是否应用优先差分到这个滞后的坐标变量
  #输出:
  # block -长度为(cols)的列，和NROW(数据)行的数组
  
  if(!is.numeric(cols)){
    cols <- as.numeric(factor(levels = colnames(data), x = cols))
  }
  
  lib <- matrix(lib, ncol = 2)  #???
  # data <- as.matrix(data)
  
  ncol <- length(cols)
  nrow <- dim(data)[1]
  block <- as.data.frame(array(NA, dim = c(nrow, ncol)))
  names(block) <- 1:ncol
  
  for (i in 1:ncol)
  {
    I <- 1:nrow
    I_delay <- intersect(I, I + delays[i])
    block[I_delay-delays[i], i] <- data[I_delay, cols[i]]
    if (delays[i] < 0){
      # remove data points that fall at start of lib segments
      block[lib[, 1] - (0:(delays[i] + 1)), i] <- NA
      names(block)[i] <- paste(colnames(data)[cols[i]], '_t-', abs(delays[i]), sep="")  
    } else if (delays[i] > 0) {
      # remove data points that fall at end of lib segments
      block[lib[, 2] - (0:(delays[i] - 1)), i] <- NA
      names(block)[i] <- paste(colnames(data)[cols[i]], '_t+', abs(delays[i]), sep="")  
    } else {
      names(block)[i] <- paste(colnames(data)[cols[i]], '_t', sep="")
    }
    
    if (diff_col[i]){
      block[, i] <- c(NA, diff(block[, i]))
    }
  } # i
  
  
  return(block)
}



scenario_exploration <- function(block,
                                 target_column = 1,
                                 explore_column,
                                 columns = 1:NCOL(block),
                                 delta = 0.05,
                                 lib = c(1,NROW(block)),
                                 pred = c(1,NROW(block)),
                                 ...){
  
  pred <- pred + NROW(block)
  
  block.plus <- block 
  block.plus[, explore_column] = block.plus[, explore_column] + delta/2
  
  block.minus <- block
  block.minus[, explore_column] = block.minus[, explore_column] - delta/2
  
  block.plus <- bind_rows(block, block.plus)
  block.minus <- bind_rows(block, block.minus)
  
  results.plus <- block_lnlp(
    block = block.plus,
    target_column = target_column,
    lib = lib,
    pred = pred,
    stats_only = FALSE,
    #short_output = TRUE,
    method = 's-map',
    num_neighbors = 0,
    theta = L_theta_star_embeds[embed_SE],
    tp = tp,
    first_column_time = TRUE)
  
  results.minus <- block_lnlp(
    block=block.minus,
    target_column = target_column,
    lib = lib,
    pred = pred,
    stats_only = FALSE,
    #short_output = TRUE,
    method = 's-map',
    num_neighbors = 0,
    theta = L_theta_star_embeds[embed_SE],
    tp = tp,
    first_column_time = TRUE)
  
  num <- L_theta_star_embeds[embed_SE]

  results.plus <- results.plus[["model_output"]][[1]]%>%  #theta数
    rename(pred.plus = Predictions)
  
  results.minus <- results.minus[["model_output"]][[1]] %>%
    rename(pred.minus = Predictions)
  
  
  ################!!!!!!!!!!!!!!!!!!!
  results <- full_join(results.plus,
                       results.minus,
                       by = c("time", "Observations")) %>%
    #mutate(delta=pred.plus-pred.minus)            # Only necessary when reporting normalized values
    mutate(delta = (pred.plus-pred.minus)/delta )

    return(results)
}



# Normalization functions
norm_rMM <- function(v){
  sqrt((v - min(v, na.rm=TRUE)) / ( max(v, na.rm = TRUE) - min(v, na.rm=TRUE) ))
}


norm_MM <- function(v){
  (v - min(v, na.rm=TRUE)) / ( max(v, na.rm = TRUE) - min(v, na.rm=TRUE) ) 
}  


norm_coeff <- function(v, v_mean, v_sd) {
  (v - v_mean) / v_sd
} 





# END OF SCRIPT