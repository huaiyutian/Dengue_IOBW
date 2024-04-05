library(dplyr)
library(testcorr)
library(ggplot2)
library(tidyr)
library(caret)

#===============================================================================
# plot 1: ID sensitivity analysis
#===============================================================================
ir_data<-read.csv('02_output/12_aeg_with_iobw_sens_ID.csv')
# Research Country
countrys <- unique(ir_data$country)

dir.create("02_output/01_sim_id_aeg")
result_all_id <- c()
for (i in 1:length(countrys)){
  #Extract all variables of the specified country
  ir_sim_id_c <- subset(ir_data,country == countrys[i] & time >= 485)
  
  #Cyclic standardization + determines whether pre cases are all 0
  ir_sim_id_c_std <- c()
  for (j in 1:51) { 
    #Standardization
    ir_sim_id_c_spe <- ir_sim_id_c %>% 
      filter(ID == j) %>% 
      mutate(pre_cases_std = scale(pre_cases),
             obs_cases_std = scale(obs_cases))
    
    ir_sim_id_c_std <- rbind(ir_sim_id_c_std, ir_sim_id_c_spe)
  }
  
  ir_sim_id_pre_95 <- ir_sim_id_c_std %>% 
    select(time,pre_cases,pre_cases_std) %>%
    as_tibble() %>%
    group_by(time) %>%
    summarize(
      lo_std=quantile(pre_cases_std,prob=0.05,na.rm=TRUE),
      hi_std=quantile(pre_cases_std,prob=0.95,na.rm=TRUE),
      med_std=mean(pre_cases_std,na.rm=TRUE)
    ) %>%
    ungroup()
  
  if(all(ir_sim_id_pre_95$med_std == "NaN")) {
    ir_sim_id_pre_95$lo_std <- 0
    ir_sim_id_pre_95$hi_std <- 0
    ir_sim_id_pre_95$med_std <- 0
  }
  
  #obs case
  ir_sim_id_obs <- subset(ir_sim_id_c_std, ID == 51)
  
  ir_sim_id_95 <-merge(ir_sim_id_obs,ir_sim_id_pre_95, by=c("time")) %>%
    select(ID,IC,country,obs_cases,obs_cases_std,lo_std,hi_std,med_std) %>%
    mutate(date = seq(as.Date("2014/4/1"), by = "month", length.out = 69)) #Convert to monthly
  
  #Prevention predicted cases are 0
  if(all(ir_sim_id_pre_95$med_std == 0)) {
    ans_best <- data.frame("lag" = "0", "R" = "na", "t_P" = "na", "t_robust_P" = "na", "hb_P" = "na", "hb_robust_P" = "na")
  } else {
    ans <- cc.test(ir_sim_id_95$med_std, ir_sim_id_95$obs_cases_std, max.lag = 5, plot = F)
    ans_best <- data.frame("lag" = ans$lag, "R" = round(ans$cc, 2),
                           "t_P" = round(ans$pvt, 3), "t_robust_P" = round(ans$pvttilde, 3),
                           "hb_P" = round(ans$pvhb, 3), "hb_robust_P" = round(ans$pvqtilde, 3)) %>% filter(lag ==0)
  }
  
  ##############################################################################
  #Plot
  ir_sim_id_p <- ir_sim_id_95 %>%
    ggplot(aes(x = date)) + xlab("Time (month)") + ylab("stdalized cases") +
    geom_ribbon(aes(ymax=hi_std, ymin=lo_std, group = 1), fill="dodgerblue4", alpha =0.3) + 
    geom_line(aes(y = med_std, group = 1, color="Prediction"), size = 0.9) + 
    geom_line(aes(y = obs_cases_std, color="Observation"), size = 0.9,linetype = "dashed")  +
    scale_color_manual(values = c(Prediction = "dodgerblue4", Observation = "red2"))+
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", 
                 limits=as.Date(c("2014/1/1","2020/1/1")), expand=c(0,0), date_labels = "%Y")+
    geom_abline(slope = 0, intercept = 0,linetype = "dashed", color = "grey40", lwd = 0.5) +
    # scale_y_continuous(limits=c(0,ceiling(max(ir_sim_id_95$hi_std[2:72],ir_sim_id_95$obs_cases_std[2:72])))) + #!!!记得修改
    ggtitle(paste(countrys[i], " (r=", ans_best$R, " p=", ans_best$hb_robust_P, ")", sep="")) + 
    theme_test(base_size = 15)+ 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(size = .5,linetype = "dashed", color = "grey40"),
          #panel.grid.minor.x = element_line(size = .5,linetype = "dashed", color = "grey40"),
          axis.title.x = element_text(margin = margin(t = 10), size = 13),
          axis.title.y = element_text(margin = margin(r = 10), size = 13)) +
    theme(legend.position = "none") +
    theme(plot.margin=unit(rep(1,4),'lines'))
  ir_sim_id_p
  file_name = paste("02_output/01_sim_id_aeg/",i,"_",countrys[i],".pdf",sep="")
  ggsave(file_name, ir_sim_id_p, width = 5, height = 5)
  
  #-------------------------------------------------------------------------------
  reslut_id_95_c <- merge(ir_sim_id_95,ans_best) %>% select(-lag)
  result_all_id <- rbind(result_all_id, reslut_id_95_c)
}
write.csv(result_all_id,"02_output/01_sim_id_aeg/01_sim_id_aeg.csv")

#===============================================================================
# plot 2: IC sensitivity analysis
#===============================================================================
ir_data<-read.csv('02_output/13_aeg_with_iobw_sens_IC.csv')
# Research Country
countrys <- unique(ir_data$country)

dir.create("02_output/02_sim_ic_aeg")
result_all_ic <- c()

for (i in 1:length(countrys)){
  #Extracts all variables for the specified country
  ir_sim_ic_c <- subset(ir_data, country == countrys[i] & time >= 485)

  
  #Cyclic standardization + determines whether pre cases are all 0
  ir_sim_ic_c_std <- c()
  for (j in 1:50) { 
    # Standardization
    ir_sim_ic_c_spe <- ir_sim_ic_c %>% 
      filter(IC == j)%>% 
      mutate(pre_cases_std = scale(pre_cases),
             obs_cases_std = scale(obs_cases))
    
    ir_sim_ic_c_std <- rbind(ir_sim_ic_c_std, ir_sim_ic_c_spe)
  }
  
  ir_sim_ic_pre_95 <- ir_sim_ic_c_std %>% 
    select(time,pre_cases,pre_cases_std) %>%
    as_tibble() %>%
    group_by(time) %>%
    summarize(
      lo_std=quantile(pre_cases_std,prob=0.05,na.rm=TRUE),
      hi_std=quantile(pre_cases_std,prob=0.95,na.rm=TRUE),
      med_std=mean(pre_cases_std,na.rm=TRUE)
    ) %>%
    ungroup()
  
  if(all(ir_sim_ic_pre_95$med_std == "NaN")) {
    ir_sim_ic_pre_95$lo_std <- 0
    ir_sim_ic_pre_95$hi_std <- 0
    ir_sim_ic_pre_95$med_std <- 0
  }
  
  #obs case
  ir_sim_ic_obs <- subset(ir_sim_ic_c_std, IC == 18)
  
  ir_sim_ic_95 <-merge(ir_sim_ic_obs,ir_sim_ic_pre_95, by=c("time")) %>%
    select(ID,IC,country,obs_cases,obs_cases_std,lo_std,hi_std,med_std) %>%
    mutate(date = seq(as.Date("2014/4/30"), by = "month", length.out = 69)) #Convert to monthly
  
  #Prevention predicted cases are 0
  if(all(ir_sim_ic_pre_95$med_std == 0)) {
    ans_best <- data.frame("lag" = "0", "R" = "na", "t_P" = "na", "t_robust_P" = "na", "hb_P" = "na", "hb_robust_P" = "na")
  } else {
    ans <- cc.test(ir_sim_ic_95$med_std, ir_sim_ic_95$obs_cases_std, max.lag = 5, plot = F)
    ans_best <- data.frame("lag" = ans$lag, "R" = round(ans$cc, 2),
                           "t_P" = round(ans$pvt, 3), "t_robust_P" = round(ans$pvttilde, 3),
                           "hb_P" = round(ans$pvhb, 3), "hb_robust_P" = round(ans$pvqtilde, 3)) %>% filter(lag ==0)
  }
  
  ##############################################################################
  #plot
  ir_sim_ic_p <- ir_sim_ic_95 %>%
    ggplot(aes(x = date)) + xlab("Time (month)") + ylab("stdalized cases") +
    geom_ribbon(aes(ymax=hi_std, ymin=lo_std, group = 1), fill="dodgerblue4", alpha =0.3) + 
    geom_line(aes(y = med_std, group = 1, color="Prediction"), size = 0.9) + 
    geom_line(aes(y = obs_cases_std, color="Observation"), size = 0.9,linetype = "dashed")  +
    scale_color_manual(values = c(Prediction = "dodgerblue4", Observation = "red2"))+
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", 
                 limits=as.Date(c("2014/1/1","2020/1/1")), expand=c(0,0), date_labels = "%Y")+
    geom_abline(slope = 0, intercept = 0,linetype = "dashed", color = "grey40", lwd = 0.5) +
    ggtitle(paste(countrys[i], " (r=", ans_best$R, " p=", ans_best$hb_robust_P, ")", sep="")) + 
    theme_test(base_size = 15)+ 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(size = .5,linetype = "dashed", color = "grey40"),
          #panel.grid.minor.x = element_line(size = .5,linetype = "dashed", color = "grey40"),
          axis.title.x = element_text(margin = margin(t = 10), size = 13),
          axis.title.y = element_text(margin = margin(r = 10), size = 13)) +
    theme(legend.position = "none") +
    theme(plot.margin=unit(rep(1,4),'lines'))
  ir_sim_ic_p
  file_name = paste("02_output/02_sim_ic_aeg/",i,"_",countrys[i],".pdf",sep="")
  ggsave(file_name, ir_sim_ic_p, width = 5, height = 5)
  
  #-------------------------------------------------------------------------------
  reslut_ic_95_c <- merge(ir_sim_ic_95,ans_best) %>% select(-lag)
  result_all_ic <- rbind(result_all_ic, reslut_ic_95_c)
}
write.csv(result_all_ic,"02_output/02_sim_ic_aeg/02_sim_ic_aeg.csv")


#===============================================================================
# plot 3: ratio sensitivity analysis
#===============================================================================
ir_data<-read.csv('02_output/14_aeg_with_iobw_sens_ratio.csv')
# Research Country
countrys <- unique(ir_data$country)

dir.create("02_output/03_sim_ratio_aeg")
result_all_ratio <- c()

for (i in 1:length(countrys)){
  #Extracts all variables for the specified country
  ir_sim_ratio_c <- subset(ir_data, country == countrys[i] & time >= 485)
  
  #Cyclic standardization + determines whether pre cases are all 0
  ir_sim_ratio_c_std <- c()
  for (j in 1:25) { 
    # Standardization
    ir_sim_ratio_c_spe <- ir_sim_ratio_c %>% 
      filter(ratio == j)%>% 
      mutate(pre_cases_std = scale(pre_cases),
             obs_cases_std = scale(obs_cases))
    
    ir_sim_ratio_c_std <- rbind(ir_sim_ratio_c_std, ir_sim_ratio_c_spe)
  }
  
  ir_sim_ratio_pre_95 <- ir_sim_ratio_c_std %>% 
    select(time,pre_cases,pre_cases_std) %>%
    as_tibble() %>%
    group_by(time) %>%
    summarize(
      lo_std=quantile(pre_cases_std,prob=0.05,na.rm=TRUE),
      hi_std=quantile(pre_cases_std,prob=0.95,na.rm=TRUE),
      med_std=mean(pre_cases_std,na.rm=TRUE)
    ) %>%
    ungroup()
  
  if(all(ir_sim_ratio_pre_95$med_std == "NaN")) {
    ir_sim_ratio_pre_95$lo_std <- 0
    ir_sim_ratio_pre_95$hi_std <- 0
    ir_sim_ratio_pre_95$med_std <- 0
  }
  
  #obs case
  ir_sim_ratio_obs <- subset(ir_sim_ratio_c_std, ratio == 2)
  
  ir_sim_ratio_95 <-merge(ir_sim_ratio_obs,ir_sim_ratio_pre_95, by=c("time")) %>%
    select(ID,IC,country,obs_cases,obs_cases_std,lo_std,hi_std,med_std) %>%
    mutate(date = seq(as.Date("2014/4/1"), by = "month", length.out = 69)) #Convert to monthly
  
  #Prevention predicted cases are 0
  if(all(ir_sim_ratio_pre_95$med_std == 0)) {
    ans_best <- data.frame("lag" = "0", "R" = "na", "t_P" = "na", "t_robust_P" = "na", "hb_P" = "na", "hb_robust_P" = "na")
  } else {
    ans <- cc.test(ir_sim_ratio_95$med_std, ir_sim_ratio_95$obs_cases_std, max.lag = 5, plot = F)
    ans_best <- data.frame("lag" = ans$lag, "R" = round(ans$cc, 2),
                           "t_P" = round(ans$pvt, 3), "t_robust_P" = round(ans$pvttilde, 3),
                           "hb_P" = round(ans$pvhb, 3), "hb_robust_P" = round(ans$pvqtilde, 3)) %>% filter(lag ==0)
  }
  
  ##############################################################################
  #plot
  ir_sim_ratio_p <- ir_sim_ratio_95 %>%
    ggplot(aes(x = date)) + xlab("Time (month)") + ylab("stdalized cases") +
    geom_ribbon(aes(ymax=hi_std, ymin=lo_std, group = 1), fill="dodgerblue4", alpha =0.3) + 
    geom_line(aes(y = med_std, group = 1, color="Prediction"), size = 0.9) + 
    geom_line(aes(y = obs_cases_std, color="Observation"), size = 0.9,linetype = "dashed")  +
    scale_color_manual(values = c(Prediction = "dodgerblue4", Observation = "red2"))+
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", 
                 limits=as.Date(c("2014/1/1","2020/1/1")), expand=c(0,0), date_labels = "%Y")+
    geom_abline(slope = 0, intercept = 0,linetype = "dashed", color = "grey40", lwd = 0.5) +
    ggtitle(paste(countrys[i], " (r=", ans_best$R, " p=", ans_best$hb_robust_P, ")", sep="")) + 
    theme_test(base_size = 15)+ 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(size = .5,linetype = "dashed", color = "grey40"),
          axis.title.x = element_text(margin = margin(t = 10), size = 13),
          axis.title.y = element_text(margin = margin(r = 10), size = 13)) +
    theme(legend.position = "none") +
    theme(plot.margin=unit(rep(1,4),'lines'))
  ir_sim_ratio_p
  file_name = paste("03_sim_ratio_aeg/",i,"_",countrys[i],".pdf",sep="")
  ggsave(file_name, ir_sim_ratio_p, width = 5, height = 5)
  
  #-------------------------------------------------------------------------------
  reslut_ratio_95_c <- merge(ir_sim_ratio_95,ans_best) %>% select(-lag)
  result_all_ratio <- rbind(result_all_ratio, reslut_ratio_95_c)
}
write.csv(result_all_ratio,"02_output/03_sim_ratio_aeg/03_sim_ratio_aeg.csv")
