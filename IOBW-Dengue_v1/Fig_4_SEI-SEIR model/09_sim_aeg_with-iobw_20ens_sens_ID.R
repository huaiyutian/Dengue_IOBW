library(knitr)
opts_chunk$set(
  progress=TRUE,
  prompt=FALSE,tidy=FALSE,highlight=TRUE,
  purl=FALSE,
  strip.white=TRUE,
  warning=FALSE,
  message=FALSE,
  error=FALSE,
  echo=TRUE,
  cache=TRUE,
  cache.extra=rand_seed,
  dev="png",
  dev.args=c(bg="white"),
  dpi=150,
  results='markup',
  fig.show='asis',
  size='small',
  fig.align='left',
  fig.path=paste0("figure/",params$prefix,"-"),
  cache.path=paste0("cache/",params$prefix,"-"))

library(plyr)
library(tidyverse)
library(magrittr)
library(tibble)
library(stringi)
library(pomp)
library(panelPomp)
library(foreach)
library(iterators)
library(doRNG)
library(aakmisc) ## available at https://kingaa.github.io/
stopifnot(getRversion()>="4.0")
stopifnot(packageVersion("pomp")>="3.0.3")
stopifnot(packageVersion("aakmisc")>="0.26.2")
options(
  stringsAsFactors=FALSE,
  keep.source=TRUE,
  encoding="UTF-8"
)
set.seed(407958184)

library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(plotly)
library(cowplot)
library(scales)
theme_set(theme_bw())

dir.create("02_output/01_sim-aeg_with-iobw_sens")

#================================================================================
Combnation <- read_csv("02_output/19_best_comb_aeg.csv")
CaseAll <- read_csv("01_data/01_dengue_mt_14_19.csv")
CoverAll <- read_csv("01_data/02_covar_day_13-19_20ens.csv")

# Select Country
countrys <- unique(Combnation$country)
# Initial scale
init.cond <- read_csv("01_data/04_init.cond.csv")
# Thermal response parameters
trait_posterior <- read_csv("01_data/05_aeg_traits.csv")

start_time_all <- Sys.time()

for (i in 1:13) {
  sim_all <- c()
  
  country_id <- countrys[i]
  IC_id <- Combnation$IC[i]
  ratio_id <- Combnation$ratio[i]
  
  
  CaseData <- CaseAll %>% filter(country ==countrys[i]) %>% 
    select(time,cases,country)
  CoverData <- CoverAll %>% filter(country ==countrys[i]) %>% 
    select(-old_time, -month, -day)
  
  for (j in IC_id){
    init <- init.cond[j,]
    
    for (k in 1:51) {
      trait <- trait_posterior[k,]
      
      for (ratio in ratio_id) {
        start_time <- Sys.time()
        
        Cover_trait <- cbind(CoverData, trait, ratio)
        
        all_data <- CaseData %>% 
          right_join(Cover_trait, by=c("time","country")) 
        
        #================================================================================
        seic_step <- Csnippet("
        //=========================Ae. aegypti vector parameters function==============================
        // Parameter values are defined as follows:
        double a,b,EFD,PDR,pMI,pEA,MDR,mu;
        
        // EFD: Eggs laid per female per day (briere)  
        if((temp_with_iobw < EFD_T0) || (temp_with_iobw > EFD_Tm)){
        EFD =0.0;}
        else{
        EFD =(EFD_c)*temp_with_iobw*(temp_with_iobw-EFD_T0)*sqrt(EFD_Tm-temp_with_iobw);}
        
        // pEA: Probability of mosquito egg-to-adult survival  (quadratic)
        if((temp_with_iobw < pEA_T0) || (temp_with_iobw > pEA_Tm)){
        pEA =0.0; }
        else{
        pEA =(pEA_c)*(temp_with_iobw-pEA_T0)*(temp_with_iobw-pEA_Tm);}
        
        // MDR: Mosquito egg-to-adult development rate (day−1)  (briere)  
        if((temp_with_iobw < MDR_T0) || (temp_with_iobw > MDR_Tm)){
        MDR =0.0;}
        else{
        MDR =(MDR_c)*temp_with_iobw*(temp_with_iobw-MDR_T0)*sqrt(MDR_Tm-temp_with_iobw);}
        
        // a: Biting rate (day−1) (briere)
        if((temp_with_iobw < a_T0) || (temp_with_iobw > a_Tm)){
        a =0.0;}
        else{
        a =(a_c)*temp_with_iobw*(temp_with_iobw-a_T0)*sqrt(a_Tm-temp_with_iobw);}
        
        // pMI: Probability of mosquito infection (briere) 
        if((temp_with_iobw < pMI_T0) || (temp_with_iobw > pMI_Tm)){
        pMI =0.0;}
        else{
        pMI =(pMI_c)*temp_with_iobw*(temp_with_iobw-pMI_T0)*sqrt(pMI_Tm-temp_with_iobw);}
        
        // mu: Adult mosquito lifespan (days)  (inverted_quadratic)
        if((temp_with_iobw < mu_th_T0) || (temp_with_iobw > mu_th_Tm)){
        mu =24; }
        else{
        mu =1/((mu_th_c)*(temp_with_iobw-mu_th_T0)*(temp_with_iobw-mu_th_Tm));}
        
        // PDR: Parasite development rate (day−1) (briere)  
        if((temp_with_iobw < PDR_T0) || (temp_with_iobw > PDR_Tm)){
        PDR =0.0;}
        else{
        PDR =(PDR_c)*temp_with_iobw*(temp_with_iobw-PDR_T0)*sqrt(PDR_Tm-temp_with_iobw);}
        
        // b: Probability of mosquito infectiousness (briere) 
        if((temp_with_iobw < b_T0) || (temp_with_iobw > b_Tm)){
        b =0.0;}
        else{
        b =(b_c)*temp_with_iobw*(temp_with_iobw-b_T0)*sqrt(b_Tm-temp_with_iobw);}
        
        //----------------------------------------------------
        // K: Adult mosquito carrying capacity
        double T29 =29.0; 
        double EFD_T29, pEA_T29, MDR_T29, mu_T29;
        if((T29 < EFD_T0) || (T29 > EFD_Tm)){
        EFD_T29 =0.0;} 
        else{
        EFD_T29 =(EFD_c)*T29*(T29-EFD_T0)*sqrt(EFD_Tm-T29);}
        
        if((T29 < pEA_T0) || (T29 > pEA_Tm)){
        pEA_T29 =0.0; }
        else{
        pEA_T29 =(pEA_c)*(T29-pEA_T0)*(T29-pEA_Tm);}
        
        
        if((T29 < MDR_T0) || (T29 > MDR_Tm)){
        MDR_T29 =0.0;}
        else{
        MDR_T29 =(MDR_c)*T29*(T29-MDR_T0)*sqrt(MDR_Tm-T29);}
        
        if((T29 < mu_th_T0) || (T29 > mu_th_Tm)){
        mu_T29 =24; }
        else{
        mu_T29 =1/((mu_th_c)*(T29-mu_th_T0)*(T29-mu_th_Tm));}
        
        //----------------------------------------------------
        double kappa =8.617e-05;
        double EA =0.5;
        double N =(S+E+I+C)*2;
        
        //----------------------------------------------------
        double alpha =(EFD_T29*pEA_T29*MDR_T29*(1.0/mu_T29)-mu_T29)/(EFD_T29*pEA_T29*MDR_T29*(1.0/mu_T29));
        double K =alpha*N*exp(-EA*(pow((temp_with_iobw-T29),2))/(kappa*(temp_with_iobw+273.0)*(T29+273.0)));
        
        //==========================mosquito parameter ==========================
        // Breeding, death and infection of susceptible mosquitos
        // B:birth D:death M:immigration
        double dM1B;
        
        //----------------------------------------------------
        // If the number of mosquitoes is greater than the maximum environmental carrying capacity, then the maximum number of mosquitoes =the maximum environmental carrying capacity K; otherwise, the maximum number of mosquitoes =(M1+M2+M3)*(1-((M1+M2+M3)/K)
        
        if((1-((M1+M2+M3)/K)) <=0){
        //dM1B =(EFD*pEA*MDR*(1.0/mu))*(K)*dt;}
        dM1B =(0)*dt;}
        else{
        dM1B =(EFD*pEA*MDR*(1.0/mu))*(M1+M2+M3)*(1-((M1+M2+M3)/K))*dt;}
        
        //----------------------------------------------------
        double dM1D =mu*M1*dt;
        double dM12 =(a*pMI*I/(S+E+I+C))*M1*dt;
        // Expose the latent and dead mosquito vectors
        double dM23 =PDR*M2*dt;
        double dM2D =mu*M2*dt;
        // mosquito-borne deaths
        double dM3D =mu*M3*dt;
        // ==========================human parameter ==========================
        // Birth,death, cross-protection and infection in susceptible populations
        double dSB =BR/1000*S*dt;
        double dSD =DR/1000*S*dt;
        double dCS =(1/(2.2*365))*C*dt;  //2.2 years ->2.2*365 days
        double dSE =a*b*(M3/(S+E+I+C))*S*dt;
        //Latentperiod and death of exposed people
        double dEI =(1/5.9)*E*dt;
        double dED =DR/1000*E*dt;
        //Recovery and death of infected people
        double dIC =(1/5.0)*I*dt;
        double dID =DR/1000*I*dt;
        //Cross protection of rehabilitated people
        double dCD =DR/1000*C*dt;
        
        //==========================differential equation==========================
        // compute equations
        M1 +=dM1B - dM1D - dM12;
        M2 +=dM12 - dM23 - dM2D;
        M3 +=dM23 - dM3D;
        S  +=dCS - dSE + (dSB - dSD);
        E  +=dSE - dEI - dED;
        I  +=dEI - dIC - dID;
        C  +=dIC - dCS - dCD;
        T  +=dEI;           // true incidence
        
        // track errors
        if (M1 < 0.0) { err -=M1; M1=0.0; }
        if (M2 < 0.0) { err -=M2; M2=0.0; }
        if (M3 < 0.0) { err -=M3; M3=0.0; }
        if (S < 0.0) { err -=S; S=0.0; }
        if (E < 0.0) { err -=E; E=0.0; }
        if (I < 0.0) { err -=I; I=0.0; }
        if (C < 0.0) { err -=C; C=0.0; }
                              ")
        
        seic_rinit <- Csnippet("
        //K: The carrying capacity of adult mosquitoes ~~ the total number of mosquitoes when time=0
        double T29 =29.0; 
        double EFD_T29, pEA_T29, MDR_T29, mu_T29;
        
        if((T29 < EFD_T0) || (T29 > EFD_Tm)){
        EFD_T29 =0.0;} 
        else{
        EFD_T29 =(EFD_c)*T29*(T29-EFD_T0)*sqrt(EFD_Tm-T29);}
        
        if((T29 < pEA_T0) || (T29 > pEA_Tm)){
        pEA_T29 =0.0; }
        else{
        pEA_T29 =(pEA_c)*(T29-pEA_T0)*(T29-pEA_Tm);}
        
        if((T29 < MDR_T0) || (T29 > MDR_Tm)){
        MDR_T29 =0.0;}
        else{
        MDR_T29 =(MDR_c)*T29*(T29-MDR_T0)*sqrt(MDR_Tm-T29);}
        
        if((T29 < mu_th_T0) || (T29 > mu_th_Tm)){
        mu_T29 =24; }
        else{
        mu_T29 =1/((mu_th_c)*(T29-mu_th_T0)*(T29-mu_th_Tm));}
        
        //----------------------------------------------------
        double kappa =8.617e-05;
        double EA =0.5;
        //----------------------------------------------------
        double alpha =(EFD_T29*pEA_T29*MDR_T29*(1.0/mu_T29)-mu_T29)/(EFD_T29*pEA_T29*MDR_T29*(1.0/mu_T29));
        double K_0 =alpha*(ratio*pop)*exp(-EA*(pow((temp_with_iobw-T29),2))/(kappa*(temp_with_iobw+273.0)*(T29+273.0)));
        // ratio: mos vs human
        M1 =nearbyint(K_0*ms_0);
        M2 =nearbyint(K_0*me_0);
        M3 =nearbyint(K_0*mi_0);
        
        double nn = pop;
        S =nearbyint(nn*hs_0);
        E =nearbyint(nn*he_0);
        I =nearbyint(nn*hi_0);
        C =nearbyint(nn*hc_0);
        T =0;
        err =0;
                               ")
        
        #ng bin (Mean: observed case, variance: dispersion coefficient 0.1)
        seic_dmeas <- Csnippet("
        double size =T;
        double prob =0.1;
        double tol =1.0e-18;
        lik =dnbinom(cases, size, prob+tol, 0) + tol; 
                               if (give_log) lik =log(lik);
                               ")
        
        seic_rmeas<- Csnippet("
        double size =T;
        double prob =0.1;
        cases =rnbinom(size,prob);
        if (cases > 0.0) {
        cases =nearbyint(cases);
        } else {
                              cases =0.0;}
                              ")
        
        covar <- covariate_table(
          t=Cover_trait$time, temp_with_iobw=Cover_trait$temp_with_iobw, ratio=Cover_trait$ratio,
          pop=Cover_trait$pop,BR=Cover_trait$BR,DR=Cover_trait$DR,
          a_T0=Cover_trait$a_T0, a_Tm=Cover_trait$a_Tm, a_c=Cover_trait$a_c,
          pEA_T0=Cover_trait$pEA_T0, pEA_Tm=Cover_trait$pEA_Tm, pEA_c=Cover_trait$pEA_c,
          EFD_T0=Cover_trait$EFD_T0, EFD_Tm=Cover_trait$EFD_Tm, EFD_c=Cover_trait$EFD_c,
          mu_th_T0=Cover_trait$mu_th_T0, mu_th_Tm=Cover_trait$mu_th_Tm, mu_th_c=Cover_trait$mu_th_c,
          MDR_T0=Cover_trait$MDR_T0, MDR_Tm=Cover_trait$MDR_Tm, MDR_c=Cover_trait$MDR_c,
          PDR_T0=Cover_trait$PDR_T0, PDR_Tm=Cover_trait$PDR_Tm, PDR_c=Cover_trait$PDR_c,
          b_T0=Cover_trait$b_T0, b_Tm=Cover_trait$b_Tm, b_c=Cover_trait$b_c,
          pMI_T0=Cover_trait$pMI_T0, pMI_Tm=Cover_trait$pMI_Tm, pMI_c=Cover_trait$pMI_c,
          times="t") 
        
        t0 <- CoverData$time[which(Cover_trait$date=="2013/1/1")]
        paramnames <- c("ms_0","me_0","mi_0","hs_0","he_0","hi_0","hc_0")
        params_guess <- c(ms_0=init$m1, me_0=init$m2, mi_0=init$m3,
                          hs_0=init$s,  he_0=init$e,  hi_0=init$i,  hc_0=init$r)
        partrans <- parameter_trans(logit=c("ms_0","me_0","mi_0","hs_0","he_0","hi_0","hc_0"))
        
        CaseData %>% 
          filter(time >=t0 ) %>%
          select(cases,time) %>%  
          pomp(
            t0=t0,
            times="time",
            rprocess=euler(step.fun=seic_step,delta.t=1),#Differential equation simulation
            rinit=seic_rinit,#initial value
            rmeasure=seic_rmeas,#Case count
            dmeasure=seic_dmeas,#Case assessment
            covar=covar, #concomitant variable
            accumvars  =c('T','err'),
            statenames =c('S','E','I','C','M1','M2','M3','T','err'),
            paramnames =paramnames,
            params=params_guess,
            partrans=partrans
          ) -> measSEIC
        
        sim <- simulate(measSEIC,format="data.frame") %>%
          mutate(IC =init$IC) %>%
          rename(pre_cases =cases)
        
        sim_country <-merge(sim,all_data, by=c("time"))%>%
          rename(obs_cases =cases) %>%
          select(ratio,ID,IC,country,date,obs_cases,pre_cases,
                 M1,M2,M3,S,E,I,C,T,err,everything()) %>%
          select(-.id) 
        
        sim_all <- rbind(sim_all,sim_country)
        
        print(paste("country=", i, "IC=", j, "ID=", k, "ratio=", ratio))          }
    }
  }

  total_elapsed_time <- Sys.time() - start_time_all
  print(paste("Total elapsed time for country ", i, ": "))
  print(total_elapsed_time)
  
  write.csv(sim_all, paste("02_output/01_sim-aeg_with-iobw_sens/", i, "_", countrys[i], "_ID.csv"))
}

total_elapsed_time_all <- Sys.time() - start_time_all
print("Total elapsed time for all countries: ")
print( total_elapsed_time_all)

