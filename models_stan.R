#########################################################
# Poisson Model
# Implementation in STAN
# Code for the manuscript:
# Modelling COVID-19 With More Disaggregation and Less Nomothetic Parameterisation: UK and India Examples
# Olsen et al.
#########################################################
# Authors:
# Jihye Kim, Arkadiusz Wisniowski
# Social Statistics Department, UOM
#########################################################
# Run the Poisson  
# using STAN
# Requires:  rstan, tidyverse, tictoc

rm(list=ls(all=TRUE))
library(rstan)
library("tidyverse")
library("tictoc")
pkgbuild::has_build_tools(debug = TRUE)
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
#########################################################
######### download data and sampling for model  #########
#########################################################
# reading individual (sample1.csv) and district-level data (combineddata_1806.csv)
# making cummurative frequnecy data using counts of samples each district (table1.csv)

setwd("D:/NFHS/")
sample1 <- read.csv("sample1.csv")
districtmean <- read.csv("combineddata_1806.csv")
write.csv(table(sample1$district_id), "table1.csv")
table1 <- read.csv("table1.csv")
ctab1=c(0,cumsum(table1$Freq))

########################################################
##########DATA cleaning for Models #####################
########################################################

line.data <-list( "ctab1" = ctab1,
                  "female" = sample1$female,
                  "urban" = sample1$urban,
                  "sc" = sample1$sc,
                  "st" = sample1$st,
                  "smoke" = sample1$smoke,
                  "health" = sample1$health,
                  "wealth1" = sample1$q1,
                  "fwt" = sample1$fwt,
                  "district_id" = sample1$district_id,
                  "deaths" = districtmean$deaths,
                  "age65up" = districtmean$age65up-mean(districtmean$age65up),
                  "obesity" = districtmean$obesity-mean(districtmean$obesity),
                  "underweight" = districtmean$underweight-mean(districtmean$underweight),
                  "migration2" = districtmean$migration2-mean(districtmean$migration2),
                  "pop_1000" = districtmean$pop_1000,
                  "N"=69837, "G"=345)

length(unique(line.data$district_id))

########################################################
##########m####### Model 2 #############################
########################################################

model <- "
 data {                              

   // Covariates
   int  N;
   int  G;
   int <lower=0, upper=1> female[N];
   int <lower=0, upper=1> urban[N];  
   int <lower=0, upper=1> sc[N];  
   int <lower=0, upper=1> st[N]; 
   int <lower=0, upper=1> smoke[N];  
   int <lower=0> health[N];
   int <lower=0> wealth1[N];
   int <lower=1> fwt[N];
   int <lower=1, upper=G> district_id[N];
   real age65up[G];
   real obesity[G];
   real underweight[G];
   real migration2[G];
   real <lower=115> pop_1000[G];
   // Count outcome
   int <lower=0> deaths[G];
   int ctab1[G+1];                        
}

parameters {
real alpha[12];
}

transformed parameters  {

  real mu[N];
  real mean_mu[G];

  for (i in 1:N) {
     // Linear predictor 
     mu[i] = exp(alpha[1]+ alpha[2]*female[i]+ alpha[3]*urban[i]+ alpha[4]*sc[i] + alpha[5]*st[i] + alpha[6]*smoke[i] + alpha[7]*health[i]
      + alpha[8]*wealth1[i] + log(fwt[i])
      + alpha[9]*age65up[district_id[i]]
      + alpha[10]*obesity[district_id[i]]
      + alpha[11]*underweight[district_id[i]]
      + alpha[12]*migration2[district_id[i]]);            // eq1  Vulnerability to severe COVID-19
   }

  for (j in 1:G) {   
     mean_mu[j]=mean(mu[(ctab1[j]+1):ctab1[j+1]]);        // eq2  Calculating the average vulnerability rate by district
    }
}
model {

  alpha ~ normal(0, 1);                                   // define priors for coefficients

  for (j in 1:G) { 
   deaths[j] ~ poisson(mean_mu[j]*pop_1000[j]);           // eq3 Risk Equation for death by COVID-19
 }
 }
"
init <- function() list(alpha=rep(0.1, 12))

tic()
fit <- stan(model_code = model, data = line.data, init=init, iter = 800, warmup=300, chains = 2)
toc()

########################################################
##########m####### Plots   #############################
########################################################
plot(fit, pars=c("alpha"))
pairs(fit, pars=c("alpha"))
print(fit, pars = c("alpha"))
saveRDS(fit, "fit.wealth.rds")
