#Shark RL with Stan
library(rstan)
library(shinystan)
#library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
source("shark_utility.R")
reloaddata<-F
if(reloaddata){
  source("behaviroal/shark_beh_analyses_import_process.R")
} else {load(file = "shark1.RData") }

#The data frame to use is bdf;
bdfx<-bdf[order(bdf$ID),]
#bdfx<-bdfx[which(bdfx$GROUP1245=="1"),]
shark_split_all<-split(bdfx,bdfx$ID)

shark_split_HC<-shark_split_all[sapply(shark_split_all, function(dfx){unique(dfx$GROUP1245)=="1"})]
shark_stan<-shark_stan_prep(shark_split_all)
#Models
if(file.exists("stanmodeloutput.rdata")){
  load("stanmodeloutput.rdata")
} else{allmodels<-new.env()}


###Model starts here:
SH_daw_nolapse_l1=stan(file='stan_scripts/daw_nolapse_lambda1.stan',
                       data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                       pars=c('lp_','prev_choice','tran_count','tran_type','Q_TD','Q_MB','Q_2','delta_1','delta_2'),
                       include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.01))
launch_shinystan(SH_daw_nolapse_l1)
assign("SH_daw_nolapse_l1",SH_daw_nolapse_l1,envir = allmodels)

save(allmodels,file = "stanmodeloutput.rdata")


#Bump up the iterations;
SH_otto_nolapse_l1=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod.stan',
                       data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                       pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                       include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
assign("SH_otto_nolapse_l1",SH_otto_nolapse_l1,envir = allmodels)
launch_shinystan(SH_otto_nolapse_l1)
save(allmodels,file = "stanmodeloutput.rdata")
########################################################
#Bump up the iterations;
shark_stan_HC<-shark_stan_prep(shark_split_HC)
SH_otto_nolapse_l1_HC=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod.stan',
                        data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                        pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                        include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
assign("SH_otto_nolapse_l1_HC",SH_otto_nolapse_l1_HC,envir = allmodels)
launch_shinystan(SH_otto_nolapse_l1_HC)
save(allmodels,file = "stan_scripts/stan_output/SH_otto_nolapse_l1_HC.rdata")
#Investigate:
stan_outfit<-allmodels$SH_otto_nolapse_l1_HC
stan_out<-data.frame(ID=shark_stan_HC$ID,
                     alpha=summary(stan_outfit,pars=c('alpha'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MF=summary(stan_outfit,pars=c('beta_1_MF'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MB=summary(stan_outfit,pars=c('beta_1_MB'),probs=c(0.5))$summary[,'50%'],
                     beta_2=summary(stan_outfit,pars=c('beta_2'),probs=c(0.5))$summary[,'50%'],
                     pers=summary(stan_outfit,pars=c('pers'),probs=c(0.5))$summary[,'50%'],row.names = NULL)
stan_out$GROUP<-as.character(bdf$GROUP1245[match(stan_out$ID,bdf$ID)])

#Who are high on beta:
shark_split_HC_HBeta<-shark_split_all[stan_out$ID[which(stan_out$beta_1_MB>median(stan_out$beta_1_MB))]]
shark_stan_HC_HBeta<-shark_stan_prep(shark_split_HC_HBeta)
SH_otto_nolapse_l1_HC_HBeta=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod.stan',
                           data=shark_stan_HC_HBeta,verbose=FALSE,save_warmup=FALSE,
                           pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                           include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
assign("SH_otto_nolapse_l1_HC_HBeta",SH_otto_nolapse_l1_HC_HBeta,envir = allmodels)
save(allmodels,file = "stanmodeloutput.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC_HBeta)
stan_outfit<-allmodels$SH_otto_nolapse_l1_HC_HBeta
stan_out<-data.frame(ID=shark_stan_HC_HBeta$ID,
                     alpha=summary(stan_outfit,pars=c('alpha'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MF=summary(stan_outfit,pars=c('beta_1_MF'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MB=summary(stan_outfit,pars=c('beta_1_MB'),probs=c(0.5))$summary[,'50%'],
                     beta_2=summary(stan_outfit,pars=c('beta_2'),probs=c(0.5))$summary[,'50%'],
                     pers=summary(stan_outfit,pars=c('pers'),probs=c(0.5))$summary[,'50%'],row.names = NULL)

stan_out$GROUP<-as.character(bdf$GROUP1245[match(stan_out$ID,bdf$ID)])
deltas<-shark_extract_delta(nSub = length(shark_split_HC_HBeta),stan_outfit = allmodels$SH_otto_nolapse_l1_HC_HBeta)
################################
SH_otto_nolapse_l1_HC_HBeta_wshark=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod_wshark.stan',
                                 data=shark_stan_HC_HBeta,verbose=FALSE,save_warmup=FALSE,
                                 pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                                 include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
assign("SH_otto_nolapse_l1_HC_HBeta_wshark",SH_otto_nolapse_l1_HC_HBeta_wshark,envir = allmodels)
save(allmodels,file = "stanmodeloutput.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC_HBeta_wshark)
################################
SH_otto_nolapse_l1_HC_HBeta_wgrp=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod_wgroup.stan',
                                        data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                                        pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                                        include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
assign("SH_otto_nolapse_l1_HC_HBeta_wgrp",SH_otto_nolapse_l1_HC_HBeta_wgrp,envir = allmodels)
save(allmodels,file = "stanmodeloutput.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC_HBeta_wgrp) 

##############STOP FIGURE OUT THE BASE MODEL BEFORE ADVANCING#####################
SH_otto_nolapse_l1_shark=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod_wshark.stan',
                        data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                        pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                        include=FALSE,iter=8000,control=list(adapt_delta=0.99,stepsize=.01))
print('running new model') 
assign("SH_otto_nolapse_l1_shark",SH_otto_nolapse_l1_shark,envir = allmodels)
launch_shinystan(SH_otto_nolapse_l1_shark)
save(allmodels,file = "stanmodeloutput.rdata")


SH_otto_nolapse_l1_wShark_grpDiff=stan(file='stan_scripts/otto_nolapse_lambda1_wShark_grp.stan',
                        data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                        pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                        include=FALSE,iter=8000,control=list(adapt_delta=0.99,stepsize=.01))
print('running new model') 
assign("SH_otto_nolapse_l1_wShark_grpDiff",SH_otto_nolapse_l1_wShark_grpDiff,envir = allmodels)
launch_shinystan(SH_otto_nolapse_l1_wShark_grpDiff)
save(allmodels,file = "stanmodeloutput.rdata")


