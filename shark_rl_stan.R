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
# if(file.exists("stanmodeloutput.rdata")){
#   load("stanmodeloutput.rdata")
# } else{allmodels<-new.env()}

stop("DO NOT JUST AUTO SOURCE!")
############################HC Modeling:############################
#Bump up the iterations;
shark_stan_HC<-shark_stan_prep(shark_split_HC)
SH_otto_nolapse_l1_HC=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod.stan',
                        data=shark_stan_HC,verbose=FALSE,save_warmup=FALSE,
                        pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                        include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
save(SH_otto_nolapse_l1_HC,file = "stan_scripts/stan_output/SH_otto_nolapse_l1_HC.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC)
################################
SH_otto_nolapse_l1_HC_wshark=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod_wshark.stan',
                                  data=shark_stan_HC,verbose=FALSE,save_warmup=FALSE,
                                  pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                                  include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
save(SH_otto_nolapse_l1_HC_wshark,file = "stan_scripts/stan_output/SH_otto_nolapse_l1_HC_wshark.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC_HBeta_wshark)
##################Investigate:#######################################
#Only the Healthy Controls
stan_outfit<-allmodels$SH_otto_nolapse_l1_HC
stan_out<-data.frame(ID=shark_stan_HC$ID,
                     alpha=summary(stan_outfit,pars=c('alpha'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MF=summary(stan_outfit,pars=c('beta_1_MF'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MB=summary(stan_outfit,pars=c('beta_1_MB'),probs=c(0.5))$summary[,'50%'],
                     beta_2=summary(stan_outfit,pars=c('beta_2'),probs=c(0.5))$summary[,'50%'],
                     pers=summary(stan_outfit,pars=c('pers'),probs=c(0.5))$summary[,'50%'],row.names = NULL)
stan_out$GROUP<-as.character(bdf$GROUP1245[match(stan_out$ID,bdf$ID)])

#Who are high on beta:
shark_split_HC_HBeta<-shark_split_all[high_mb_hc]
shark_stan_HC_HBeta<-shark_stan_prep(shark_split_HC_HBeta)
SH_otto_nolapse_l1_HC_HBeta=stan(file='stan_scripts/otto_nolapse_lambda1_jcmod.stan',
                                 data=shark_stan_HC_HBeta,verbose=FALSE,save_warmup=FALSE,
                                 pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                                 include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
save(SH_otto_nolapse_l1_HC_HBeta,file = "stan_scripts/stan_output/SH_otto_nolapse_l1_HC_HBeta.rdata")
launch_shinystan(SH_otto_nolapse_l1_HC_HBeta)

#Alll Together now!
coef_shark<-coef(m1shark)$ID
high_MB_all<-rownames(coef_shark)[which(coef_shark$`RewardTypeNo Reward:TransitionRare` > 0.55)]
shark_split_MB_all<-shark_split_all[high_MB_all]
shark_stan_MB_all<-shark_stan_prep(shark_split_MB_all)
run_shark_stan(data_list=shark_stan_MB_all,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_nodecay.stan',
               modelname="SH_otto_nolapse_l1_MB_nodecay",stan_args="default",assignresult=T,
               savepath="stan_scripts/stan_output",open_shinystan=T)
stan_outfit<-SH_otto_nolapse_l1_MB_nodecay
stan_out<-data.frame(ID=shark_stan_MB_all$ID,
                     alpha=summary(stan_outfit,pars=c('alpha'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MF=summary(stan_outfit,pars=c('beta_1_MF'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MB=summary(stan_outfit,pars=c('beta_1_MB'),probs=c(0.5))$summary[,'50%'],
                     beta_2=summary(stan_outfit,pars=c('beta_2'),probs=c(0.5))$summary[,'50%'],
                     pers=summary(stan_outfit,pars=c('pers'),probs=c(0.5))$summary[,'50%'],row.names = NULL)
stan_out$GROUP<-as.character(bdf$GROUP1245[match(stan_out$ID,bdf$ID)])

####Let's take out pers too;
run_shark_stan(data_list=shark_stan_MB_all,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_nodecay_nopers.stan',
               modelname="SH_otto_nolapse_l1_MB_nodecay_nopers",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=T)
#####Re-try the original with the model cleaned up; 
shark_stan_all<-shark_stan_prep(shark_split = shark_split_all)
run_shark_stan(data_list=shark_stan_all,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_all",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=T)
stan_outfit<-SH_otto_nolapse_l1_all
stan_out<-data.frame(ID=shark_stan_all$ID,
                     alpha=summary(stan_outfit,pars=c('alpha'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MF=summary(stan_outfit,pars=c('beta_1_MF'),probs=c(0.5))$summary[,'50%'],
                     beta_1_MB=summary(stan_outfit,pars=c('beta_1_MB'),probs=c(0.5))$summary[,'50%'],
                     beta_2=summary(stan_outfit,pars=c('beta_2'),probs=c(0.5))$summary[,'50%'],
                     pers=summary(stan_outfit,pars=c('pers'),probs=c(0.5))$summary[,'50%'],row.names = NULL)
stan_out$GROUP<-as.character(bdf$GROUP1245[match(stan_out$ID,bdf$ID)])
low_pers<-stan_out$ID[which(abs(stan_out$pers) < 0.15)]
#####We now look into people who are low on perseveration 
shark_stan_lowpers<-shark_stan_prep(shark_split = shark_split_all[low_pers])
run_shark_stan(data_list=shark_stan_lowpers,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_lowpers",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=T)

shark_stan_HC<-shark_stan_prep(shark_split = shark_split_HC)
run_shark_stan(data_list=shark_stan_HC,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_betadist.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_betadist_HC",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=T)
#Who are high on beta:
shark_split_HC_HBeta<-shark_split_all[high_mb_hc]
shark_stan_HC_HBeta<-shark_stan_prep(shark_split_HC_HBeta)
run_shark_stan(data_list=shark_stan_HC_HBeta,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_betadist.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_betadist_HCHMB",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=T)
#All HMB
shark_stan_HMB<-shark_stan_prep(shark_split_MB_all[high_MB_all])
run_shark_stan(data_list=shark_stan_HMB,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_betadist.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_betadist_HMB",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=F)
#Everyone:
shark_stan_all<-shark_stan_prep(shark_split_all)
run_shark_stan(data_list=shark_stan_all,stanfile='stan_scripts/otto_nolapse_lambda1_jcmod_betadist.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_nolapse_l1_betadist_all",stan_args="default",assignresult=T,iter = 4000,
               savepath="stan_scripts/stan_output",open_shinystan=F)
SH_otto_betadist_all_log_lik<-shark_get_log_lik(stan_fitoutput = SH_otto_nolapse_l1_betadist_all$stanfit_SH_otto_nolapse_l1_betadist_all,
                                                data_list = shark_stan_all)
names(SH_otto_betadist_all_log_lik)<-shark_stan_all$ID
################Let's add motor perseveration:###########
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#Let's run these models:
#So we know motor_pre and pre_choice is good. we will keep them; now try unified first stage betas but leave second one alone;
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_ubeta.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_ubeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)
##Now let's run the basic model with shark in health controls...
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_wShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_wShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#Now let's run the basic model on all subjects.....
run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_all",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#Try shark with group level 
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_wIndviShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_wIndiviShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

###Currently the best script is probably otto_l1_betadist_mp.stan

#Okay, now we clean up and try repara beta...
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_expbeta.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_expBeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)





get_summary_df(output_ls = SH_otto_l1_betadist_mp_all,pars = c("beta_1_MB"),returnas = "data.frame",probs = 0.5)

everyone<-lapply(shark_split_HC,function(dfx){
  nsg<-list()
  for(i in 1:nrow(dfx)){
    if(i==1){
    alpha_b=1
    beta_b=1}
    
    if(!is.na(dfx$state[i]) & !is.na(dfx$choice1[i])){
    if ((dfx$state[i]- dfx$choice1[i]-1)) {alpha_b = alpha_b + 1;} else {beta_b = beta_b + 1;}
    mu_b = ((alpha_b) / (alpha_b+beta_b));
    } else {mu_b = NA}
    nsg[[i]]=data.frame(mu_b=mu_b,alpha_b=alpha_b,beta_b=beta_b)
  }
  do.call(rbind,nsg)

})


#############OLDER MODELS###################
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
     
     
     
     
