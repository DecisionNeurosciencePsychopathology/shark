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
dfx<-read.csv("./scheck/all_wexcludes_subj_data_V1_2018-12-13.csv",stringsAsFactors = F)
dfx$step_one_chosen_stim[which(is.na(dfx$step_one_response))]<-NA
dfx$step_one_rt[which(is.na(dfx$step_one_response))]<-NA
dfx$step_two_chosen_stim[which(is.na(dfx$step_two_response))]<-NA
dfx$step_two_rt[which(is.na(dfx$step_two_response))]<-NA
dfx$choice1<-as.numeric(dfx$step_one_chosen_stim)
dfx$choice2<-as.numeric(dfx$step_two_chosen_stim)
dfx$rts1<-as.numeric(dfx$step_one_rt)/1000
dfx$rts2<-as.numeric(dfx$step_two_rt)/1000
dfx$state<-as.numeric(dfx$step_two_state)
dfx$RewardType<-ifelse(as.logical(dfx$step_two_reward),"Reward","No Reward")
dfx$keycode1<-as.numeric(dfx$step_one_response=="left")
dfx$keycode2<-as.numeric(dfx$step_two_response=="left")
dfx$GROUP1245<-0
dfx$sharkattack<-FALSE
dfx$ifSharkBlock<-FALSE
vb_sp<-split(dfx,dfx$ID)

stop("STOP BEFORE RUNNING ALL MODELS")
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
               modelname="SH_otto_l1_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#Let's run these models:
#So we know motor_pre and pre_choice is good. we will keep them; now try unified first stage betas but leave second one alone;
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_ubeta.stan',add_data = list(factorizedecay=0),
               modelname="Otto_l1_betadist_mp_ubeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

##Now let's run the basic model with shark in health controls...
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_wShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_wShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#########Now let's run the basic model on all subjects.....###########
run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_all",stan_args="default",assignresult=T,iter = 4000,forcererun = T,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#Try shark with group level 
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_wIndviShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_wIndiviShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

###Currently the best script is probably otto_l1_betadist_mp.stan
##We try this with Vanessa's data
run_shark_stan(data_list=shark_stan_prep(vb_sp),stanfile='stan_scripts/otto_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="VB_otto_l1_betadist_mp",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)



l#Okay, now we clean up and try repara beta...
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_expbeta.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_expBeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)
#didn't work; try it with Shark?
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_expbeta_withShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_expBeta_wShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_l1_betadist_mp_expbeta_withShark.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_expBeta_wShark_all",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_ubeta2_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_jc_l1_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp_ubeta2indiv.stan',add_data = list(factorizedecay=0),
               modelname="SH_otto_l1_betadist_mp_ubeta2indiv_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_ubeta2_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_ubeta2_all",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2indiv.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_ubeta2indiv_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)
run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2indiv.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_ubeta2indiv_all",stan_args="default",assignresult=T,iter = 1000,forcererun = F,chains = 1,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2indiv.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc2_l1_betadist_mp_ubeta2indiv_hc",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc2_l1_betadist_mp_ubeta2_HC",stan_args="default",assignresult=T,iter = 2000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc2_l1_betadist_mp_ubeta2_all",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)





run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_spBeta.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_spBeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/ottojcmod_betadist_ubeta_wShark.stan',add_data = list(factorizedecay=0),
               modelname="ottojcmod_betadist_ubeta_wShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)



run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/ottojcmod_betadist_ubeta_wShark.stan',add_data = list(factorizedecay=0),
               modelname="ottojcmod_betadist_ubeta_wdShark_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="ottojcmod_l1_betadist_mp_ubeta2_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta2.stan',add_data = list(factorizedecay=0),
               modelname="ottojcmod_l1_betadist_mp_ubeta2_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

#######Let's go back to the basics:
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_l1_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="otto_l1_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ubeta1.stan',add_data = list(factorizedecay=0),
               modelname="otto_jc_l1_betadist_mp_ubeta1_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,
               savepath="stan_scripts/stan_output",open_shinystan=F)





nS<-length(shark_split_HC)
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/shahar_l1_betadist_mp_ogbeta.stan',add_data = list(factorizedecay=0),
               modelname="shahar_l1_betadist_mp_ogbeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,init= shark_initfun,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/shahar_mod_l1_betadist_nobeta.stan',add_data = list(factorizedecay=0),
               modelname="shahar_mod_l1_betadist_nobeta_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,init= shark_initfun,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/shahar_l1_betadist_nobeta.stan',add_data = list(factorizedecay=0),
               modelname="shahar_l1_betadist_nobeta_HC",stan_args="default",assignresult=T,iter = 5000,forcererun = F,chains = 4,init=0,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/shahar_jcmod_l1_betadist_nobeta.stan',add_data = list(factorizedecay=0),
               modelname="shahar_jcmod_l1_betadist_nobeta_HC",stan_args="default",assignresult=T,iter = 5000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(vb_sp),stanfile='stan_scripts/shahar_jcmod_l1_betadist_nobeta.stan',add_data = list(factorizedecay=0),
               modelname="shahar_jcmod_l1_betadist_nobeta_VB",stan_args="default",assignresult=T,iter = 5000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_all),stanfile='stan_scripts/RL_betadist_mp_SH.stan',add_data = list(factorizedecay=0),
               modelname="RL_betadist_mp_SH_all",stan_args="default",assignresult=T,iter = 4000,forcererun = T,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/RL_betadist_mp_SH.stan',add_data = list(factorizedecay=0),
               modelname="RL_betadist_mp_SH_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)


####Corredted for TD Learning;
run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/RL_bp_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="RL_bp_betadist_mp_HC",stan_args="default",assignresult=T,iter = 5000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/RL_1l_bp_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="RL_1l_bp_betadist_mp_HC",stan_args="default",assignresult=T,iter = 5000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/RL_gx_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="RL_gx_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = F,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

run_shark_stan(data_list=shark_stan_prep(shark_split_HC),stanfile='stan_scripts/RLdaw_gx_betadist_mp.stan',add_data = list(factorizedecay=0),
               modelname="RLdaw_gx_betadist_mp_HC",stan_args="default",assignresult=T,iter = 4000,forcererun = T,chains = 4,
               savepath="stan_scripts/stan_output",open_shinystan=F)

rxj<-shark_mkinit(1)
optimizing()
fitxr <- stan(file="stan_scripts/RL_betadist_mp_SH.stan",data = shark_stan_prep(shark_split_HC),iter=1, chains=1,seed=424546151, algorithm="Fixed_param")


system(paste0("rclone sync ",file.path(getwd())," box:skinner/projects_analyses/Shark -L"))
rstan::expose_stan_functions(fitxr)



