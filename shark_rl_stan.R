#Shark RL with Stan
library(rstan)
library(shinystan)
#library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = 2)

reloaddata<-F
if(reloaddata){
  source("behaviroal/shark_beh_analyses_import_process.R")
} else {load(file = "shark1.RData") }

#The data frame to use is bdf;
bdfx<-bdf[order(bdf$ID),]
shark_split<-split(bdfx,bdfx$ID)

nSub=length(shark_split)
nTrial=max(sapply(shark_split,nrow))
shark_stan<-list(
choice=array(0,dim=c(nSub,nTrial,2)),
state=array(0,dim=c(nSub,nTrial)),
sharkblock=array(0,dim=c(nSub,nTrial)), #we might want to do another layer of the model.... 
reward=array(0,dim=c(nSub,nTrial)),
miss_choice=array(0,dim=c(nSub,nTrial,2)),
miss_reward=array(0,dim=c(nSub,nTrial)),
ID=array(0,dim=nSub)
)
for(s in 1:nSub) {
  shark_stan$ID[s]<-names(shark_split[s])
  dfx<-shark_split[[s]]
  shark_stan$choice[s,1:nrow(dfx),1]<-dfx$choice1 -1 #Choice is 0 or 1
  shark_stan$choice[s,1:nrow(dfx),2]<-dfx$choice2 -1
  shark_stan$state[s,1:nrow(dfx)]<-as.numeric(dfx$Transition=="Rare")+1 #State is 1 or 2
  shark_stan$reward[s,1:nrow(dfx)]<-as.numeric(dfx$RewardType=="Reward") #Reward is 0 or 1
  shark_stan$sharkblock[s,1:nrow(dfx)]<-as.numeric(dfx$ifSharkBlock==TRUE) #SharkBlock is 0 or 1
  #Deal with missing trials;
  miss_c1<-which(is.na(dfx$choice1))
  miss_c2<-which(is.na(dfx$choice2))
  miss_any<-which(dfx$Missed)
  shark_stan$choice[s,miss_c1,1]=0
  shark_stan$choice[s,miss_c2,2]=0
  shark_stan$reward[s,miss_any]=0
  shark_stan$state[s,miss_c2]=1
  shark_stan$miss_choice[s,miss_c1,1]=1
  shark_stan$miss_choice[s,miss_c2,2]=1
  shark_stan$miss_reward[s,miss_any]=1
  
  dfx<-NULL
}
#Clean up dataset so it will run with vanessa's code first; 
shark_stan$ID<-NULL;shark_stan$sharkblock<-NULL

SH_daw_nolapse_l1=stan(file='daw_nolapse_lambda1.stan',
                   data=shark_stan,verbose=FALSE,save_warmup=FALSE,
                   pars=c('lp_','prev_choice','tran_count','tran_type','Q_TD','Q_MB','Q_2','delta_1','delta_2'),
                   include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.01))
print('running new model') 
launch_shinystan(SH_daw_nolapse_l1)