###simulation study

###build the environment: 

get_s_stage <- function(f_stage_choice,mu_b){
  if(!f_stage_choice %in% c(1,2)) {
    stop("illegal first stage choice")
  }
  
  ifRare= as.logical(sample(c(1,0),size = 1,replace = TRUE,prob = c(1-mu_b,mu_b)))
  if(f_stage_choice == 1 ){
    s_stage = ifelse(ifRare,3,2)
  } else {
    s_stage = ifelse(ifRare,2,3)
  }
}

prep_df<-function(df){
  df$s1_switch <- as.factor(df$s1_choice == lag(df$s1_choice))
  df$RareTrans <- as.factor( ifelse(df$s2_state - 2 == df$s1_choice,"Rare","Common"))
  df$reward <- as.factor(ifelse(as.logical(df$reward),"Reward","NoReward"))
  
  df$s1_switch_lead <- lead(df$s1_switch)
  
  return(df)
}



gen_reward_contingency <- function(ntrials,step_sd=2,upper=0.75,lower=0.25){
  
  sq <- rnorm(n = ntrials,mean = 0,sd = step_sd)
  sq[1] <- runif(1,min = min(sq),max = max(sq))
  sq <- scale(cumsum(sq))
  sq<-scales::rescale(sq,to=c(lower,upper))
  return(sq)
}

agent_daw<- function(ntrials,omega=0.5,betas=c(2,2),alphas=c(0.3,0.3),lamda=0.8,per_para=1.2,reward_contingency=NULL,index=""){
  
  #ntrials = 200
  
  
  #Parameters:
  # betas = c(4,4,4) #Inverse Temperature MF. MB. 2ndStage
  # alphas = c(0.4,0.4)     #Learning Rate
  # lamda = 0.5      #Cross stage eligibility trace
  # per_para = 0     #choice 'stickiness' parameters
  #omega = 0.5
  
  
  mu_b = 0.7
  
  
  Q_2<-array(NA,c(ntrials,2,2)) #(Trial,Choice,State)
  Q_MF <- matrix(NA,nrow = ntrials,ncol = 2)
  Q_MB =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_Net =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_MF[1,] = 0.5
  Q_2[1,,] = 0.5
  prev_choice = 0
  
  s1_choice = rep(NA,ntrials)
  s2_state = rep(NA,ntrials)
  s2_choice = rep(NA,ntrials)
  delta_1 = rep(NA,ntrials)
  delta_2 = rep(NA,ntrials)
  
     
  
  if(is.null(reward_contingency)){
    reward_contingency = array(data = NA,dim = c(2,2,ntrials))
    for(s in 1:2) {
      for (a in 1:2){
        reward_contingency[a,s,] <- gen_reward_contingency(ntrials,step_sd = 2,upper = 0.75,lower = 0.25)
      }
    }
  }
  
  reward = rep(NA,ntrials)
  

  
  for(t in 1:ntrials) {
    #message("####Trial ",t)
    Q_MB[t,2] =  ((mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (1-mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    Q_MB[t,1] =  ((1-mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    
    Q_Net[t,1] = (omega * Q_MB[t,1]) + ((1-omega) * Q_MF[t,1]);
    Q_Net[t,2] = (omega * Q_MB[t,2]) + ((1-omega) * Q_MF[t,2]);
    
    p_s1<-gtools::inv.logit(
      ((Q_Net[t,2]-Q_Net[t,1])*betas[1]) + (per_para*prev_choice)
    )

    s1_choice[t]<-prev_choice<-sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s1,p_s1))
    
    #message("Stage 1 Action: ", s1_choice[t]+1)
    s2_state[t] <- get_s_stage(f_stage_choice = s1_choice[t],mu_b=mu_b)
    #message("Stage 2 State: ", s2_state[t])
    #s2_state = rep(2,ntrials)
    p_s2 <-gtools::inv.logit(
      ( ( Q_2[t,2,(s2_state[t] - 1)] - Q_2[t,1,(s2_state[t] - 1)] ) * betas[2])
    )  
    s2_choice[t] <- sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s2,p_s2))
    #message("Stage 2 Action: ", s2_choice[t]+1)
    
    #message(t)
    r_p_i<-reward_contingency[ s2_choice[t], s2_state[t]-1, t ]
    reward[t] <- sample(c(0,1),size = 1,replace = TRUE,prob = c(1-r_p_i,r_p_i))
    #message("Reward: ",reward[t])
    
    delta_1[t] = Q_2[t,s2_choice[t],s2_state[t]-1] - Q_MF[t,s1_choice[t]]; 
    delta_2[t] = reward[t] - Q_2[t,s2_choice[t], (s2_state[t] - 1)];
    
    #Update the rest:
    if(t<ntrials){
      
      
      Q_MF[t+1,] = Q_MF[t,] * 1;
      Q_2[t+1,,] = Q_2[t,,] * 1;
      #Over write with the right value 
      
      
      Q_2[t+1,s2_choice[t],(s2_state[t] - 1)] = Q_2[t,s2_choice[t],(s2_state[t] - 1)] + (alphas[2] * delta_2[t]);
      Q_MF[t+1,s1_choice[t]] = Q_MF[t,s1_choice[t]] + (alphas[1]*delta_1[t]) + (alphas[1]*lamda*delta_2[t]);
      
      #message("##End####\n")
    }
  }
  Q_df<-as.data.frame(cbind(Q_MF,Q_MB,Q_Net))
  names(Q_df) <- c("Q_MF1","Q_MF2","Q_MB_1","Q_MB_2","Q_Net1","Q_Net2")
  df<-cbind(Q_df,data.frame(s1_choice,s2_state,s2_choice,reward,delta_1,delta_2,modelname="daw",ID=index,stringsAsFactors = F))
  df$trial <- 1:ntrials
  #df$probability<-sapply(1:ntrials, function(t){reward_contingency[df$s2_choice[t],(df$s2_state[t]-1),t]})
  
  out<-list(Q_MB=Q_MB,Q_MF=Q_MF,Q_2=Q_2,df=df,reward_contingency=reward_contingency,index=index)
  return(out)
}

agent_RewardSensitivity<- function(ntrials,omega=0.5,betas=c(2,2),alphas=c(0.3,0.3),lamda=0.8,per_para=1.2,RewardSensitivity=0.8,reward_contingency=NULL,index=""){
  
  #ntrials = 200
  
  
  #Parameters:
  # betas = c(4,4,4) #Inverse Temperature MF. MB. 2ndStage
  # alphas = c(0.4,0.4)     #Learning Rate
  # lamda = 0.5      #Cross stage eligibility trace
  # per_para = 0     #choice 'stickiness' parameters
  #omega = 0.5
  
  
  mu_b = 0.7
  
  
  Q_2<-array(NA,c(ntrials,2,2)) #(Trial,Choice,State)
  Q_MF <- matrix(NA,nrow = ntrials,ncol = 2)
  Q_MB =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_Net =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_MF[1,] = 0.5
  Q_2[1,,] = 0.5
  prev_choice = 0
  
  s1_choice = rep(NA,ntrials)
  s2_state = rep(NA,ntrials)
  s2_choice = rep(NA,ntrials)
  delta_1 = rep(NA,ntrials)
  delta_2 = rep(NA,ntrials)
  
  
  
  if(is.null(reward_contingency)){
    reward_contingency = array(data = NA,dim = c(2,2,ntrials))
    for(s in 1:2) {
      for (a in 1:2){
        reward_contingency[a,s,] <- gen_reward_contingency(ntrials,step_sd = 2,upper = 0.75,lower = 0.25)
      }
    }
  }
  
  reward = rep(NA,ntrials)
  
  
  
  for(t in 1:ntrials) {
    #message("####Trial ",t)
    Q_MB[t,2] =  ((mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (1-mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    Q_MB[t,1] =  ((1-mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    
    Q_Net[t,1] = (omega * Q_MB[t,1]) + ((1-omega) * Q_MF[t,1]);
    Q_Net[t,2] = (omega * Q_MB[t,2]) + ((1-omega) * Q_MF[t,2]);
    
    p_s1<-gtools::inv.logit(
      ((Q_Net[t,2]-Q_Net[t,1])*betas[1]) + (per_para*prev_choice)
    )
    
    s1_choice[t]<-prev_choice<-sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s1,p_s1))
    
    #message("Stage 1 Action: ", s1_choice[t]+1)
    s2_state[t] <- get_s_stage(f_stage_choice = s1_choice[t],mu_b=mu_b)
    #message("Stage 2 State: ", s2_state[t])
    #s2_state = rep(2,ntrials)
    p_s2 <-gtools::inv.logit(
      ( ( Q_2[t,2,(s2_state[t] - 1)] - Q_2[t,1,(s2_state[t] - 1)] ) * betas[2])
    )  
    s2_choice[t] <- sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s2,p_s2))
    #message("Stage 2 Action: ", s2_choice[t]+1)
    
    #message(t)
    r_p_i<-reward_contingency[ s2_choice[t], s2_state[t]-1, t ]
    reward[t] <- sample(c(0,1),size = 1,replace = TRUE,prob = c(1-r_p_i,r_p_i))
    #message("Reward: ",reward[t])
    
    delta_1[t] = Q_2[t,s2_choice[t],s2_state[t]-1] - Q_MF[t,s1_choice[t]]; 
    delta_2[t] = RewardSensitivity * reward[t] - Q_2[t,s2_choice[t], (s2_state[t] - 1)];
    
    #Update the rest:
    if(t<ntrials){
      
      
      Q_MF[t+1,] = Q_MF[t,] * 1;
      Q_2[t+1,,] = Q_2[t,,] * 1;
      #Over write with the right value 
      
      
      Q_2[t+1,s2_choice[t],(s2_state[t] - 1)] = Q_2[t,s2_choice[t],(s2_state[t] - 1)] + (alphas[2] * delta_2[t]);
      Q_MF[t+1,s1_choice[t]] = Q_MF[t,s1_choice[t]] + (alphas[1]*delta_1[t]) + (alphas[1]*lamda*delta_2[t]);
      
      #message("##End####\n")
    }
  }
  Q_df<-as.data.frame(cbind(Q_MF,Q_MB,Q_Net))
  names(Q_df) <- c("Q_MF1","Q_MF2","Q_MB_1","Q_MB_2","Q_Net1","Q_Net2")
  df<-cbind(Q_df,data.frame(s1_choice,s2_state,s2_choice,reward,delta_1,delta_2,modelname="daw",ID=index,stringsAsFactors = F))
  df$trial <- 1:ntrials
  #df$probability<-sapply(1:ntrials, function(t){reward_contingency[df$s2_choice[t],(df$s2_state[t]-1),t]})
  
  out<-list(Q_MB=Q_MB,Q_MF=Q_MF,Q_2=Q_2,df=df,reward_contingency=reward_contingency,index=index)
  return(out)
}

agent_Affect <- function(ntrials,omega=0.5,betas=c(2,2),alphas=c(0.4,0.4,0.4),af_bias=0.3,lamda=0.8,per_para=1.2,RewardSensitivity=0.8,reward_contingency=NULL,index=""){
  
  #ntrials = 200
  
  
  #Default Parameters: saved here for debuging 
  # betas = c(4,4,4) #Inverse Temperature MF. MB. 2ndStage
  # alphas = c(0.4,0.4,0.4)     #Learning Rate
  # lamda = 0.5      #Cross stage eligibility trace
  # per_para = 0     #choice 'stickiness' parameters
  # RewardSensitivity = 0.8
  # af_bias=-1
  
  
  mu_b = 0.7
  
  
  Q_2 = array(NA,c(ntrials,2,2)) #(Trial,Choice,State)
  Q_MF = matrix(NA,nrow = ntrials,ncol = 2)
  Q_MB =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_Net =  matrix(NA,nrow = ntrials,ncol = 2)
  Q_af = rep(NA,ntrials)
  Q_MF[1,] = 0.5
  Q_2[1,,] = 0.5
  Q_af[1] = 0.5
  prev_choice = 0
  
  s1_choice = rep(NA,ntrials)
  s2_state = rep(NA,ntrials)
  s2_choice = rep(NA,ntrials)
  delta_1 = rep(NA,ntrials)
  delta_2 = rep(NA,ntrials)
  w_ts = rep(NA,ntrials)

  delta_af = rep(NA,ntrials)
  
  if(is.null(reward_contingency)){
    reward_contingency = array(data = NA,dim = c(2,2,ntrials))
    for(s in 1:2) {
      for (a in 1:2){
        reward_contingency[a,s,] <- gen_reward_contingency(ntrials,step_sd = 2,upper = 0.75,lower = 0.25)
      }
    }
  }
  
  reward = rep(NA,ntrials)
  
  for(t in 1:ntrials) {
    #message("####Trial ",t)
    Q_MB[t,2] =  ((mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (1-mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    Q_MB[t,1] =  ((1-mu_b)*max(Q_2[t,1,2],Q_2[t,2,2]) + (mu_b)*max(Q_2[t,1,1],Q_2[t,2,1]));
    
    w_ts[t] = gtools::inv.logit(Q_af[t]+af_bias);
    
    Q_Net[t,1] = (w_ts[t] * Q_MB[t,1]) + ((1-w_ts[t]) * Q_MF[t,1]);
    Q_Net[t,2] = (w_ts[t] * Q_MB[t,2]) + ((1-w_ts[t]) * Q_MF[t,2]);
    
    p_s1<-gtools::inv.logit(
      ((Q_Net[t,2]-Q_Net[t,1])*betas[1]) + (per_para*prev_choice)
    )
    
    s1_choice[t]<-prev_choice<-sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s1,p_s1))
    
    #message("Stage 1 Action: ", s1_choice[t]+1)
    s2_state[t] <- get_s_stage(f_stage_choice = s1_choice[t],mu_b=mu_b)
    #message("Stage 2 State: ", s2_state[t])
    #s2_state = rep(2,ntrials)
    p_s2 <-gtools::inv.logit(
      ( ( Q_2[t,2,(s2_state[t] - 1)] - Q_2[t,1,(s2_state[t] - 1)] ) * betas[2])
    )  
    s2_choice[t] <- sample(c(1,2),size = 1,replace = TRUE,prob = c(1-p_s2,p_s2))
    #message("Stage 2 Action: ", s2_choice[t]+1)
    
    #message(t)
    r_p_i<-reward_contingency[ s2_choice[t], s2_state[t]-1, t ]
    reward[t] <- sample(c(0,1),size = 1,replace = TRUE,prob = c(1-r_p_i,r_p_i))
    #message("Reward: ",reward[t])
    
    delta_1[t] = Q_2[t,s2_choice[t],s2_state[t]-1] - Q_MF[t,s1_choice[t]]; 
    delta_2[t] = (RewardSensitivity * reward[t]) - Q_2[t,s2_choice[t], (s2_state[t] - 1)];
    delta_af[t] = delta_2[t] - Q_af[t]
    #Update the rest:
    if(t<ntrials){
      
      
      Q_MF[t+1,] = Q_MF[t,] * 1;
      Q_2[t+1,,] = Q_2[t,,] * 1;
      Q_af[t+1] = Q_af[t] * 1;
      #Over write with the right value 
      
      
      Q_2[t+1,s2_choice[t],(s2_state[t] - 1)] = Q_2[t,s2_choice[t],(s2_state[t] - 1)] + (alphas[2] * delta_2[t]);
      Q_MF[t+1,s1_choice[t]] = Q_MF[t,s1_choice[t]] + (alphas[1]*delta_1[t]) + (alphas[1]*lamda*delta_2[t]);
      Q_af[t+1] = Q_af[t] + (alphas[3] * delta_af[t]);
      #message("##End####\n")
    }
  }
  Q_df<-as.data.frame(cbind(Q_MF,Q_MB,Q_Net,Q_af))
  names(Q_df) <- c("Q_MF1","Q_MF2","Q_MB_1","Q_MB_2","Q_Net1","Q_Net2","Q_af")
  df<-cbind(Q_df,data.frame(s1_choice,s2_state,s2_choice,reward,delta_1,delta_2,modelname="daw",ID=index,stringsAsFactors = F))
  df$trial <- 1:ntrials
  #df$probability<-sapply(1:ntrials, function(t){reward_contingency[df$s2_choice[t],(df$s2_state[t]-1),t]})
  
  out<-list(Q_MB=Q_MB,Q_MF=Q_MF,Q_2=Q_2,df=df,reward_contingency=reward_contingency,index=index)
  return(out)
}
###
stop("functions done")

reward_contingency<-R.matlab::readMat("masterprob3.mat")$payoff #(Action, State, Time)

ntrials = 400
reward_contingency = array(data = NA,dim = c(2,2,ntrials))
for(s in 1:2) {
  for (a in 1:2){
    reward_contingency[a,s,] <- gen_reward_contingency(ntrials,step_sd = 2,upper = 0.75,lower = 0.25)
  }
}

#rip, use too much memeory, kills connections all the time ;_;
# prax<-parallel::makeForkCluster(nnodes = 2,outfile="")
# 
# search_omega<-parallel::parLapply(cl = prax,X = c(0,1:10/10),fun = function(omegaX){
#   out<-lapply(1:1000,agent_daw,ntrials=200,omega=omegaX,betas=c(4,4),alphas=c(0.4,0.4),lamda=0.5,per_para=0,reward_contingency=reward_contingency)
#   sum_of_rewards<-sapply(out,function(xs){sum(xs$df$reward)})
#   all_df <- do.call(rbind,lapply(out,`[[`,"df"))
#   all_df$omega <- omegaX
#   all_df$s1_switch <- as.factor(getifswitched(all_df$s1_choice))
#   all_df$RareTrans <- as.factor(all_df$s2_state - 2 == all_df$s1_choice)
#   all_df$reward <- as.factor(as.logical(all_df$reward))
#   regression_model<-lme4::glmer(s1_switch ~ RareTrans * reward   +  (1 | ID),family = binomial(),data = all_df)
#   grx<-ggeffects::ggpredict(regression_model,terms = c("RareTrans","reward"),x.as.factor = T,type="fe")
#   p1<-ggplot(grx,aes(x,predicted))+geom_boxplot(aes(
#     lower = predicted - std.error, 
#     upper = predicted + std.error, 
#     middle = predicted, 
#     ymin = conf.low, 
#     ymax = conf.high,fill=group),stat = "identity",position = "dodge") 
#   ga<-unique(coef(regression_model)$ID$`RareTransTRUE:rewardTRUE`)
#   return(list(out=out,sum_of_rewards=sum_of_rewards,all_df=all_df,regression_model=regression_model,p1=p1,
#               reg_coef = ga)
#          )
# })
# parallel::stopCluster(prax)
out<-lapply(1:1000,agent_daw,ntrials=200,omega=0,betas=c(4,4),alphas=c(0.4,0.4),lamda=0.5,per_para=0,reward_contingency=reward_contingency)


search_omega<-lapply(c(0,1:10/10),function(omegaX){
  out<-lapply(1:1000,agent_daw,ntrials=200,omega=omegaX,betas=c(4,4),alphas=c(0.4,0.4),lamda=0.5,per_para=0,reward_contingency=reward_contingency)
  sum_of_rewards<-sapply(out,function(xs){sum(xs$df$reward)})
  all_df <- do.call(rbind,lapply(out,`[[`,"df"))
  all_df$omega <- omegaX
  
  regression_model<-glm(s1_switch_lead ~ RareTrans * reward,family = binomial(),data = prep_df(all_df))
  summary(regression_model)
  
  grx<-ggeffects::ggpredict(regression_model,terms = c("reward","RareTrans"),x.as.factor = T,type="fe")
  p1<-ggplot(grx,aes(x,predicted))+geom_boxplot(aes(
    lower = predicted - std.error, 
    upper = predicted + std.error, 
    middle = predicted, 
    ymin = conf.low, 
    ymax = conf.high,fill=group),stat = "identity",position = "dodge") 
  ga<-last(as.data.frame(coef(regression_model))[[1]])
  return(list(out=out,sum_of_rewards=sum_of_rewards,all_df=all_df,regression_model=regression_model,p1=p1,reg_coef=ga)
  )
})

omegas<-c(0,1:10/10)
coefxInter<-sapply(search_omega,`[[`,"reg_coef")
cor.test(omegas,coefxInter)
sum_reward<-lapply(search_omega,`[[`,"sum_of_rewards")
sum_rw_df<-do.call(rbind,lapply(sum_reward,function(rx){data.frame(`mean reward percentage`=mean(rx/200),upper=max(rx/200),lower=min(rx/200),sd=sd(rx/200),stringsAsFactors = F)}))
sum_rw_df$omega <- omegas

ggplot(sum_rw_df,aes(omega, mean.reward.percentage))+geom_point() + geom_line()+ylim(0,1)+ylab("reward percentage") + xlab("weighting parameter")

search_omega_sensitive <- lapply(c(0,1:10/10),function(RSX){
  search_omega<-lapply(c(0,1:10/10),function(omegaX){
    out<-lapply(1:1000,agent_RewardSensitivity,RewardSensitivity=RSX,ntrials=200,omega=omegaX,betas=c(4,4),alphas=c(0.4,0.4),lamda=0.5,per_para=0,reward_contingency=reward_contingency)
    sum_of_rewards<-sapply(out,function(xs){sum(xs$df$reward)})
    # all_df <- do.call(rbind,lapply(out,`[[`,"df"))
    # all_df$omega <- omegaX
    # 
    # regression_model<-glm(s1_switch_lead ~ RareTrans * reward,family = binomial(),data = prep_df(all_df))
    # summary(regression_model)
    # 
    # grx<-ggeffects::ggpredict(regression_model,terms = c("reward","RareTrans"),x.as.factor = T,type="fe")
    # p1<-ggplot(grx,aes(x,predicted))+geom_boxplot(aes(
    #   lower = predicted - std.error, 
    #   upper = predicted + std.error, 
    #   middle = predicted, 
    #   ymin = conf.low, 
    #   ymax = conf.high,fill=group),stat = "identity",position = "dodge") 
    # ga<-last(as.data.frame(coef(regression_model))[[1]])
    return(list(out=out,sum_of_rewards=sum_of_rewards)
    )
  })
  omegas<-c(0,1:10/10)
  #coefxInter<-sapply(search_omega,`[[`,"reg_coef")
  #cor.test(omegas,coefxInter)
  sum_reward<-lapply(search_omega,`[[`,"sum_of_rewards")
  sum_rw_df<-do.call(rbind,lapply(sum_reward,function(rx){data.frame(`mean reward percentage`=mean(rx/200),upper=max(rx/200),lower=min(rx/200),sd=sd(rx/200),stringsAsFactors = F)}))
  sum_rw_df$omega <- omegas
  sum_rw_df$RS <- RSX
  return(sum_rw_df)
})

all_s_o_s<-do.call(rbind,search_omega_sensitive)
save(all_s_o_s,file = "all_s_o_s.rdata") #Save it just in case... It took FOREVER to run. 
ggplot(all_s_o_s,aes())+geom_point()

plot_ly(x=all_s_o_s$omega,y=all_s_o_s$RS,z=all_s_o_s$mean.reward.percentage,type = 'scatter3d',color = all_s_o_s$mean.reward.percentage) 



###Model Affect:
out<-lapply(1:1000,agent_Affect,ntrials=400,betas=c(4,4),alphas=c(0.4,0.4,0.8),af_bias= -10,RewardSensitivity=1,lamda=0.5,per_para=0,reward_contingency=reward_contingency)
all_df <- do.call(rbind,lapply(out,`[[`,"df"))

regression_model<-glm(s1_switch_lead ~ RareTrans * reward,family = binomial(),data = prep_df(all_df))
summary(regression_model)
grx<-ggeffects::ggpredict(regression_model,terms = c("reward","RareTrans"),x.as.factor = T,type="fe")
ggplot(grx,aes(x,predicted))+geom_boxplot(aes(
  lower = predicted - std.error, 
  upper = predicted + std.error, 
  middle = predicted, 
  ymin = conf.low, 
  ymax = conf.high,fill=group),stat = "identity",position = "dodge") 


ggplot(grx,aes(x,predicted))+geom_boxplot(aes(
  lower = predicted - std.error, 
  upper = predicted + std.error, 
  middle = predicted, 
  ymin = conf.low, 
  ymax = conf.high,fill=group),stat = "identity",position = "dodge") 



allgrapsh<-lapply(c(1,6,11),function(x){
  p<-search_omega[[x]]$p1+ylab("Predicted Probability of Staying") + xlab(paste("weight=",omegas[x]))+scale_fill_discrete(name = "Transisiton Type")
  if(x!=11){
    p<-p+ theme(legend.position = "none")}
  if(x!=1){
    p<-p+theme(axis.title.y = element_blank())}
  return(p)
  })
allgrapsh$nrow <- 1
allgrapsh$layout_matrix <- matrix(c(1,1,1,2,2,2,3,3,3,3),nrow = 1)
do.call(gridExtra::grid.arrange,allgrapsh)
