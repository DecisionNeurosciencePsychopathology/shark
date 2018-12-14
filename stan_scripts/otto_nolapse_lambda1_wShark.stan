data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
  int<lower=0,upper=1> shark [nS,nT];
  int missing_choice[nS,nT,2];
  int missing_reward[nS,nT];
}

parameters {
  real alpha_m;
  real<lower=0> alpha_s;
  real<lower=0> beta_1_MF_m;
  real<lower=0> beta_1_MF_s;
  real<lower=0> beta_1_MB_m;
  real<lower=0> beta_1_MB_s;
  real<lower=0> beta_2_m;
  real<lower=0> beta_2_s;
  
    
  real alpha_m_shark;
  real<lower=0> beta_1_MF_m_shark;
  real<lower=0> beta_1_MB_m_shark;
  real<lower=0> beta_2_m_shark;
  
  
  //real lambda_m;
  //real<lower=0> lambda_s;
  real pers_m;
  real<lower=0> pers_s;
  vector[nS] alpha_raw;
  vector[nS] beta_1_MF_raw;
  vector[nS] beta_1_MB_raw;
  vector[nS] beta_2_raw;
  //vector[nS] lambda_raw;
  vector[nS] pers_raw;
}

transformed parameters {
  //define transformed parameters
  vector<lower=0,upper=1>[nS] alpha;
  vector[nS] alpha_normal;
  vector[nS] beta_1_MF;
  vector[nS] beta_1_MB;
  vector[nS] beta_2;
  
  vector<lower=0,upper=1>[nS] alpha_shark;
  vector[nS] alpha_normal_shark;
  vector[nS] beta_1_MF_shark;
  vector[nS] beta_1_MB_shark;
  vector[nS] beta_2_shark;
  //vector<lower=0,upper=1>[nS] lambda;
  //vector[nS] lambda_normal;
  vector[nS] pers;

  //create transformed parameters using non-centered parameterization for all
  // and logistic transformation for alpha & lambda (range: 0 to 1)
  alpha_normal = alpha_m + (alpha_s*alpha_raw);
  alpha = inv_logit(alpha_normal);
  beta_1_MF = beta_1_MF_m + beta_1_MF_s*beta_1_MF_raw;
  beta_1_MB = beta_1_MB_m + beta_1_MB_s*beta_1_MB_raw;
  beta_2 = beta_2_m + beta_2_s*beta_2_raw;
  
  alpha_normal_shark = alpha_m_shark + (alpha_s*alpha_raw);
  alpha_shark = inv_logit(alpha_normal_shark);
  beta_1_MF_shark = beta_1_MF_m_shark + beta_1_MF_s*beta_1_MF_raw;
  beta_1_MB_shark = beta_1_MB_m_shark + beta_1_MB_s*beta_1_MB_raw;
  beta_2_shark = beta_2_m_shark + beta_2_s*beta_2_raw;
  //lambda_normal = lambda_m + lambda_s*lambda_raw;
  //lambda = inv_logit(lambda_normal);
  pers = pers_m + pers_s*pers_raw;
}

model {
  //define variables
  int prev_choice;
  int tran_count;
  int tran_type[2];
  int unc_state;
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_2[2,2];
  real alpha_use;
  real beta_1_mb_use;
  real beta_1_mf_use;
  real beta_2_use;
  
  //define priors
  alpha_m ~ normal(0,2.5);
  beta_1_MF_m ~ normal(0,5);
  beta_1_MB_m ~ normal(0,5);
  beta_2_m ~ normal(0,5);
  //lambda_m ~ normal(0,2);
  alpha_m_shark ~ normal(0,2.5);
  beta_1_MF_m_shark ~ normal(0,5);
  beta_1_MB_m_shark ~ normal(0,5);
  beta_2_m_shark ~ normal(0,5);
  
  pers_m ~ normal(0,2.5);
  
  alpha_s ~ cauchy(0,1);
  beta_1_MF_s ~ cauchy(0,2);
  beta_1_MB_s ~ cauchy(0,2);
  beta_2_s ~ cauchy(0,2);
  //lambda_s ~ cauchy(0,1);
  pers_s ~ cauchy(0,1);
  alpha_raw ~ normal(0,1);
  beta_1_MF_raw ~ normal(0,1);
  beta_1_MB_raw ~ normal(0,1);
  beta_2_raw ~ normal(0,1);
  //lambda_raw ~ normal(0,1);
  pers_raw ~ normal(0,1);
for (s in 1:nS) {
  
  //set initial values
  for (i in 1:2) {
    Q_TD[i]=.5;
    Q_MB[i]=.5;
    Q_2[1,i]=.5;
    Q_2[2,i]=.5;
    tran_type[i]=0;
  }
  prev_choice=0;
  
    for (t in 1:nT) {
      //Setup parameters to use:
      if(shark[s,t]==0){
        alpha_use = alpha[s];
        beta_1_mb_use = beta_1_MB[s];
        beta_1_mf_use = beta_1_MF[s];
        beta_2_use=beta_2[s];
      } else {
        alpha_use = alpha_shark[s];
        beta_1_mb_use = beta_1_MB_shark[s];
        beta_1_mf_use = beta_1_MF_shark[s];
        beta_2_use = beta_2_shark[s];
      }
      
      //use if not missing 1st stage choice
      if (missing_choice[s,t,1]==0) {
        choice[s,t,1] ~ bernoulli_logit((beta_1_mf_use*(Q_TD[2]-Q_TD[1]))+(beta_1_mb_use*(Q_MB[2]-Q_MB[1]))+pers[s]*prev_choice);
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
        
        //use if not missing 2nd stage choice
        if (missing_choice[s,t,2]==0) {
          choice[s,t,2] ~ bernoulli_logit(beta_2_use*(Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1]));
          
          //use if not missing 2nd stage reward
          if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]/alpha_use-Q_TD[choice[s,t,1]+1]; 
             delta_2 = reward[s,t]/alpha_use - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_use*(delta_1+lambda[s]*delta_2);
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_use*(delta_1+delta_2);
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha_use*delta_2;
            
             
             //update unchosen TD & second stage values
             Q_TD[(choice[s,t,1] ? 2 : 1)] = (1-alpha_use)*Q_TD[(choice[s,t,1] ? 2 : 1)];
             Q_2[state_2[s,t],(choice[s,t,2] ? 2 : 1)] = (1-alpha_use)*Q_2[state_2[s,t],(choice[s,t,2] ? 2 : 1)];
             unc_state = (state_2[s,t]-1) ? 2 : 1;
             Q_2[unc_state,1] = (1-alpha_use)*Q_2[unc_state,1];
             Q_2[unc_state,2] = (1-alpha_use)*Q_2[unc_state,2];
             
             //Update MB Value;
              Q_MB[1] = (tran_type[1] > tran_type[2]) ? (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2])) : (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2]));
             Q_MB[2] = (tran_type[1] > tran_type[2]) ? (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2])) : (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2]));
            
          } //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1||missing_reward[s,t]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_use*delta_1;
          Q_TD[(choice[s,t,1] ? 2 : 1)] = (1-alpha_use)*Q_TD[(choice[s,t,1] ? 2 : 1)];
          Q_2[1,1] = (1-alpha_use)*Q_2[1,1];
          Q_2[1,2] = (1-alpha_use)*Q_2[1,2];
          Q_2[2,1] = (1-alpha_use)*Q_2[2,1];
          Q_2[2,2] = (1-alpha_use)*Q_2[2,2];
          //MB update of first stage values based on second stage values, so don't change

        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values & update previous choice
      prev_choice=0;
      Q_TD[1] = (1-alpha_use)*Q_TD[1];
      Q_TD[2] = (1-alpha_use)*Q_TD[2];
      Q_2[1,1] = (1-alpha_use)*Q_2[1,1];
      Q_2[1,2] = (1-alpha_use)*Q_2[1,2];
      Q_2[2,1] = (1-alpha_use)*Q_2[2,1];
      Q_2[2,2] = (1-alpha_use)*Q_2[2,2];
      }
    }
  }
}

generated quantities {
  //same code as above, with following changes: 
  // 1) values and choices used to calculate probability, rather than fitting values to choices
  // 2) no priors, etc.- uses estimated pararamter values from model block
  real log_lik[nS,nT,2]; //log likelihood- must be named this
  int prev_choice;
  int tran_count;
  int tran_type[2];
  int unc_state;
  real delta_1[nS,nT];
  real delta_2[nS,nT];
  real Q_TD[nS,nT+1,2];
  real Q_MB[nS,nT+1,2];
  real Q_2[nS,nT+1,2,2];
  real alpha_use;
  real beta_1_mb_use;
  real beta_1_mf_use;
  real beta_2_use;
  
  for (s in 1:nS) {
    for (i in 1:2) {
      Q_TD[s,1,i]=.5;
      Q_MB[s,1,i]=.5;
      Q_2[s,1,1,i]=.5;
      Q_2[s,1,2,i]=.5;
      tran_type[i]=0;
    }
    prev_choice=0;
    for (t in 1:nT) {
       if(shark[s,t]==0){
        alpha_use=alpha[s];
        beta_1_mb_use=beta_1_MB[s];
        beta_1_mf_use=beta_1_MF[s];
        beta_2_use=beta_2[s];
      } else {
        alpha_use=alpha_shark[s];
        beta_1_mb_use=beta_1_MB_shark[s];
        beta_1_mf_use=beta_1_MF_shark[s];
        beta_2_use=beta_2_shark[s];
      }
      if (missing_choice[s,t,1]==0) {
        log_lik[s,t,1] = bernoulli_logit_lpmf(choice[s,t,1] | beta_1_mf_use*(Q_TD[s,t,2]-Q_TD[s,t,1])+beta_1_mb_use*(Q_MB[s,t,2]-Q_MB[s,t,1])+pers[s]*prev_choice);
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
        
        if (missing_choice[s,t,2]==0) {
          log_lik[s,t,2] = bernoulli_logit_lpmf(choice[s,t,2] | beta_2_use*(Q_2[s,t,state_2[s,t],2]-Q_2[s,t,state_2[s,t],1]));
          
          //use if not missing 2nd stage reward
          if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1[s,t] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1]/alpha_use-Q_TD[s,t,choice[s,t,1]+1]; 
             delta_2[s,t] = reward[s,t]/alpha_use - Q_2[s,t,state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_use*(delta_1+lambda[s]*delta_2);
             Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + alpha_use*(delta_1[s,t]+delta_2[s,t]);
             Q_2[s,t+1,state_2[s,t],choice[s,t,2]+1] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1] + alpha_use*delta_2[s,t];
             
             //update unchosen TD & second stage values
             Q_TD[s,t+1,(choice[s,t,1] ? 2 : 1)] = (1-alpha_use)*Q_TD[s,t,(choice[s,t,1] ? 2 : 1)];
             Q_2[s,t+1,state_2[s,t],(choice[s,t,2] ? 2 : 1)] = (1-alpha_use)*Q_2[s,t,state_2[s,t],(choice[s,t,2] ? 2 : 1)];
             unc_state = (state_2[s,t]-1) ? 2 : 1;
             Q_2[s,t+1,unc_state,1] = (1-alpha_use)*Q_2[s,t,unc_state,1];
             Q_2[s,t+1,unc_state,2] = (1-alpha_use)*Q_2[s,t,unc_state,2];
             
             //Update MB Q
              Q_MB[s,t+1,1] = (tran_type[1] > tran_type[2]) ? (.7*fmax(Q_2[s,t+1,1,1],Q_2[s,t+1,1,2]) + .3*fmax(Q_2[s,t+1,2,1],Q_2[s,t+1,2,2])) : (.3*fmax(Q_2[s,t+1,1,1],Q_2[s,t+1,1,2]) + .7*fmax(Q_2[s,t+1,2,1],Q_2[s,t+1,2,2]));
             Q_MB[s,t+1,2] = (tran_type[1] > tran_type[2]) ? (.3*fmax(Q_2[s,t+1,1,1],Q_2[s,t+1,1,2]) + .7*fmax(Q_2[s,t+1,2,1],Q_2[s,t+1,2,2])) : (.7*fmax(Q_2[s,t+1,1,1],Q_2[s,t+1,1,2]) + .3*fmax(Q_2[s,t+1,2,1],Q_2[s,t+1,2,2]));
            
          } //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1||missing_reward[s,t]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
        log_lik[s,t,2] = 0;
          delta_1[s,t] = Q_2[s,t+1,state_2[s,t],choice[s,t,2]+1]-Q_TD[s,t,choice[s,t,1]+1]; 
          Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + alpha_use*delta_1[s,t];
          Q_TD[s,t+1,(choice[s,t,1] ? 2 : 1)] = (1-alpha_use)*Q_TD[s,t,(choice[s,t,1] ? 2 : 1)];
          Q_2[s,t+1,1,1] = (1-alpha_use)*Q_2[s,t,1,1];
          Q_2[s,t+1,1,2] = (1-alpha_use)*Q_2[s,t,1,2];
          Q_2[s,t+1,2,1] = (1-alpha_use)*Q_2[s,t,2,1];
          Q_2[s,t+1,2,2] = (1-alpha_use)*Q_2[s,t,2,2];
          //MB update of first stage values based on second stage values, so don't change

        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values
      log_lik[s,t,1] = 0;
      log_lik[s,t,2] = 0;
      Q_TD[s,t+1,1] = (1-alpha_use)*Q_TD[s,t,1];
      Q_TD[s,t+1,2] = (1-alpha_use)*Q_TD[s,t,2];
      Q_2[s,t+1,1,1] = (1-alpha_use)*Q_2[s,t,1,1];
      Q_2[s,t+1,1,2] = (1-alpha_use)*Q_2[s,t,1,2];
      Q_2[s,t+1,2,1] = (1-alpha_use)*Q_2[s,t,2,1];
      Q_2[s,t+1,2,2] = (1-alpha_use)*Q_2[s,t,2,2];
      }
    } 
  }
}







