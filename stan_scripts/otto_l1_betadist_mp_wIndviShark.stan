data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  int<lower=-1,upper=1> motorchoice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
  int missing_choice[nS,nT,2];
  vector[nT] shark[nS];
  //int missing_reward[nS,nT];
 // int<lower=0,upper=1> factorizedecay;
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
  //real lambda_m;
  //real<lower=0> lambda_s;
  
  real alpha_shark_gm;
  real beta_1_MF_shark_gm;
  real beta_1_MB_shark_gm;
  real beta_2_shark_gm;
  
  real alpha_shark_gs;
  real beta_1_MF_shark_gs;
  real beta_1_MB_shark_gs;
  real beta_2_shark_gs;
  
  real pers_m;
  real<lower=0> pers_s;
  real Mo_pers_m;
  real<lower=0> Mo_pers_s;
  //DO NOT USE THIS HORRIBLE IDEA
  //real omega_m;
  //real<lower=0> omega_s;
  
  vector[nS] alpha_raw;
  vector[nS] beta_1_MF_raw;
  vector[nS] beta_1_MB_raw;
  vector[nS] beta_2_raw;
  //vector[nS] lambda_raw;
  vector[nS] pers_raw;
  vector[nS] Mo_pers_raw;
  //vector[nS] omega_raw;
  vector[nS] alpha_shark_raw;
  vector[nS] beta_1_MF_shark_raw;
  vector[nS] beta_1_MB_shark_raw;
  vector[nS] beta_2_shark_raw;
 
  
}

transformed parameters {

  //define transformed parameters
  vector<lower=0,upper=1>[nT] alpha[nS] ;
  vector[nT] beta_1_MF[nS];
  vector[nT] beta_1_MB[nS];
  vector[nT] beta_2[nS];
  
  vector[nT] alpha_normal[nS]; 
  vector[nT] beta_1_MF_normal[nS];
  vector[nT] beta_1_MB_normal[nS];
  vector[nT] beta_2_normal[nS];
  
  vector[nS] pers;
  vector[nS] Mo_pers;
  
  //create transformed parameters using non-centered parameterization for all
  // and logistic transformation for alpha & lambda (range: 0 to 1)
  vector[nS] alpha_m_shark;
  vector[nS] beta_1_MF_m_shark;
  vector[nS] beta_1_MB_m_shark;
  vector[nS] beta_2_m_shark;
  
  alpha_m_shark = alpha_shark_gm + alpha_shark_gs * alpha_shark_raw; 
  beta_1_MF_m_shark = beta_1_MF_shark_gm + beta_1_MF_shark_gs * beta_1_MF_shark_raw;  
  beta_1_MB_m_shark = beta_1_MB_shark_gm + beta_1_MB_shark_gs * beta_1_MB_shark_raw;  
  beta_2_m_shark = beta_2_shark_gm + beta_2_shark_gs * beta_2_shark_raw;  
  
  
  for (S in 1:nS) {  
    for (T in 1:nT) {
      alpha_normal[S,T] = alpha_m + (alpha_s*alpha_raw[S]) + (alpha_m_shark[S]*shark[S,T]);
      beta_1_MF_normal[S,T] = beta_1_MF_m + (beta_1_MF_s*beta_1_MF_raw[S]) + (beta_1_MF_m_shark[S]*shark[S,T]) ;
      beta_1_MB_normal[S,T] = beta_1_MB_m + (beta_1_MB_s*beta_1_MB_raw[S]) + (beta_1_MB_m_shark[S]*shark[S,T]);
      beta_2_normal[S,T] = beta_2_m + (beta_2_s*beta_2_raw[S]) + (beta_2_m_shark[S]*shark[S,T]);
    }
  }
 
  pers = pers_m + pers_s*pers_raw;
  Mo_pers = Mo_pers_m + Mo_pers_s * Mo_pers_raw;
 // omega = omega_m + (omega_s*omega_raw);
  
  
  alpha = inv_logit(alpha_normal);
  beta_1_MF = beta_1_MF_normal;
  beta_1_MB = beta_1_MB_normal;
  beta_2 = beta_2_normal;
  
}

model {
  //define variables
  int prev_choice;
  int prev_motor;
  int tran_count;
  int tran_type[2];
  int unc_state;
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_2[2,2];
  real alpha_b; real beta_b;real mu_b;
 
  
  //define priors
  alpha_m ~ normal(0,2.5);
  beta_1_MF_m ~ normal(0,5);
  beta_1_MB_m ~ normal(0,5);
  beta_2_m ~ normal(0,5);
  //lambda_m ~ normal(0,2);
  pers_m ~ normal(0,2.5);
  Mo_pers_m ~ normal(0,2.5);
  
  alpha_s ~ cauchy(0,1);
  beta_1_MF_s ~ cauchy(0,1);
  beta_1_MB_s ~ cauchy(0,1);
  beta_2_s ~ cauchy(0,1);
  //lambda_s ~ cauchy(0,1);
  pers_s ~ cauchy(0,1);
  Mo_pers_s ~ cauchy(0,1);
  
  alpha_raw ~ normal(0,1);
  beta_1_MF_raw ~ normal(0,1);
  beta_1_MB_raw ~ normal(0,1);
  beta_2_raw ~ normal(0,1);
  //lambda_raw ~ normal(0,1);
  pers_raw ~ normal(0,1);
  Mo_pers_raw ~ normal(0,1);
  
  for (s in 1:nS) {
  
  //set initial values
  for (i in 1:2) {
    Q_TD[i]=.5;
    Q_MB[i]=.5;
    Q_2[1,i]=.5;
    Q_2[2,i]=.5;
    tran_type[i]=0;
  }
  prev_motor=0;
  prev_choice=0;
  alpha_b=1;
  beta_b=1;
  
    for (t in 1:nT) {
      //use if not missing 1st stage choice
      if (missing_choice[s,t,1]==0) {
        //pirint(beta_1_MF[s,t]);
        choice[s,t,1] ~ bernoulli_logit( 
          
          ((Q_TD[2]-Q_TD[1])*beta_1_MF[s,t])+((Q_MB[2]-Q_MB[1])*beta_1_MB[s,t])+(pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
          
          );
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
        prev_motor = 2*motorchoice[s,t,1]-1; //1 if motorchoice 1, 1  if motor choice 2, -1 
        //use if not missing 2nd stage choice
        if (missing_choice[s,t,2]==0) {
          choice[s,t,2] ~ bernoulli_logit( ((Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])*beta_2[s,t]) );
          
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]/alpha[s,t]-Q_TD[choice[s,t,1]+1]; 
             delta_2 = reward[s,t]/alpha[s,t] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s]*(delta_1+lambda[s]*delta_2);
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,t]*(delta_1+delta_2);
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha[s,t]*delta_2;
            
             
             //update unchosen TD & second stage values
             Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,t])*Q_TD[(choice[s,t,1] ? 1 : 2)];
             Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s,t])*Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[unc_state,1] = (1-alpha[s,t])*Q_2[unc_state,1];
             Q_2[unc_state,2] = (1-alpha[s,t])*Q_2[unc_state,2];
            
            //Use Beta distribution instead of fix value for trans p;
            if ((state_2[s,t]-choice[s,t,1]-1)) {alpha_b = alpha_b + 1;} else {beta_b = beta_b + 1;}
            mu_b = ((alpha_b) / (alpha_b+beta_b));
             
            Q_MB[1] =  ((mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (1-mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
            Q_MB[2] =  ((1-mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
         
                           
          //} //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,t]*delta_1;
          Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,t])*Q_TD[(choice[s,t,1] ? 1 : 2)];
          Q_2[1,1] = (1-alpha[s,t])*Q_2[1,1];
          Q_2[1,2] = (1-alpha[s,t])*Q_2[1,2];
          Q_2[2,1] = (1-alpha[s,t])*Q_2[2,1];
          Q_2[2,2] = (1-alpha[s,t])*Q_2[2,2];
          //MB update of first stage values based on second stage values, so don't change

        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values & update previous choice
      prev_choice=0;
      prev_motor=0;
      Q_TD[1] = (1-alpha[s,t])*Q_TD[1];
      Q_TD[2] = (1-alpha[s,t])*Q_TD[2];
      Q_2[1,1] = (1-alpha[s,t])*Q_2[1,1];
      Q_2[1,2] = (1-alpha[s,t])*Q_2[1,2];
      Q_2[2,1] = (1-alpha[s,t])*Q_2[2,1];
      Q_2[2,2] = (1-alpha[s,t])*Q_2[2,2];
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
  int prev_motor;
  int tran_count;
  int tran_type[2];
  int unc_state;
  real delta_1[nS,nT];
  real delta_2[nS,nT];
  real Q_TD[2];
  real Q_MB[2];
  real Q_2[2,2];
  real alpha_b; real beta_b;real mu_b;
  
  for (s in 1:nS) {
    for (i in 1:2) {
      Q_TD[i]=.5;
      Q_MB[i]=.5;
      Q_2[1,i]=.5;
      Q_2[2,i]=.5;
      tran_type[i]=0;
    }
    
  prev_motor=0;
  prev_choice=0;
  alpha_b=1;
  beta_b=1;
  
    for (t in 1:nT) {
      if (missing_choice[s,t,1]==0) {
        log_lik[s,t,1] = bernoulli_logit_lpmf(choice[s,t,1] | ((Q_TD[2]-Q_TD[1])*beta_1_MF[s,t])+((Q_MB[2]-Q_MB[1])*beta_1_MB[s,t])+(pers[s]*prev_choice) + (Mo_pers[s]*prev_motor));
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
         prev_motor = 2*motorchoice[s,t,1]-1;
        if (missing_choice[s,t,2]==0) {
          log_lik[s,t,2] = bernoulli_logit_lpmf(choice[s,t,2] | ((Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])*beta_2[s]));
          
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
            //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1[s,t] = Q_2[state_2[s,t],choice[s,t,2]+1]/alpha[s,t]-Q_TD[choice[s,t,1]+1]; 
             delta_2[s,t] = reward[s,t]/alpha[s,t] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s]*(delta_1+lambda[s]*delta_2);
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,t]*(delta_1[s,t]+delta_2[s,t]);
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha[s,t]*delta_2[s,t];
            
             
             //update unchosen TD & second stage values
             Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,t])*Q_TD[(choice[s,t,1] ? 1 : 2)];
             Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s,t])*Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[unc_state,1] = (1-alpha[s,t])*Q_2[unc_state,1];
             Q_2[unc_state,2] = (1-alpha[s,t])*Q_2[unc_state,2];
             
             //Use Beta distribution instead of fix value for trans p;
            if ((state_2[s,t]-choice[s,t,1]-1)) {alpha_b = alpha_b + 1;} else {beta_b = beta_b + 1;}
            mu_b = ((alpha_b) / (alpha_b+beta_b));
             
            Q_MB[1] =  ((mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (1-mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
            Q_MB[2] =  ((1-mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
            
          //} //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          log_lik[s,t,2] = 0;
          delta_1[s,t] = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
          delta_2[s,t] = -998;
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,t]*delta_1[s,t];
          Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,t])*Q_TD[(choice[s,t,1] ? 1 : 2)];
          Q_2[1,1] = (1-alpha[s,t])*Q_2[1,1];
          Q_2[1,2] = (1-alpha[s,t])*Q_2[1,2];
          Q_2[2,1] = (1-alpha[s,t])*Q_2[2,1];
          Q_2[2,2] = (1-alpha[s,t])*Q_2[2,2];
          //MB update of first stage values based on second stage values, so don't change

        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values
      log_lik[s,t,1] = 0;
      log_lik[s,t,2] = 0;
      Q_TD[1] = (1-alpha[s,t])*Q_TD[1];
      Q_TD[2] = (1-alpha[s,t])*Q_TD[2];
      Q_2[1,1] = (1-alpha[s,t])*Q_2[1,1];
      Q_2[1,2] = (1-alpha[s,t])*Q_2[1,2];
      Q_2[2,1] = (1-alpha[s,t])*Q_2[2,1];
      Q_2[2,2] = (1-alpha[s,t])*Q_2[2,2];
      delta_1[s,t] = -999;
      delta_2[s,t] = -999;
      prev_choice=0;
      prev_motor=0;
      }
    } 
  }
}


