data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
  int missing_choice[nS,nT,2];
  int missing_reward[nS,nT];
}

parameters {
  real alpha_1_m;
  real<lower=0> alpha_1_s;
  real alpha_2_m;
  real<lower=0> alpha_2_s;
  real<lower=0> beta_1_m;
  real<lower=0> beta_1_s;
  real<lower=0> beta_2_m;
  real<lower=0> beta_2_s;
  real omega_m;
  real<lower=0> omega_s;
  real lambda_m;
  real<lower=0> lambda_s;
  real pers_m;
  real<lower=0> pers_s;
  real eps_m;
  real<lower=0> eps_s;
  vector[nS] alpha_1_raw;
  vector[nS] alpha_2_raw;
  vector[nS] beta_1_raw;
  vector[nS] beta_2_raw;
  vector[nS] omega_raw;
  vector[nS] lambda_raw;
  vector[nS] pers_raw;
  vector[nS] eps_raw;
}

transformed parameters {
  //define transformed parameters
  vector<lower=0,upper=1>[nS] alpha_1;
  vector<lower=0,upper=1>[nS] alpha_2;
  vector[nS] alpha_1_normal;
  vector[nS] alpha_2_normal;
  vector[nS] beta_1;
  vector[nS] beta_2;
  vector<lower=0,upper=1>[nS] omega;
  vector<lower=0,upper=1>[nS] lambda;
  vector[nS] omega_normal;
  vector[nS] lambda_normal;
  vector[nS] pers;
  vector<lower=0,upper=1>[nS] epsilon;
  vector[nS] eps_normal;

  //create transformed parameters using non-centered parameterization for all
  // and logistic transformation for alpha, omega, & lambda (range: 0 to 1)
  alpha_1_normal = alpha_1_m + alpha_1_s*alpha_1_raw;
  alpha_1 = inv_logit(alpha_1_normal);
  alpha_2_normal = alpha_2_m + alpha_2_s*alpha_2_raw;
  alpha_2 = inv_logit(alpha_2_normal);
  beta_1 = beta_1_m + beta_1_s*beta_1_raw;
  beta_2 = beta_2_m + beta_2_s*beta_2_raw;
  omega_normal = omega_m + omega_s*omega_raw;
  omega = inv_logit(omega_normal);
  lambda_normal = lambda_m + lambda_s*lambda_raw;
  lambda = inv_logit(lambda_normal);
  pers = pers_m + pers_s*pers_raw;
  eps_normal = eps_m + eps_s*eps_raw;
  epsilon = inv_logit(eps_normal);
}

model {
  //define variables
  int prev_choice;
  int tran_count;
  int tran_type[2];
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_net[2];
  real Q_2[2,2];
  
  //define priors
  alpha_1_m ~ normal(0,2);
  alpha_2_m ~ normal(0,2);
  beta_1_m ~ normal(0,5);
  beta_2_m ~ normal(0,5);
  omega_m ~ normal(0,2);
  lambda_m ~ normal(0,2);
  pers_m ~ normal(0,2);
  eps_m ~ normal(0,2);
  alpha_1_s ~ cauchy(0,1);
  alpha_2_s ~ cauchy(0,1);
  beta_1_s ~ cauchy(0,2);
  beta_2_s ~ cauchy(0,2);
  omega_s ~ cauchy(0,1);
  lambda_s ~ cauchy(0,1);
  pers_s ~ cauchy(0,1);
  eps_s ~ cauchy(0,1);
  alpha_1_raw ~ normal(0,1);
  alpha_2_raw ~ normal(0,1);
  beta_1_raw ~ normal(0,1);
  beta_2_raw ~ normal(0,1);
  omega_raw ~ normal(0,1);
  lambda_raw ~ normal(0,1);
  pers_raw ~ normal(0,1);
  eps_raw ~ normal(0,1);
  
  for (s in 1:nS) {
  
  //set initial values
  for (i in 1:2) {
    Q_TD[i]=.5;
    Q_MB[i]=.5;
    Q_net[i]=.5;
    Q_2[1,i]=.5;
    Q_2[2,i]=.5;
    tran_type[i]=0;
  }
  prev_choice=0;
  
    for (t in 1:nT) {
      //use if not missing 1st stage choice
      if (missing_choice[s,t,1]==0) {
        choice[s,t,1] ~ bernoulli((1-epsilon[s])*inv_logit(beta_1[s]*(Q_net[2]-Q_net[1])+pers[s]*prev_choice) + .5*epsilon[s]);
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
        
        //use if not missing 2nd stage choice
        if (missing_choice[s,t,2]==0) {
          choice[s,t,2] ~ bernoulli((1-epsilon[s])*inv_logit(beta_2[s]*(Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])) + .5*epsilon[s]);
          
          //use if not missing 2nd stage reward
          if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_net[choice[s,t,1]+1]; 
             delta_2 = reward[s,t] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update values
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_1[s]*(delta_1+lambda[s]*delta_2);
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha_2[s]*delta_2;
             Q_MB[1] = (tran_type[1] > tran_type[2]) ? (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2])) : (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2]));
             Q_MB[2] = (tran_type[1] > tran_type[2]) ? (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2])) : (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2]));
             Q_net[1] = omega[s]*Q_MB[1] + (1-omega[s])*Q_TD[1];
             Q_net[2] = omega[s]*Q_MB[2] + (1-omega[s])*Q_TD[2];
            
          } //if missing 2nd stage reward: do nothing
          
        } else { //if missing 2nd stage choice: still update 1st stage TD values
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_net[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_1[s]*delta_1;
          //MB update of first stage values based on second stage values, so don't change
          Q_net[1] = omega[s]*Q_MB[1] + (1-omega[s])*Q_TD[1];
          Q_net[2] = omega[s]*Q_MB[2] + (1-omega[s])*Q_TD[2];
        }
      } else { //if missing 1st stage choice: update previous choice
      prev_choice=0;
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
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_net[2];
  real Q_2[2,2];
  
  for (s in 1:nS) {
    for (i in 1:2) {
      Q_TD[i]=.5;
      Q_MB[i]=.5;
      Q_net[i]=.5;
      Q_2[1,i]=.5;
      Q_2[2,i]=.5;
      tran_type[i]=0;
    }
    prev_choice=0;
    for (t in 1:nT) {
      if (missing_choice[s,t,1]==0) {
        log_lik[s,t,1] = bernoulli_lpmf(choice[s,t,1] | (1-epsilon[s])*inv_logit(beta_1[s]*(Q_net[2]-Q_net[1])+pers[s]*prev_choice) + .5*epsilon[s]);
        prev_choice = 2*choice[s,t,1]-1; //1 if choice 2, -1 if choice 1
        
        if (missing_choice[s,t,2]==0) {
          log_lik[s,t,2] = bernoulli_lpmf(choice[s,t,2] | (1-epsilon[s])*inv_logit(beta_2[s]*(Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])) + .5*epsilon[s]);
          if (missing_reward[s,t]==0) {
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_net[choice[s,t,1]+1]; 
             delta_2 = reward[s,t] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             tran_type[tran_count] = tran_type[tran_count] + 1;
             
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_1[s]*(delta_1+lambda[s]*delta_2);
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha_2[s]*delta_2;
             Q_MB[1] = (tran_type[1] > tran_type[2]) ? (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2])) : (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2]));
             Q_MB[2] = (tran_type[1] > tran_type[2]) ? (.3*fmax(Q_2[1,1],Q_2[1,2]) + .7*fmax(Q_2[2,1],Q_2[2,2])) : (.7*fmax(Q_2[1,1],Q_2[1,2]) + .3*fmax(Q_2[2,1],Q_2[2,2]));
             Q_net[1] = omega[s]*Q_MB[1] + (1-omega[s])*Q_TD[1];
             Q_net[2] = omega[s]*Q_MB[2] + (1-omega[s])*Q_TD[2];
          }
        } else {
          log_lik[s,t,2] = 0;
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_net[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha_1[s]*delta_1;
          Q_net[1] = omega[s]*Q_MB[1] + (1-omega[s])*Q_TD[1];
          Q_net[2] = omega[s]*Q_MB[2] + (1-omega[s])*Q_TD[2];
        }
      } else {
        prev_choice=0;
        log_lik[s,t,1] = 0;
        log_lik[s,t,2] = 0;
      }
    }
  }
}
