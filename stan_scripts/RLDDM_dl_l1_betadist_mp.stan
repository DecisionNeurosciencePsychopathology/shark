data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  real rt[nS,nT,2]; 
  int<lower=-1,upper=1> motorchoice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
  int missing_choice[nS,nT,2];
  int skip_choice[nS,nT,2];
  //int missing_reward[nS,nT];
 // int<lower=0,upper=1> factorizedecay;
}

parameters {
  real alpha_m;
  real<lower=0> alpha_s;
  // real<lower=0> beta_1_MF_m;
  // real<lower=0> beta_1_MF_s;
  // real<lower=0> beta_1_MB_m;
  // real<lower=0> beta_1_MB_s;
  real<lower=0> beta_1_m;
  real<lower=0> beta_1_s;
  real<lower=0> omega_m;
  real<lower=0> omega_s;
  real<lower=0> beta_2_m;
  real<lower=0> beta_2_s;
  //real lambda_m;
  //real<lower=0> lambda_s;
  real pers_m;
  real<lower=0> pers_s;
  real Mo_pers_m;
  real<lower=0> Mo_pers_s;
  //DO NOT USE THIS HORRIBLE IDEA
  //real omega_m;
  //real<lower=0> omega_s;
  
  real<lower=0> alpha_ddm_s1_m;
  real beta_ddm_s1_m;
  real<lower=0> non_dec_s1_m;
  
  real<lower=0> alpha_ddm_s2_m;
  real beta_ddm_s2_m;
  real<lower=0> non_dec_s2_m;

  real<lower=0> alpha_ddm_s1_s;
  real<lower=0> beta_ddm_s1_s;
  real<lower=0> non_dec_s1_s;
  
  real<lower=0> alpha_ddm_s2_s;
  real<lower=0> beta_ddm_s2_s;
  real<lower=0> non_dec_s2_s;
  
  
  vector[nS] alpha_raw;
  //vector[nS] beta_1_MF_raw;
  //vector[nS] beta_1_MB_raw;
  vector[nS] beta_1_raw;
  vector[nS] omega_raw;
  vector[nS] beta_2_raw;
  //vector[nS] lambda_raw;
  vector[nS] pers_raw;
  vector[nS] Mo_pers_raw;
  //vector[nS] omega_raw;
  
  // alpha_ddm_s1
  // beta_ddm_s1
  // non_dec_s1
  // alpha_ddm_s2
  // beta_ddm_s2
  // non_dec_s2
  
  vector[nS] alpha_ddm_s1_raw;
  vector[nS] beta_ddm_s1_raw;
  vector[nS] non_dec_s1_raw;
  
  vector[nS] alpha_ddm_s2_raw;
  vector[nS] beta_ddm_s2_raw;
  vector[nS] non_dec_s2_raw;
  
  
}

transformed parameters {

  
  //define transformed parameters
  vector[nS] alpha;
  vector[nS] beta_1;
  vector[nS] omega;
  vector[nS] beta_2;
  
  vector[nS] alpha_normal;
  vector[nS] beta_1_normal;
  vector[nS] omega_normal;
  vector[nS] beta_2_normal;
  
  // vector[nS] alpha_ddm_s1_n;
  // vector[nS] alpha_ddm_s2_n;
  // vector[nS] non_dec_s1_n;
  // vector[nS] non_dec_s2_n;
  
  vector<lower=0>[nS] alpha_ddm_s1;
  vector[nS] beta_ddm_s1;
  vector<lower=0>[nS] non_dec_s1;
  vector<lower=0>[nS] alpha_ddm_s2;
  vector[nS] beta_ddm_s2;
  vector<lower=0>[nS] non_dec_s2;
  
  //vector<lower=0,upper=1>[nS] lambda;
  //vector[nS] lambda_normal;
  //vector[nS] omega;
  vector[nS] pers;
  vector[nS] Mo_pers;
  
  // print("alpha_s1")
  // print(alpha_ddm_s1_m)
  // print(alpha_ddm_s1_s)
  // print(alpha_ddm_s1_raw)
  // print("END")
  //create transformed parameters using non-centered parameterization for all
  // and logistic transformation for alpha & lambda (range: 0 to 1)
  alpha_normal = alpha_m + (alpha_s*alpha_raw);
  beta_2_normal = beta_2_m + (beta_2_s*beta_2_raw);
  
  beta_1_normal = beta_1_m + (beta_1_s*beta_1_raw);
  omega_normal = omega_m + (omega_s*omega_raw);
  
  beta_ddm_s1 = beta_ddm_s1_m + (beta_ddm_s1_s * beta_ddm_s1_raw);
  beta_ddm_s2 = beta_ddm_s2_m + (beta_ddm_s2_s * beta_ddm_s2_raw);
  //lambda_normal = lambda_m + lambda_s*lambda_raw;
  //lambda = inv_logit(lambda_normal);
  pers = pers_m + pers_s*pers_raw;
  Mo_pers = Mo_pers_m + Mo_pers_s * Mo_pers_raw;
  
  alpha_ddm_s1 = alpha_ddm_s1_m + (alpha_ddm_s1_s * alpha_ddm_s1_raw);
  alpha_ddm_s2 = alpha_ddm_s2_m + (alpha_ddm_s2_s * alpha_ddm_s2_raw);
  
  non_dec_s1 = non_dec_s1_m + (non_dec_s1_s * non_dec_s1_raw);
  non_dec_s2 = non_dec_s2_m + (non_dec_s2_s * non_dec_s2_raw);
  
 
  // for (s in 1:nS) {
  // non_dec_s1[s] = 0.2;
  // non_dec_s2[s] = 0.2;
  // }
  // print("alpha_s1")
  // print(alpha_ddm_s1_m)
  // print(alpha_ddm_s1_s)
  // print(alpha_ddm_s1_raw)
  // print("END")
  // 
  
  
  alpha = inv_logit(alpha_normal);
  beta_1 = exp(beta_1_normal);
  omega = exp(omega_normal);
  beta_2 = exp(beta_2_normal);
  // //non_dec_s1 = (non_dec_s1_n);
  // non_dec_s2 = (non_dec_s2_n);
  
  // for (s in 1:nS) {
  // non_dec_s1[s] = 0.2;
  // non_dec_s2[s] = 0.2;
  // }

  // alpha_ddm_s1 = (alpha_ddm_s1_n);
  // alpha_ddm_s2 = (alpha_ddm_s2_n);
}

model {
 //define variables
  int prev_choice;
  int prev_motor;
  //int tran_count;
  //int tran_type[2];
  int unc_state;
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_S1[2];
  real Q_2[2,2];
  real drift[2];
  real alpha_b; real beta_b;real mu_b;
 
  
  //define priors
  alpha_m ~ normal(0,2.5);
  beta_1_m ~ normal(0,2.5);
  omega_m ~ normal(0,2.5);
  // beta_1_MF_m ~ normal(0,5);
  // beta_1_MB_m ~ normal(0,5);
  beta_2_m ~ normal(0,5);
  //lambda_m ~ normal(0,2);
  pers_m ~ normal(0,2.5);
  Mo_pers_m ~ normal(0,2.5);
  
  alpha_ddm_s1_m ~ normal(1.5,1);
  beta_ddm_s1_m ~ normal(0,5);
  non_dec_s1_m ~ normal(0.2,0.1);
  alpha_ddm_s2_m ~ normal(1.5,1);
  beta_ddm_s2_m ~ normal(0,5);
  non_dec_s2_m ~ normal(0.2,0.1);
  
  
  alpha_s ~ cauchy(0,1);
  beta_1_s ~ cauchy(0,1);
  omega_s ~ cauchy(0,1);
  // beta_1_MF_s ~ cauchy(0,1);
  // beta_1_MB_s ~ cauchy(0,1);
  beta_2_s ~ cauchy(0,1);
  //lambda_s ~ cauchy(0,1);
  pers_s ~ cauchy(0,1);
  Mo_pers_s ~ cauchy(0,1);
  
  alpha_ddm_s1_s ~ cauchy(0,1);
  beta_ddm_s1_s ~ cauchy(0,1);
  non_dec_s1_s ~ cauchy(0,1);
  alpha_ddm_s2_s ~ cauchy(0,1);
  beta_ddm_s2_s ~ cauchy(0,1);
  non_dec_s2_s ~ cauchy(0,1);
  
  
  alpha_raw ~ normal(0,1);
  // beta_1_MF_raw ~ normal(0,1);
  // beta_1_MB_raw ~ normal(0,1);
  beta_2_raw ~ normal(0,1);
  omega_raw ~ normal(0,1);
  beta_1_raw ~ normal(0,1);
  //lambda_raw ~ normal(0,1);
  pers_raw ~ normal(0,1);
  Mo_pers_raw ~ normal(0,1);
  
  alpha_ddm_s1_raw ~ normal(0,1);
  beta_ddm_s1_raw ~ normal(0,1);
  non_dec_s1_raw ~ normal(0,1);
  alpha_ddm_s2_raw ~ normal(0,1);
  beta_ddm_s2_raw ~ normal(0,1);
  non_dec_s2_raw ~ normal(0,1);
  // 
  for (s in 1:nS) {
  
  //set initial values
  for (i in 1:2) {
    Q_TD[i]=.5;
    //Q_MB[i]=.5;
    Q_2[1,i]=.5;
    Q_2[2,i]=.5;
   // tran_type[i]=0;
  }
  prev_motor=0;
  prev_choice=0;
  alpha_b=1;
  beta_b=1;
  
    for (t in 1:nT) {
      //use if not missing 1st stage choice
      
      //Use Beta distribution instead of fix value for trans p; Let's update it before hand;
      mu_b = ((alpha_b) / (alpha_b+beta_b));
      Q_MB[1] =  ((mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (1-mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
      Q_MB[2] =  ((1-mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
      Q_S1[1] = omega[s] * Q_MB[1] + (1-omega[s]) * Q_TD[1];
      Q_S1[2] = omega[s] * Q_MB[2] + (1-omega[s]) * Q_TD[2];
      if (missing_choice[s,t,1]==0) {
         
         if(skip_choice[s,t,1]==0) {
         
        //print(beta_1_MF[s,t]);
         choice[s,t,1] ~ bernoulli_logit(
           (Q_S1[2] - Q_S1[1]) * beta_1[s] + (pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
          );
           //This section is for drift calculation:
          //wiener: 
        if(choice[s,t,1]) {drift[1] = beta_ddm_s1[s]*(Q_S1[2] - Q_S1[1]);}else{drift[1] = -(beta_ddm_s1[s]*(Q_S1[2] - Q_S1[1]));}
        rt[s,t,1] ~ wiener(alpha_ddm_s1[s], non_dec_s1[s], 0.5, drift[1]);
      
         }

        prev_choice = 2*choice[s,t,1]-1; //choice is 1 and 0; 1 should be 1 and 0 should be -1
        prev_motor = motorchoice[s,t,1]; //motorchoice is already 1, -1
        //use if not missing 2nd stage choice
        if (missing_choice[s,t,2]==0) {
          if(skip_choice[s,t,2]==0) {
          
          choice[s,t,2] ~ bernoulli_logit( ((Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])*beta_2[s]) );
          if(choice[s,t,2]) {
            drift[2] = (beta_ddm_s2[s]*(Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1]));
          }else{drift[2] = -((beta_ddm_s2[s]*(Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])));}
          
          rt[s,t,2] ~ wiener(alpha_ddm_s2[s], non_dec_s2[s], 0.5,drift[2]);
          }
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]/alpha[s]-Q_TD[choice[s,t,1]+1]; 
             delta_2 = reward[s,t]/alpha[s] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             //tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             //tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s]*(delta_1+lambda[s]*delta_2);
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + (alpha[s]*delta_1) + (1*delta_2);
             
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha[s]*delta_2;
            
             
             //update unchosen TD & second stage values
             Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s])*Q_TD[(choice[s,t,1] ? 1 : 2)];
             Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s])*Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[unc_state,1] = (1-alpha[s])*Q_2[unc_state,1];
             Q_2[unc_state,2] = (1-alpha[s])*Q_2[unc_state,2];
            
             if (choice[s,t,1]==0 && state_2[s,t]==1) {alpha_b = alpha_b + 1;}   
             if (choice[s,t,1]==1 && state_2[s,t]==2) {alpha_b = alpha_b + 1;}
             
             if (choice[s,t,1]==0 && state_2[s,t]==2) {beta_b = beta_b + 1;}   
             if (choice[s,t,1]==1 && state_2[s,t]==1) {beta_b = beta_b + 1;}
          //} //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s]*delta_1;
          Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s])*Q_TD[(choice[s,t,1] ? 1 : 2)];
          Q_2[1,1] = (1-alpha[s])*Q_2[1,1];
          Q_2[1,2] = (1-alpha[s])*Q_2[1,2];
          Q_2[2,1] = (1-alpha[s])*Q_2[2,1];
          Q_2[2,2] = (1-alpha[s])*Q_2[2,2];
         
          //MB update of first stage values based on second stage values, so don't change //not true...update happened post 
        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values & update previous choice
      prev_choice=0;
      prev_motor=0;
      Q_TD[1] = (1-alpha[s])*Q_TD[1];
      Q_TD[2] = (1-alpha[s])*Q_TD[2];
      Q_2[1,1] = (1-alpha[s])*Q_2[1,1];
      Q_2[1,2] = (1-alpha[s])*Q_2[1,2];
      Q_2[2,1] = (1-alpha[s])*Q_2[2,1];
      Q_2[2,2] = (1-alpha[s])*Q_2[2,2];
      }
    }
  }
}

generated quantities {
  //same code as above, with following changes: 
  // 1) values and choices used to calculate probability, rather than fitting values to choices
  // 2) no priors, etc.- uses estimated pararamter values from model block
  real log_lik[nS,nT,2]; //log likelihood- must be named this
  real log_lik_rt[nS,nT,2];
  int prev_choice;
  int prev_motor;
  //int tran_count;
  //int tran_type[2];
  int unc_state;
  real delta_1[nS,nT];
  real delta_2[nS,nT];
  real Q_TD[nS,nT+1,2];
  real Q_MB[nS,nT,2];
  real Q_S1[nS,nT,2];
  real Q_2[nS,nT+1,2,2];
  real alpha_b[nS,nT+1]; 
  real beta_b[nS,nT+1];
  real mu_b[nS,nT];
 
  
  for (s in 1:nS) {
    for (i in 1:2) {
      Q_TD[s,1,i]=.5;
      //Q_MB[s,1,i]=.5;
      Q_2[s,1,1,i]=.5;
      Q_2[s,1,2,i]=.5;
     // tran_type[i]=0;
    }
  prev_motor=0;
  prev_choice=0;
  alpha_b[s,1]=1;
  beta_b[s,1]=1;
    for (t in 1:nT) {
       //print(choice[s,t,1])
       //print(state_2[s,t])
      mu_b[s,t] = ( (alpha_b[s,t]) / (alpha_b[s,t]+beta_b[s,t]) );
     
      Q_MB[s,t,1] =  ((mu_b[s,t])*fmax(Q_2[s,t,1,1],Q_2[s,t,1,2]) + (1-mu_b[s,t])*fmax(Q_2[s,t,2,1],Q_2[s,t,2,2]));
      Q_MB[s,t,2] =  ((1-mu_b[s,t])*fmax(Q_2[s,t,1,1],Q_2[s,t,1,2]) + (mu_b[s,t])*fmax(Q_2[s,t,2,1],Q_2[s,t,2,2]));
      Q_S1[s,t,1] = omega[s] * Q_MB[s,t,1] + (1-omega[s]) * Q_TD[s,t,1];
      Q_S1[s,t,2] = omega[s] * Q_MB[s,t,2] + (1-omega[s]) * Q_TD[s,t,2];
      //print (alpha_b[s,t])
      //print (beta_b[s,t])
      if (missing_choice[s,t,1]==0) {

        if(skip_choice[s,t,1]==0) {
         
          log_lik[s,t,1] = bernoulli_logit_lpmf(choice[s,t,1] |  ((Q_S1[s,t,2] - Q_S1[s,t,1]) * beta_1[s]) + (pers[s]*prev_choice) + (Mo_pers[s]*prev_motor) );

          log_lik_rt[s,t,1] = wiener_lpdf(rt[s,t,1] | alpha_ddm_s1[s], non_dec_s1[s], 0.5, beta_ddm_s1[s]*(Q_S1[s,t,2] - Q_S1[s,t,1]));
         
        } else {
          log_lik[s,t,1] = 0;
          log_lik_rt[s,t,1] = 0;}
        prev_choice = (2*choice[s,t,1])-1; //1 if choice 2, -1 if choice 1
        prev_motor = motorchoice[s,t,1];
        
        if (missing_choice[s,t,2]==0) {
          if(skip_choice[s,t,2]==0) {
             
             log_lik[s,t,2] = bernoulli_logit_lpmf(choice[s,t,2] | ((Q_2[s,t,state_2[s,t],2]-Q_2[s,t,state_2[s,t],1])*beta_2[s]));
             
             log_lik_rt[s,t,2] = wiener_lpdf(rt[s,t,2] | alpha_ddm_s2[s], non_dec_s2[s], 0.5, (beta_ddm_s2[s]*(Q_2[s,t,state_2[s,t],2]-Q_2[s,t,state_2[s,t],1])) );
             
          } else {
            log_lik[s,t,2] = 0;
            log_lik_rt[s,t,2] = 0;}
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
            //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1[s,t] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1]/alpha[s] - Q_TD[s,t,choice[s,t,1]+1]; 
             delta_2[s,t] = reward[s,t]/alpha[s] - Q_2[s,t,state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             //tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             //tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s]*(delta_1+lambda[s]*delta_2);
             Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + (alpha[s]*delta_1[s,t])+(1*delta_2[s,t]);
             Q_2[s,t+1,state_2[s,t],choice[s,t,2]+1] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1] + alpha[s]*delta_2[s,t];
            
             
             //update unchosen TD & second stage values
             Q_TD[s,t+1,(choice[s,t,1] ? 1 : 2)] = (1-alpha[s])*Q_TD[s,t,(choice[s,t,1] ? 1 : 2)];
             Q_2[s,t+1,state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s])*Q_2[s,t,state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[s,t+1,unc_state,1] = (1-alpha[s])*Q_2[s,t,unc_state,1];
             Q_2[s,t+1,unc_state,2] = (1-alpha[s])*Q_2[s,t,unc_state,2];
             
             //Use Beta distribution instead of fix value for trans p;
             //print(choice[s,t,1])
             //print(state_2[s,t])
             
             if (choice[s,t,1]==0 && state_2[s,t]==1) {alpha_b[s,t+1] = alpha_b[s,t] + 1;beta_b[s,t+1] = beta_b[s,t];}   
             if (choice[s,t,1]==1 && state_2[s,t]==2) {alpha_b[s,t+1] = alpha_b[s,t] + 1;beta_b[s,t+1] = beta_b[s,t];}
             if (choice[s,t,1]==0 && state_2[s,t]==2) {beta_b[s,t+1] = beta_b[s,t] + 1;alpha_b[s,t+1] = alpha_b[s,t];}   
             if (choice[s,t,1]==1 && state_2[s,t]==1) {beta_b[s,t+1] = beta_b[s,t] + 1;alpha_b[s,t+1] = alpha_b[s,t];}
           
          //} //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          log_lik[s,t,2] = 0;
          log_lik_rt[s,t,2] = 0;
          delta_1[s,t] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1]-Q_TD[s,t,choice[s,t,1]+1]; 
          delta_2[s,t] = -998;
          Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + alpha[s]*delta_1[s,t];
          Q_TD[s,t+1,(choice[s,t,1] ? 1 : 2)] = (1-alpha[s])*Q_TD[s,t,(choice[s,t,1] ? 1 : 2)];
          Q_2[s,t+1,1,1] = (1-alpha[s])*Q_2[s,t,1,1];
          Q_2[s,t+1,1,2] = (1-alpha[s])*Q_2[s,t,1,2];
          Q_2[s,t+1,2,1] = (1-alpha[s])*Q_2[s,t,2,1];
          Q_2[s,t+1,2,2] = (1-alpha[s])*Q_2[s,t,2,2];
          //MB update of first stage values based on second stage values, so don't change //But that's not true because second stage value decayed ;_;
          alpha_b[s,t+1] = alpha_b[s,t];
          beta_b[s,t+1] = beta_b[s,t];
         
        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values
      log_lik[s,t,1] = 0;
      log_lik[s,t,2] = 0;
      log_lik_rt[s,t,1] = 0;
      log_lik_rt[s,t,2] = 0;
      Q_TD[s,t+1,1] = (1-alpha[s])*Q_TD[s,t,1];
      Q_TD[s,t+1,2] = (1-alpha[s])*Q_TD[s,t,2];
      Q_2[s,t+1,1,1] = (1-alpha[s])*Q_2[s,t,1,1];
      Q_2[s,t+1,1,2] = (1-alpha[s])*Q_2[s,t,1,2];
      Q_2[s,t+1,2,1] = (1-alpha[s])*Q_2[s,t,2,1];
      Q_2[s,t+1,2,2] = (1-alpha[s])*Q_2[s,t,2,2];
      //Decay Q_MB as well
      alpha_b[s,t+1] = alpha_b[s,t];
      beta_b[s,t+1] = beta_b[s,t];
       
      delta_1[s,t] = -999;
      delta_2[s,t] = -999;
      prev_choice=0;
      prev_motor=0;
      }
   } 
  }
}


