data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  int<lower=-1,upper=1> motorchoice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
  int missing_choice[nS,nT,2];
  int skip_choice[nS,nT,2];
  int<lower=0,upper=1> shark [nS,nT];
  int<lower=0,upper=1> grp_dep[nS];
  int<lower=0,upper=1> grp_ide[nS];
  int<lower=0,upper=1> grp_att[nS];
  int Group[nS];
}

parameters {
  real alpha_m;
  real<lower=0> alpha_s;
  real beta_1_m;
  real<lower=0> beta_1_s;
  real omega_m;
  real<lower=0> omega_s;
  real beta_2_m;
  real<lower=0> beta_2_s;
  
  real alpha_sh_m;
  real beta1_sh_m;
  real omega_sh_m;

  real<lower=0> alpha_sh_s;
  real<lower=0> beta1_sh_s;
  real<lower=0> omega_sh_s;
  
  vector[3] alpha_sh_grp;
  vector[3] beta1_sh_grp;
  vector[3] omega_sh_grp;
  
  vector[3] alpha_grp;
  vector[3] beta1_grp;
  vector[3] omega_grp;
  
  real pers_m;
  real<lower=0> pers_s;
  real Mo_pers_m;
  real<lower=0> Mo_pers_s;
  
  vector[nS] alpha_raw;
  
  vector[nS] beta_1_raw;
  vector[nS] omega_raw;
  
  vector[nS] beta_2_raw;
  
  vector[nS] pers_raw;
  vector[nS] Mo_pers_raw;
  
  vector[nS] alpha_sh_raw;
  vector[nS] beta1_sh_raw;
  vector[nS] omega_sh_raw;
  
}

transformed parameters {

  //define transformed parameters
  vector[2] alpha_normal[nS];
  vector[2] beta_1_normal[nS];
  vector[2] omega_normal[nS];
  vector[nS] beta_2_normal;
  
  vector<lower=0,upper=1>[2] alpha[nS];
  vector<lower=0>[2] beta_1[nS];
  vector<lower=0,upper=1>[2] omega[nS];
  vector<lower=0>[nS] beta_2;
  
  vector[nS] pers;
  vector[nS] Mo_pers;
  vector[nS] alpha_sh;
  vector[nS] beta1_sh;
  vector[nS] omega_sh;
 
  for (s in 1:nS){
    if(Group[s]==0) {
        alpha_sh[s] = alpha_sh_m + (alpha_sh_s*alpha_sh_raw[s]);
        beta1_sh[s] = beta1_sh_m + (beta1_sh_s*beta1_sh_raw[s]);
        omega_sh[s] = omega_sh_m + (omega_sh_s*omega_sh_raw[s]);
        
        alpha_normal[s,1] = alpha_m + (alpha_s*alpha_raw[s]);
        alpha_normal[s,2] = alpha_m + (alpha_s*alpha_raw[s]) + alpha_sh[s];
        
        beta_1_normal[s,1] = beta_1_m + (beta_1_s*beta_1_raw[s]);
        beta_1_normal[s,2] = beta_1_m + (beta_1_s*beta_1_raw[s]) + beta1_sh[s];
        
        omega_normal[s,1] = omega_m + (omega_s*omega_raw[s]);
        omega_normal[s,2] = omega_m + (omega_s*omega_raw[s]) + omega_sh[s];
    } else {
        alpha_sh[s] = alpha_sh_m + (alpha_sh_s*alpha_sh_raw[s]) + alpha_sh_grp[Group[s]];
        beta1_sh[s] = beta1_sh_m + (beta1_sh_s*beta1_sh_raw[s]) + beta1_sh_grp[Group[s]];
        omega_sh[s] = omega_sh_m + (omega_sh_s*omega_sh_raw[s]) + omega_sh_grp[Group[s]];
      
        alpha_normal[s,1] = alpha_m + (alpha_s*alpha_raw[s]) + alpha_grp[Group[s]];
        alpha_normal[s,2] = alpha_m + (alpha_s*alpha_raw[s]) + alpha_sh[s] + alpha_grp[Group[s]];
        
        beta_1_normal[s,1] = beta_1_m + (beta_1_s*beta_1_raw[s]) + beta1_grp[Group[s]];
        beta_1_normal[s,2] = beta_1_m + (beta_1_s*beta_1_raw[s]) + beta1_grp[Group[s]] + beta1_sh[s];
        
        omega_normal[s,1] = omega_m + (omega_s*omega_raw[s]) + omega_grp[Group[s]];
        omega_normal[s,2] = omega_m + (omega_s*omega_raw[s]) + omega_grp[Group[s]] + omega_sh[s];
    }
  }
  beta_2_normal = beta_2_m + (beta_2_s*beta_2_raw);
  
  pers = pers_m + pers_s*pers_raw;
  Mo_pers = Mo_pers_m + Mo_pers_s * Mo_pers_raw;
  
  alpha = inv_logit(alpha_normal);
  omega = inv_logit(omega_normal);
  beta_1 = exp(beta_1_normal);
  
  beta_2 = exp(beta_2_normal);
  

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
  real Q_net[2];
  real Q_2[2,2];
  real alpha_b; real beta_b;real mu_b;
 
  
  //define priors
  alpha_m ~ normal(0,2.5);
  beta_1_m ~ normal(0,2.5);
  omega_m ~ normal(0,2.5);
  beta_2_m ~ normal(0,2.5);
  
  pers_m ~ normal(0,2.5);
  Mo_pers_m ~ normal(0,2.5);
  
  alpha_s ~ cauchy(0,1);
  beta_1_s ~ cauchy(0,1);
  omega_s ~ cauchy(0,1);
  beta_2_s ~ cauchy(0,1);
 
  pers_s ~ cauchy(0,1);
  Mo_pers_s ~ cauchy(0,1);
  
  alpha_raw ~ normal(0,1);
  beta_1_raw ~ normal(0,1);
  omega_raw ~ normal(0,1);
  beta_2_raw ~ normal(0,1);
  
  pers_raw ~ normal(0,1);
  Mo_pers_raw ~ normal(0,1);
  
  alpha_sh_m ~ normal(0,4);
  beta1_sh_m ~ normal(0,4);
  omega_sh_m ~ normal(0,4);
  
  alpha_sh_s ~ cauchy(0,1);
  beta1_sh_s ~ cauchy(0,1);
  omega_sh_s ~ cauchy(0,1);
  
  alpha_sh_raw ~ normal(0,1);
  beta1_sh_raw ~ normal(0,1);
  omega_sh_raw ~ normal(0,1);
  
  alpha_sh_grp ~ normal(0,4);
  beta1_sh_grp ~ normal(0,4);
  omega_sh_grp ~ normal(0,4);
  
  alpha_grp ~ normal(0,4);
  beta1_grp ~ normal(0,4);
  omega_grp ~ normal(0,4);
  
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
      
      Q_net[1] = (omega[s,shark[s,t]+1] * Q_MB[1]) + ((1-omega[s,shark[s,t]+1]) * Q_TD[1]); 
      Q_net[2] = (omega[s,shark[s,t]+1] * Q_MB[1]) + ((1-omega[s,shark[s,t]+1]) * Q_TD[1]); 
      
      if (missing_choice[s,t,1]==0) {
         
         if(skip_choice[s,t,1]==0) {
        //print(beta_1_MF[s,t]);
         choice[s,t,1] ~ bernoulli_logit( 
          // ((Q_TD[2]-Q_TD[1])*beta_1_MF[s,shark[s,t]+1])+((Q_MB[2]-Q_MB[1])*beta_1_MB[s,shark[s,t]+1])+(pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
          (Q_net[2]-Q_net[1])*beta_1[s,shark[s,t]+1] + (pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
          );
         }
         
        prev_choice = 2*choice[s,t,1]-1; //choice is 1 and 0; 1 should be 1 and 0 should be -1
        prev_motor = motorchoice[s,t,1]; //motorchoice is already 1, -1
        //use if not missing 2nd stage choice
        if (missing_choice[s,t,2]==0) {
          if(skip_choice[s,t,2]==0) {
          choice[s,t,2] ~ bernoulli_logit( ((Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1])*beta_2[s]) );
          }
          
          
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
             //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]/alpha[s,shark[s,t]+1]-Q_TD[choice[s,t,1]+1]; 
             delta_2 = reward[s,t]/alpha[s,shark[s,t]+1] - Q_2[state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             //tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             //tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,shark[s,t]+1]*(delta_1+lambda[s]*delta_2);
             Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + (alpha[s,shark[s,t]+1]*delta_1)+(0.99*delta_2);
             
             Q_2[state_2[s,t],choice[s,t,2]+1] = Q_2[state_2[s,t],choice[s,t,2]+1] + alpha[s,shark[s,t]+1]*delta_2;
            
             
             //update unchosen TD & second stage values
             Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_TD[(choice[s,t,1] ? 1 : 2)];
             Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_2[state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[unc_state,1] = (1-alpha[s,shark[s,t]+1])*Q_2[unc_state,1];
             Q_2[unc_state,2] = (1-alpha[s,shark[s,t]+1])*Q_2[unc_state,2];
            
             if (choice[s,t,1]==0 && state_2[s,t]==1) {alpha_b = alpha_b + 1;}   
             if (choice[s,t,1]==1 && state_2[s,t]==2) {alpha_b = alpha_b + 1;}
             
             if (choice[s,t,1]==0 && state_2[s,t]==2) {beta_b = beta_b + 1;}   
             if (choice[s,t,1]==1 && state_2[s,t]==1) {beta_b = beta_b + 1;}
          //} //if missing 2nd stage reward: do nothing
          
        } else if (missing_choice[s,t,2]==1) { //if missing 2nd stage choice or reward: still update 1st stage TD values, decay 2nd stage values
          delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
          Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,shark[s,t]+1]*delta_1;
          Q_TD[(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_TD[(choice[s,t,1] ? 1 : 2)];
          Q_2[1,1] = (1-alpha[s,shark[s,t]+1])*Q_2[1,1];
          Q_2[1,2] = (1-alpha[s,shark[s,t]+1])*Q_2[1,2];
          Q_2[2,1] = (1-alpha[s,shark[s,t]+1])*Q_2[2,1];
          Q_2[2,2] = (1-alpha[s,shark[s,t]+1])*Q_2[2,2];
         
          //MB update of first stage values based on second stage values, so don't change //not true...update happened post 
        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values & update previous choice
      prev_choice=0;
      prev_motor=0;
      Q_TD[1] = (1-alpha[s,shark[s,t]+1])*Q_TD[1];
      Q_TD[2] = (1-alpha[s,shark[s,t]+1])*Q_TD[2];
      Q_2[1,1] = (1-alpha[s,shark[s,t]+1])*Q_2[1,1];
      Q_2[1,2] = (1-alpha[s,shark[s,t]+1])*Q_2[1,2];
      Q_2[2,1] = (1-alpha[s,shark[s,t]+1])*Q_2[2,1];
      Q_2[2,2] = (1-alpha[s,shark[s,t]+1])*Q_2[2,2];
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
  //int tran_count;
  //int tran_type[2];
  int unc_state;
  real delta_1[nS,nT];
  real delta_2[nS,nT];
  real Q_TD[nS,nT+1,2];
  real Q_MB[nS,nT,2];
  real Q_net[nS,nT,2];
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
      
      Q_net[s,t,1] = (omega[s,shark[s,t]+1] * Q_MB[s,t,1]) + ((1-omega[s,shark[s,t]+1]) * Q_TD[s,t,1]); 
      Q_net[s,t,2] = (omega[s,shark[s,t]+1] * Q_MB[s,t,1]) + ((1-omega[s,shark[s,t]+1]) * Q_TD[s,t,1]); 
      
      if (missing_choice[s,t,1]==0) {
        //print( ((Q_TD[s,t,2]-Q_TD[s,t,1])*beta_1_MF[s]) + ((Q_MB[s,t,2]-Q_MB[s,t,1])*beta_1_MB[s])+(pers[s]*prev_choice) + (Mo_pers[s]*prev_motor) )
        if(skip_choice[s,t,1]==0) {
        log_lik[s,t,1] = bernoulli_logit_lpmf(choice[s,t,1] | 
        // ((Q_TD[s,t,2]-Q_TD[s,t,1])*beta_1_MF[s,shark[s,t]+1]) + ((Q_MB[s,t,2]-Q_MB[s,t,1])*beta_1_MB[s,shark[s,t]+1])+(pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
        // 
        (Q_net[s,t,2]-Q_net[s,t,1])*beta_1[s,shark[s,t]+1] + (pers[s]*prev_choice) + (Mo_pers[s]*prev_motor)
        
        );
        } else {log_lik[s,t,1] = 0;}
        prev_choice = (2*choice[s,t,1])-1; //1 if choice 2, -1 if choice 1
        prev_motor = motorchoice[s,t,1];
        
        if (missing_choice[s,t,2]==0) {
          if(skip_choice[s,t,2]==0) {
          log_lik[s,t,2] = bernoulli_logit_lpmf(choice[s,t,2] | ((Q_2[s,t,state_2[s,t],2]-Q_2[s,t,state_2[s,t],1])*beta_2[s]));
          } else {log_lik[s,t,2] = 0;}
          //use if not missing 2nd stage reward
          //if (missing_reward[s,t]==0) {
            //prediction errors
             //note: choices are 0/1, +1 to make them 1/2 for indexing
             delta_1[s,t] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1]/alpha[s,shark[s,t]+1] - Q_TD[s,t,choice[s,t,1]+1]; 
             delta_2[s,t] = reward[s,t]/alpha[s,shark[s,t]+1] - Q_2[s,t,state_2[s,t],choice[s,t,2]+1];
             
             //update transition counts: if choice=0 & state=1, or choice=1 & state=2, update 1st
             // expectation of transition, otherwise update 2nd expectation
             //tran_count = (state_2[s,t]-choice[s,t,1]-1) ? 1 : 2;
             //tran_type[tran_count] = tran_type[tran_count] + 1;
             
             //update chosen values
             //Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + alpha[s,shark[s,t]+1]*(delta_1+lambda[s]*delta_2);
             Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + (alpha[s,shark[s,t]+1]*delta_1[s,t])+(1*delta_2[s,t]);
             Q_2[s,t+1,state_2[s,t],choice[s,t,2]+1] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1] + alpha[s,shark[s,t]+1]*delta_2[s,t];
            
             
             //update unchosen TD & second stage values
             Q_TD[s,t+1,(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_TD[s,t,(choice[s,t,1] ? 1 : 2)];
             Q_2[s,t+1,state_2[s,t],(choice[s,t,2] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,state_2[s,t],(choice[s,t,2] ? 1 : 2)];
             unc_state = (state_2[s,t]-1) ? 1 : 2;
             Q_2[s,t+1,unc_state,1] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,unc_state,1];
             Q_2[s,t+1,unc_state,2] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,unc_state,2];
             
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
          delta_1[s,t] = Q_2[s,t,state_2[s,t],choice[s,t,2]+1]-Q_TD[s,t,choice[s,t,1]+1]; 
          delta_2[s,t] = -998;
          Q_TD[s,t+1,choice[s,t,1]+1] = Q_TD[s,t,choice[s,t,1]+1] + alpha[s,shark[s,t]+1]*delta_1[s,t];
          Q_TD[s,t+1,(choice[s,t,1] ? 1 : 2)] = (1-alpha[s,shark[s,t]+1])*Q_TD[s,t,(choice[s,t,1] ? 1 : 2)];
          Q_2[s,t+1,1,1] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,1,1];
          Q_2[s,t+1,1,2] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,1,2];
          Q_2[s,t+1,2,1] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,2,1];
          Q_2[s,t+1,2,2] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,2,2];
          //MB update of first stage values based on second stage values, so don't change //But that's not true because second stage value decayed ;_;
          alpha_b[s,t+1] = alpha_b[s,t];
          beta_b[s,t+1] = beta_b[s,t];
         
        }
      } else { //if missing 1st stage choice: decay all TD & 2nd stage values
      log_lik[s,t,1] = 0;
      log_lik[s,t,2] = 0;
      Q_TD[s,t+1,1] = (1-alpha[s,shark[s,t]+1])*Q_TD[s,t,1];
      Q_TD[s,t+1,2] = (1-alpha[s,shark[s,t]+1])*Q_TD[s,t,2];
      Q_2[s,t+1,1,1] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,1,1];
      Q_2[s,t+1,1,2] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,1,2];
      Q_2[s,t+1,2,1] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,2,1];
      Q_2[s,t+1,2,2] = (1-alpha[s,shark[s,t]+1])*Q_2[s,t,2,2];
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


