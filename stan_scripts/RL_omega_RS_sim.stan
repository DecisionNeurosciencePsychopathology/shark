data {
  int<lower=1> nT;
  int<lower=1> nS;
  int<lower=0,upper=1> choice[nS,nT,2]; 
  int<lower=0,upper=1> reward[nS,nT];
  int<lower=1,upper=2> state_2[nS,nT]; 
}

parameters {

  real alpha_m;
  real beta_1_m;
  real beta_2_m;
  real lamda_m;
  real omega_m;
  real rs_m;
  real pers_m;
  
  real<lower=0> alpha_s;
  real<lower=0> beta_1_s;
  real<lower=0> beta_2_s;
  real<lower=0> lamda_s;
  real<lower=0> omega_s;
  real<lower=0> rs_s;
  real<lower=0> pers_s;
  
  vector[nS] alpha_subj;
  vector[nS] beta_1_subj;
  vector[nS] beta_2_subj;
  vector[nS] lamda_subj;
  vector[nS] omega_subj;
  vector[nS] rs_subj;
  vector[nS] pers_subj;
}

transformed parameters {

  //define transformed parameters
  vector[nS] alpha_normal;
  vector[nS] beta_1_normal;
  vector[nS] beta_2_normal;
  vector[nS] lamda_normal;
  vector[nS] omega_normal;
  vector[nS] rs_normal;
  vector[nS] pers_normal;
  
  vector[nS] alpha;
  vector[nS] beta_1;
  vector[nS] beta_2;
  vector[nS] lamda;
  vector[nS] omega;
  vector[nS] rs;
  vector[nS] pers;
  
  for (s in 1:nS) {
    alpha_normal = alpha_m + (alpha_s * alpha_subj);
    beta_1_normal = beta_1_m + (beta_1_s * beta_1_subj);
    beta_2_normal = beta_2_m + (beta_2_s * beta_2_subj);
    lamda_normal = lamda_m + (lamda_s * lamda_subj);
    omega_normal = omega_m + (omega_s * omega_subj);
    rs_normal = rs_m + (rs_s * rs_subj);
    pers_normal = pers_m + (pers_s * pers_subj);
  }
  
  alpha = inv_logit(alpha_normal);
  omega = inv_logit(omega_normal);
  lamda = inv_logit(lamda_normal);
  rs = inv_logit(rs_normal);
  beta_1 = beta_1_normal;
  beta_2 = beta_2_normal;
  pers = pers_normal;
}


model {
  //define variables
  int prev_choice;
  
  
  int px_choice;
  
  //int tran_count;
  //int tran_type[2];
  int unc_state;
  real delta_1;
  real delta_2;
  real Q_TD[2];
  real Q_MB[2];
  real Q_S1[2];
  real Q_2[2,2];
  real alpha_b; real beta_b;real mu_b;
  
  
  // // //Z scores
  // alpha ~ normal(0,1);
  // beta ~ normal(0,1);
  // omega ~ normal(0,5);
  // lamda ~ normal(0,5);
  // rs ~ normal(0,5);
  // 
  // alpha_sd ~ cauchy(0,1);
  // beta_1_sd ~ cauchy(0,1);
  // beta_2_sd ~ cauchy(0,1);
  // omega_sd ~ cauchy(0,1);
  // lamda_sd ~ cauchy(0,1);
  // rs_sd ~ cauchy(0,1);
  // 
  // alpha_subj ~ normal(0,10);
  // beta_1_subj ~ normal(0,10);
  // beta_2_subj ~ normal(0,10);
  // omega_subj ~ normal(0,10);
  // lamda_subj ~ normal(0,10);
  // rs_subj ~ normal(0,10);
  // 
  // 
  // pers ~ normal(0,10);

  for (s in 1:nS) {
    
   
    
    //set initial values
    for (i in 1:2) {
      Q_TD[i]=.5;
      //Q_MB[i]=.5;
      Q_2[1,i]=.5;
      Q_2[2,i]=.5;
      // tran_type[i]=0;
    }
    
    prev_choice=0;
    alpha_b=1;
    beta_b=1;
    
    for (t in 1:nT) {
      //use if not missing 1st stage choice
      if(prev_choice == choice [s,t,1]) {px_choice=1;} else if (prev_choice==0) {px_choice=0;} else {px_choice= -1;}
      
      //Use Beta distribution instead of fix value for trans p; Let's update it before hand;
      mu_b = ((alpha_b) / (alpha_b+beta_b));
      Q_MB[1] =  ((mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (1-mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
      Q_MB[2] =  ((1-mu_b)*fmax(Q_2[1,1],Q_2[1,2]) + (mu_b)*fmax(Q_2[2,1],Q_2[2,2]));
      
      Q_S1[1] = omega[s] * Q_MB[1] + (1-omega[s]) * Q_TD[1] + (pers[s]*prev_choice);
      Q_S1[2] = omega[s] * Q_MB[2] + (1-omega[s]) * Q_TD[2] + (pers[s]*prev_choice);
      
      choice[s,t,1] ~ bernoulli_logit(  (Q_S1[2]-Q_S1[1])*beta_1[s] );
      
      
      prev_choice = choice[s,t,1]; //choice is 1 and 0; 1 should be 1 and 0 should be -1
      
      choice[s,t,2] ~ bernoulli_logit(  (Q_2[state_2[s,t],2]-Q_2[state_2[s,t],1]) *beta_2[s] );
      
      delta_1 = Q_2[state_2[s,t],choice[s,t,2]+1]-Q_TD[choice[s,t,1]+1]; 
      delta_2 = rs[s]*reward[s,t] - Q_2[state_2[s,t],choice[s,t,2]+1];
      
      Q_TD[choice[s,t,1]+1] = Q_TD[choice[s,t,1]+1] + (alpha[s]*delta_1) + (lamda[s]*alpha[s]*delta_2);
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
      
    }
  }
}


