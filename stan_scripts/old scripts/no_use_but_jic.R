real<lower=0> alpha_m_diff_dep;
real<lower=0> alpha_m_diff_ide;
real<lower=0> alpha_m_diff_att;

real<lower=0> beta_1_MF_m_diff_dep;
real<lower=0> beta_1_MF_m_diff_ide;
real<lower=0> beta_1_MF_m_diff_att;

real<lower=0> beta_1_MB_m_diff_dep;
real<lower=0> beta_1_MB_m_diff_ide;
real<lower=0> beta_1_MB_m_diff_att;

real<lower=0> beta_2_m_diff_dep;
real<lower=0> beta_2_m_diff_ide;
real<lower=0> beta_2_m_diff_att;

vector[2] alpha_m;
vector[2] beta_1_MF_m;
vector[2] beta_1_MB_m;
vector[2] beta_2_m;

vector[nS] alpha_m_diff_dep;
vector[nS] alpha_m_diff_ide;
vector[nS] alpha_m_diff_att;

vector[nS] beta_1_MF_m_diff_dep;
vector[nS] beta_1_MF_m_diff_ide;
vector[nS] beta_1_MF_m_diff_att;

vector[nS] beta_1_MB_m_diff_dep;
vector[nS] beta_1_MB_m_diff_ide;
vector[nS] beta_1_MB_m_diff_att;

vector[nS] beta_2_m_diff_dep;
vector[nS] beta_2_m_diff_ide;
vector[nS] beta_2_m_diff_att;


alpha_m_diff_dep=grp_dep*alpha_m_diff_dep_raw
alpha_m_diff_ide=grp_dep*alpha_m_diff_dep_raw
alpha_m_diff_att=grp_dep*alpha_m_diff_dep_raw

beta_1_MF_m_diff_dep=grp_dep*alpha_m_diff_dep_raw
beta_1_MF_m_diff_ide=grp_dep*alpha_m_diff_dep_raw
beta_1_MF_m_diff_att=grp_dep*alpha_m_diff_dep_raw

beta_1_MB_m_diff_dep=grp_dep*alpha_m_diff_dep_raw
beta_1_MB_m_diff_ide=grp_dep*alpha_m_diff_dep_raw
beta_1_MB_m_diff_att=grp_dep*alpha_m_diff_dep_raw

beta_2_m_diff_dep=grp_dep*alpha_m_diff_dep_raw
beta_2_m_diff_ide=grp_dep*alpha_m_diff_dep_raw
beta_2_m_diff_att=grp_dep*alpha_m_diff_dep_raw


alpha_m_diff_dep;
alpha_m_diff_ide;
alpha_m_diff_att;

beta_1_MF_m_diff_dep;
beta_1_MF_m_diff_ide;
beta_1_MF_m_diff_att;

beta_1_MB_m_diff_dep;
beta_1_MB_m_diff_ide;
beta_1_MB_m_diff_att;

beta_2_m_diff_dep;
beta_2_m_diff_ide;
beta_2_m_diff_att;