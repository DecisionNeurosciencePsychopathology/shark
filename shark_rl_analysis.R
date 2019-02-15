#Shark RL Analysis

xr<-extract(ottojcmod_l1_betadist_mp_ubeta2_HC$stanfit_ottojcmod_l1_betadist_mp_ubeta2_HC)

sapply(c(1,2,4,5),function(jx){mean(txjk[txjk$Group==jx,c("omega")])})

get_summary_df(output_ls = otto_jc_l1_betadist_mp_ubeta2indiv_all,pars = c("beta_1_MB","beta_2","beta_1_MF","omega"),returnas = "data.frame",probs = 0.5)->txjk
shark_stan_HC<-shark_stan_prep(shark_split_HC)
get_summary_df(output_ls = SH_otto_l1_betadist_mp_ubeta2_HC,pars = c("beta_1_MB_normal","beta_2_normal","beta_1_MF_normal"),returnas = "data.frame",probs = 0.5)
get_summary_df(output_ls = otto_jc_l1_betadist_mp_ubeta2_HC,pars = c("beta_1_MB","beta_2","beta_1_MF","pers","Mo_pers","omega"),returnas = "data.frame",probs = 0.5)

ubeta_HC<-shark_get_log_lik(stan_fitoutput = otto_jc_l1_betadist_mp_ubeta2_HC$stanfit_otto_jc_l1_betadist_mp_ubeta2_HC)
ubetaIndivi_HC<-shark_get_log_lik(stan_fitoutput = otto_jc_l1_betadist_mp_ubeta2indiv_HC$stanfit_otto_jc_l1_betadist_mp_ubeta2indiv_HC)
spbeta_nm_HC<-shark_get_log_lik(stan_fitoutput = SH_otto_l1_betadist_mp_HC$stanfit_SH_otto_l1_betadist_mp_HC)

lx<-data.frame(choice=shark_stan_HC$choice[12,,1],
               ub_hc=ubeta_HC[[12]]$stage1_loglik,
               ubi_hc=ubetaIndivi_HC[[12]]$stage1_loglik)

omegas<-get_summary_df(output_ls = otto_jc2_l1_betadist_mp_ubeta2_all,pars = c("omega","omega_normal"),returnas = "data.frame",probs = 0.5)


everyone<-lapply(shark_split_HC,function(dfx){
  nsg<-list()
  for(i in 1:nrow(dfx)){
    if(i==1){
      alpha_b=1
      beta_b=1}
    
    if(!is.na(dfx$state[i]) & !is.na(dfx$choice1[i])){
      
      if (dfx$choice1[i]==1 & dfx$state[i]==2) {alpha_b = alpha_b + 1;}   
      if (dfx$choice1[i]==2 & dfx$state[i]==3) {alpha_b = alpha_b + 1;}
      
      if (dfx$choice1[i]==1 & dfx$state[i]==3) {beta_b = beta_b + 1;}   
      if (dfx$choice1[i]==2 & dfx$state[i]==2) {beta_b = beta_b + 1;}  
      
      mu_b = ((alpha_b) / (alpha_b+beta_b));
    } else {mu_b = NA}
    nsg[[i]]=data.frame(mu_b=mu_b,alpha_b=alpha_b,beta_b=beta_b)
  }
  do.call(rbind,nsg)
  
})

sx<-6
alpha_b<-apply(xr$alpha_b,c(2,3),mean)
beta_b<-apply(xr$beta_b,c(2,3),mean)
jc<-data.frame(a=alpha_b[sx,],b=beta_b[6,])
jc$muB<-jc$a / (jc$a + jc$b)
cbind(jc[2:101,],everyone[[6]])


xr<-extract(fitxr)
xr<-extract(otto_jc2_l1_betadist_mp_ubeta2_HC$stanfit_otto_jc2_l1_betadist_mp_ubeta2_HC)
#xr$Q_MB[1,5,,]
Q_MB_b<-apply(xr$Q_MB,c(2,3,4),mean)
all_Q_MB<-do.call(rbind,lapply(1:dim(Q_MB_b)[1],function(sx){
  dfx<-data.frame(S1=Q_MB_b[sx,,1],S2=Q_MB_b[sx,,2])
  dfx$trial<-1:nrow(dfx)
  m_dfx<-reshape2::melt(dfx,id.vars="trial")
  m_dfx$ID<-sx
  return(m_dfx)
})
)

ggplot(data = all_Q_MB,aes(x=trial,y=value,color=variable)) + geom_line() + facet_wrap(~ID)

apply(ox$Q_TD,dim(Q_TD)[2:length(dim(Q_TD))],mean)
all_Q_MB<-do.call(rbind,lapply(1:dim(Q_MB_a)[1],function(sx){
  dfx<-data.frame(S1=Q_MB_a[sx,,1],S2=Q_MB_a[sx,,2])
  dfx$trial<-1:nrow(dfx)
  m_dfx<-reshape2::melt(dfx,id.vars="trial")
  m_dfx$ID<-sx
  return(m_dfx)
})
)

ggplot(data = all_Q_MB,aes(x=trial,y=value,color=variable)) + geom_line() + facet_wrap(~ID)

Q_2_a<-apply(ox$Q_2,2:length(dim(ox$Q_2)),mean)

sx<-6
dfx<-data.frame(S1C1=Q_2_a[sx,,1,1],S1C2=Q_2_a[sx,,1,2],S2C1=Q_2_a[sx,,2,1],S2C2=Q_2_a[sx,,2,2])
dfx2<-data.frame(S1=apply(Q_2_a[sx,,1,],1,max),S2=apply(Q_2_a[sx,,2,],1,max))
dfx$trial<-1:nrow(dfx)
m_dfx<-reshape2::melt(dfx,id.vars="trial")
ggplot(data = m_dfx,aes(x=trial,y=value,color=variable)) + geom_line() 



ac<-shark_get_log_lik(stan_fitoutput = otto_jc2_l1_betadist_mp_ubeta2_HC$stanfit_otto_jc2_l1_betadist_mp_ubeta2_HC)
ab<-shark_stan_prep(shark_split_HC)

plot(ab$choice[4,,1],ac[[4]]$stage1_loglik)

####################


stan_fit_oj<-ottojcmod_l1_betadist_mp_ubeta2_HC$stanfit_ottojcmod_l1_betadist_mp_ubeta2_HC
data_list<-ottojcmod_l1_betadist_mp_ubeta2_HC$data_list
pars <- c("alpha_m","alpha_s","beta_1_m","beta_1_s",
          "omega_m","omega_s","beta_2_m","beta_2_s")
plot(stan_fit_oj, pars = pars,
     ask = TRUE, exact_match = TRUE, newpage = TRUE, plot = TRUE)

pairs(stan_fit_oj,pars)
stanfit_ext<-extract(stan_fit_oj)

log_lik_x<-stanfit_ext$log_lik

mean_loglike<-apply(exp(log_lik_x),c(2,3,4),mean)
median_loglike<-apply(exp(log_lik_x),c(2,3,4),median)
mean_Q_TD<-apply(stanfit_ext$Q_TD,c(2,3,4),mean)
mean_Q_MB<-apply(stanfit_ext$Q_MB,c(2,3,4),mean)

log_lik_df<-do.call(rbind,lapply(1:dim(log_lik_x)[2],function(subj){
  data.frame(u_loglik_1=mean_loglike[subj,,1],
             u_loglik_2=mean_loglike[subj,,2],
             med_loglik_1=median_loglike[subj,,1],
             med_loglik_2=median_loglike[subj,,2],
             Q_TD_1=mean_Q_TD[subj,1:100,1],
             Q_TD_2=mean_Q_TD[subj,1:100,2],
             Q_MB_1=mean_Q_MB[subj,1:100,1],
             Q_MB_2=mean_Q_MB[subj,1:100,2],
             ID=data_list$ID[subj],
             choice_1=data_list$choice[subj,,1],
             choice_2=data_list$choice[subj,,2],
             reward=data_list$reward[subj,],
             state=data_list$state_2[subj,],
             Trial=1:100)
}))
cor(log_lik_df$u_loglik_1,log_lik_df$choice_1)
cor(log_lik_df$u_loglik_2,log_lik_df$choice_2)

lik_sp<-split(log_lik_df,log_lik_df$ID)








