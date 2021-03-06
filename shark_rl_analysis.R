#Shark RL Analysis

#Double check to make sure the code isn't doing anything funky:


tx<-shark_stan_prep(vb_sp)




#For model evidence: We calculate the WAIC:

get_summary_df(output_ls = RL_rgLR_betadist_mp_omega_SH_Grp_allex,pars = c("omega_m"),returnas = "data.frame",probs = 0.5)


sum<-summary(RL_rgLR_betadist_mp_omega_SH_Grp_allex$stanfit,pars="omega_m",probs=0.5)

xr<-extract(ottojcmod_l1_betadist_mp_ubeta2_HC$stanfit_ottojcmod_l1_betadist_mp_ubeta2_HC)

sapply(c(1,2,4,5),function(jx){mean(txjk[txjk$Group==jx,c("omega")])})

get_summary_df(output_ls = otto_jc_l1_betadist_mp_ubeta2indiv_all,pars = c("beta_1_MB","beta_2","beta_1_MF","omega"),returnas = "data.frame",probs = 0.5)->txjk
shark_stan_HC<-shark_stan_prep(shark_split_HC)
get_summary_df(output_ls = SH_otto_l1_betadist_mp_ubeta2_HC,pars = c("beta_1_MB_normal","beta_2_normal","beta_1_MF_normal"),returnas = "data.frame",probs = 0.5)
get_summary_df(output_ls = otto_jc_l1_betadist_mp_ubeta2_HC,pars = c("beta_1_MB","beta_2","beta_1_MF","pers","Mo_pers","omega"),returnas = "data.frame",probs = 0.5)

ubeta_HC<-shark_get_log_lik(stan_fitoutput = otto_jc_l1_betadist_mp_ubeta2_HC$stanfit_otto_jc_l1_betadist_mp_ubeta2_HC)
ubetaIndivi_HC<-shark_get_log_lik(stan_fitoutput = otto_jc_l1_betadist_mp_ubeta2indiv_HC$stanfit_otto_jc_l1_betadist_mp_ubeta2indiv_HC)
spbeta_nm_HC<-shark_get_log_lik(stan_fitoutput = SH_otto_l1_betadist_mp_HC$stanfit_SH_otto_l1_betadist_mp_HC)

lx<-data.frame(choice=shark_stan_HC$choice[12,,a1],
               ub_hc=ubeta_HC[[12]]$stage1_loglik,
               ubi_hc=ubetaIndivi_HC[[12]]$stage1_loglik)

omegas<-get_summary_df(output_ls = otto_jc2_l1_betadist_mp_ubeta2_all,pars = c("omega","omega_normal"),returnas = "data.frame",probs = 0.5)

dt<-otto_l1_betadist_mp_HC$data_list
cor(apply(ubeta$beta_1_MB,2,mean),apply(spbeta$beta_1_MB,2,mean))



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

mean_Q_2<-apply(stanfit_ext$Q_2,c(2,3,4,5),mean)



trg<-do.call(rbind,lapply(1:2,function(mx){
  do.call(rbind,lapply(1:2,function(rx){
     or<-as.data.frame(t(mean_Q_2[,1:100,rx,mx]))
     names(or)<-data_list$ID
     or$Trial<-1:100
     ora<-reshape2::melt(or,c("Trial"))
     names(ora)<-c("Trial","ID",paste0("Q_2"))
     ora$pl<-mx
     ora$stage<-rx
     return(ora)
   })
  )
})
)
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
stan_fit_oj<-otto_jc2_l1_betadist_mp_ubeta2_all$stanfit_otto_jc2_l1_betadist_mp_ubeta2_all
stan_fit_oj<-otto_jc_l1_betadist_mp_spBeta_HC$stanfit_otto_jc_l1_betadist_mp_spBeta_HC
data_list<-ottojcmod_l1_betadist_mp_ubeta2_HC$data_list
pars <- c("alpha_m","alpha_s","beta_1_m","beta_1_s",
          "omega_m","omega_s","beta_2_m","beta_2_s")

pars <- c("alpha_m","beta_1_MF_m","beta_1_MB_m",
          "beta_2_m","pers_m")
plot(stan_fit_oj, pars = pars,
     ask = TRUE, exact_match = TRUE, newpage = TRUE, plot = TRUE)

pairs(stan_fit_oj,pars)
stanfit_ext<-extract(stan_fit_oj)

log_lik_x<-stanfit_ext$log_lik

mean_loglike<-apply(exp(log_lik_x),c(2,3,4),mean)
median_loglike<-apply(exp(log_lik_x),c(2,3,4),median)
mean_Q_TD<-apply(stanfit_ext$Q_TD,c(2,3,4),mean)
mean_Q_MB<-apply(stanfit_ext$Q_MB,c(2,3,4),mean)
mean_Q_2<-apply(stanfit_ext$Q_2,c(2,3,4),mean)



log_lik_df<-do.call(rbind,lapply(1:dim(log_lik_x)[2],function(subj){
  state=data_list$state_2[subj,]
  data.frame(u_loglik_1=mean_loglike[subj,,1],
             u_loglik_2=mean_loglike[subj,,2],
             med_loglik_1=median_loglike[subj,,1],
             med_loglik_2=median_loglike[subj,,2],
             Q_TD_1=mean_Q_TD[subj,1:100,1],
             Q_TD_2=mean_Q_TD[subj,1:100,2],
             Q_MB_1=mean_Q_MB[subj,1:100,1],
             Q_MB_2=mean_Q_MB[subj,1:100,2],
             Q_2_1=sapply(1:100,function(t){mean_Q_2[subj,t,state[t],1]}),
             Q_2_2=sapply(1:100,function(t){mean_Q_2[subj,t,state[t],2]}),
             delta_2=mean_delta_2[subj,1:100],
             ID=data_list$ID[subj],
             choice_1=data_list$choice[subj,,1],
             choice_2=data_list$choice[subj,,2],
             reward=data_list$reward[subj,],
             state=state,
             miss_1=data_list$missing_choice[subj,,1],
             beta_2=apply(stanfit_ext$beta_2,c(2),mean)[subj],
             Trial=1:100)
}))


log_lik_df[log_lik_df==-999]<-NA
lik_sp<-split(log_lik_df,log_lik_df$ID)


sapply(lik_sp,function(gx){
  cor(gx$u_loglik_1,gx$choice_1)
  #cor(gx$u_loglik_2,gx$choice_2)
})

dfx<-lik_sp$`220104`
dfx<-lik_sp$`212020`
dfx<-lik_sp$`213163`

subj<-22
cbind(mean_Q_2[subj,1:100,1,],mean_Q_2[subj,1:100,2,],mean_delta_2[subj,1:100],data_list$choice[subj,1:100,2],data_list$reward[subj,],data_list$state_2[subj,1:100],data_list$skip_choice[subj,1:100,2])
IDx<-data_list$ID[subj]
ggplot(lik_sp[[IDx]],aes(Trial,u_loglik_2))+
  geom_point(aes(Trial,inv_logit((Q_2_2-Q_2_1)*beta_2))) + geom_point(aes(Trial,delta_2,color=as.factor(choice_2)))+
  geom_line(aes(Trial,Q_2_1,color="Q_1")) + geom_line(aes(Trial,Q_2_2,color="Q_2"))+ 
  geom_point(aes(Trial,choice_2,color=as.factor(choice_1==1),size=reward/2)) + facet_wrap(~state)


ggplot(lik_sp[["202200"]],aes(Trial,u_loglik_2))+geom_point(aes(Trial,u_loglik_2,color="loglik"))+geom_point(aes(Trial,choice_2,color=pl)) + facet_wrap(~as.factor(state))


for (grp in c("1","2")){
  shark_split_grp<-shark_split_all[sapply(shark_split_all, function(dfx){unique(dfx$GROUP1245)==grp})]
  run_shark_stan(data_list=shark_stan_prep(shark_split_grp),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ogbeta.stan',add_data = list(factorizedecay=0),
                 modelname=paste0("otto_jc_l1_betadist_mp_ogbeta_",grp),stan_args="default",assignresult=T,iter = 4000,forcererun = F,
                 savepath="stan_scripts/stan_output",open_shinystan=F)
}

for (grp in c("4","5")){
  shark_split_grp<-shark_split_all[sapply(shark_split_all, function(dfx){unique(dfx$GROUP1245)==grp})]
  run_shark_stan(data_list=shark_stan_prep(shark_split_grp),stanfile='stan_scripts/otto_jc_l1_betadist_mp_ogbeta.stan',add_data = list(factorizedecay=0),
                 modelname=paste0("otto_jc_l1_betadist_mp_ogbeta_",grp),stan_args="default",assignresult=T,iter = 4000,forcererun = F,
                 savepath="stan_scripts/stan_output",open_shinystan=F)
}


allgrps<-lapply(c("1","2","4","5"), function(grp){
  curstan<-extract(get(paste0("otto_jc_l1_betadist_mp_ogbeta_",grp))[[paste0("stanfit_otto_jc_l1_betadist_mp_ogbeta_",grp)]])
  list(extracted=curstan,
       beta_1_MB_df=data.frame(value=as.vector(curstan$beta_1_MB),
                               grp=grp))
})


ggplot(beta_1_MB_df,aes(x=inv_logit(value)))+geom_histogram(bins = 30)+facet_wrap(~grp)
#+geom_line(aes(Trial,Q_TD_1,color="1"))+geom_line(aes(Trial,Q_TD_2,color="2"))

STANFITOUT<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$stanfit
DATALIST<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$data_list
RLXR<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$extracted_fit
pars <- c("alpha_m","beta_1_MF_m","beta_1_MB_m",
          "beta_2_m","pers_m","alpha_s","beta_1_MF_s","beta_1_MB_s",
          "beta_2_s","pers_s")
pairs(STANFITOUT,pars=c("alpha_m","alpha_grp","beta_1_m","omega_m","beta_1_s","omega_s","omega_grp","beta1_grp"))

#HCmean:




xz<-lapply(1:dim(RLXR$omega)[2],function(x){
  xt<-as.data.frame(RLXR$omega[,x,])
  names(xt)<-c("omega","omega_SH")
  do.call(rbind,lapply(names(xt),function(zt){
    rxt<-data.frame(value=xt[[zt]],type=zt)
    rxt$ID<-x
    rxt$grp<-DATALIST$Group[x]
    return(rxt)
  })
  )
})
xzd<-do.call(rbind,xz)
xzd$ID<-as.factor(xzd$ID)
xzd$grp<-as.factor(xzd$grp)
ggplot(xzd,aes(x=grp, y=value,color=type))+geom_violin()







xz<-lapply(1:dim(RLXR$omega_normal)[2],function(x){
  xt<-as.data.frame(RLXR$omega_normal[,x,])
  names(xt)<-c("omega_normal","omega_normal_SH")
  do.call(rbind,lapply(names(xt),function(zt){
    rxt<-data.frame(value=xt[[zt]],type=zt)
    rxt$ID<-x
    rxt$grp<-RL_betadist_mp_omega_SH_Grp_ninezero$data_list$Group[x]
    return(rxt)
  })
  )
})
xzd<-do.call(rbind,xz)

xzd$ID<-as.factor(xzd$ID)
xzd$grp<-as.factor(xzd$grp)
xzd$type<-as.factor(xzd$type)
xzd<-xzd[order(xzd$type),]
ggplot(xzd[xzd$value<10 & xzd$value> -10,] ,aes(x=grp, y = value,color=type))+geom_boxplot()

jrx<-data.frame(sh_value=apply(RLXR$b1MB_sh,2,mean),grp=RL_betadist_mp_SH_Grp$data_list$Group)
jrx$grp<-as.factor(jrx$grp)

ggplot(data.frame(MF_SH=apply(RLXR$b1MF_sh,2,median),MB_SH=apply(RLXR$b1MB_sh,2,median),grp=as.factor(RL_betadist_mp_SH_Grp$data_list$Group)),
       aes(x=MF_SH,y=MB_SH,color=grp)) + geom_point()
########################################Do extract 
#Get the values 
lout<-stan_ex_clean(dout = RL_rgLR_betadist_mp_omega_SH_Grp_allex$extracted_fit,FUNCX = median,numcores = 10)

#fitxr <- stan(file="stan_scripts/RL_betadist_mp_omega_SH_Grp.stan",data = RL_betadist_mp_omega_SH_Grp_allex$data_list,iter=1, chains=1,seed=424546151,init = list(chain1=lout), algorithm="Fixed_param")
data_list<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$data_list

parstoget<-c("RPE_MB","RPE_MF","RPE_diff","delta_2","delta_1")
xr<-lapply(RL_rgLR_betadist_mp_omega_SH_Grp_allex$extracted_fit[parstoget],stan_ex_clean)
shark_rl_df<-lapply(1:data_list$nS,function(sx){
  rox<-lapply(parstoget,function(parx){
      xr[[parx]][sx,]
  })
  names(rox)<-parstoget
  roxj<-data.frame(rox)
  roxj$ID<-data_list$ID[sx]
  roxj$Trial<-1:data_list$nT
  return(roxj)
})
names(shark_rl_df)<-sapply(shark_rl_df,function(kr){unique(kr$ID)})
save(shark_rl_df,file = "shark_rl_df.rdata")

subnum<-2
#Q2_ch<-lout$Q_2[2,,,]
cbind(lout$Q_TD[2,1:100,],data_list$choice[2,,1],round(lout$delta_1[2,],2),sapply(1:data_list$nT,function(t){
  lout$Q_2[subnum,t,data_list$state_2[subnum,t],data_list$choice[subnum,t,2]+1]
}),round(lout$delta_2[2,],2)
)


lexRL<-exRL[c("Q_TD","delta_1","delta_2","Q_2")]
lout<-lapply(lexRL,stan_ex_clean)

subnum<-2
dfx<-data.frame(cbind(lout$Q_TD[subnum,1:100,],(data_list$choice[subnum,,1]+1),data_list$state_2[subnum,],(data_list$choice[subnum,,2]+1),round(lout$delta_1[subnum,],2),sapply(1:data_list$nT,function(t){
  lout$Q_2[subnum,t,data_list$state_2[subnum,t],data_list$choice[subnum,t,2]+1]
}),round(lout$delta_2[subnum,],2)
))
names(dfx)<-c("Q_TD_1","Q_TD_2","Spaceship","Planet","Alien","delta1","Q2_chosen","delta2")
dfx$Alien<-dfx$Alien + (2 * (dfx$Planet-1) )
dfx$trial <- 1:nrow(dfx)
#dfx$alpha<-alpha_all[subnum,1]
dfx$ifCommon <- dfx$Spaceship == dfx$Planet
ggplot(dfx) + geom_line(aes(trial,Q_TD_1, color = 'Spaceship1')) + geom_line(aes(trial,Q_TD_2, color = 'Spaceship2')) + geom_point(aes(trial,(2-Spaceship))) + geom_line(aes(trial,Q2_chosen, lty = as.factor(Alien)))


omegas<-extract(stanfit_RL_betadist_mp_omega_SH_Grp_allex,pars=c("omega_grp","omega_m","omega_sh_m","omega_sh_grp"))




para="alpha"
dxr=c("_grp","_m","_sh_m","_sh_grp")
out=extract(stanfit_RL_betadist_mp_omega_SH_Grp_allex,pars=paste(para,dxr,sep = ""))

for(sepx in c("","_sh")){
  print(sepx)
  print(0)
  basex=out[[paste0(para,sepx,"_m")]]
  print(summary((basex)))
  #print(get_p_sample(x = 0,sample_l = inv_logit(basex)))
  for(xi in 1:3){
    print(xi)
    tarx=(  out[[paste0(para,sepx,"_grp")]][,xi] )
    print(summary(tarx))
    #print(get_p_sample(x = 0,sample_l = tarx))
  }
}


#We don't have time now we will do it by hand
pars<-c("omega","alpha","beta_1")

#Get all the HC mean:
tx<-c("_m","HC mean")
HC_mean<-lapply(pars, function(x){
  data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_m")]])),
             type=tx[2],paratype=x,Group="HC",stringsAsFactors = F)
})

tx<-c("_grp","Group Mean")
Grp_mean<-lapply(pars, function(x){
  if(x=="beta_1"){y<-"beta1"}else{y<-x}
  do.call(rbind,
  lapply(1:3,function(ix){
    data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_m")]])) + as.numeric(as.character(RLXR[[paste0(y,"_grp")]][,ix])),Group=ix,
               type=paste0(ix,"_",tx[2]),paratype=x,stringsAsFactors = F)  
  })
  )
})



allmeans<-do.call(rbind,c(HC_mean,Grp_mean))
allmeans$Condi<-"Baseline"
ggplot(allmeans,aes(x=paratype,y=value,fill=Group,color=Group))+geom_violin()+ theme_bw()
###########################

xj<-summary(RL_rgLR_betadist_mp_omega_SH_Grp_allex$stanfit,pars=c("omega_normal"),probs=0.95)

dim(RLXR$omega_normal)
omega_out<-as.data.frame(apply(RLXR$omega_normal,c(2,3),median),stringsAsFactors = F)
omega_out$Group<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$data_list$Group
omega_out_m<-reshape2::melt(data = omega_out,id.vars=c("Group"))
ggplot(omega_out_m,aes(Group,value,color=variable))+geom_violin()

tx<-c("_sh_m","SH HC diff mean")
HC_sh_diff<-lapply(pars, function(x){
  if(x=="beta_1"){x<-"beta1"}
  data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_sh_m")]])),
             type=tx[2],paratype=x,Group="HC",stringsAsFactors = F)
})

tx<-c("_sh_grp","SH Group diff mean")
Grp_sh_diff<-lapply(pars, function(x){
  if(x=="beta_1"){x<-"beta1"}
  do.call(rbind,
          lapply(1:3,function(ix){
            data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_sh_m")]])) + as.numeric(as.character(RLXR[[paste0(x,"_sh_grp")]][,ix])),Group=ix,
                       type=paste0(ix,"_",tx[2]),paratype=x,stringsAsFactors = F)  
          })
  )
})
allSHdiff<-do.call(rbind,c(HC_sh_diff,Grp_sh_diff))

ggplot(allSH,aes(x=paratype,y=value,fill=Group,color=Group))+geom_violin()+ theme_bw()

############################

tx<-c("_sh_m","SH HC mean")
HC_SH_ALL<-lapply(pars, function(x){
  if(x=="beta_1"){y<-"beta1"}else{y<-x}
  data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_m")]])) + as.numeric(as.character(RLXR[[paste0(y,tx[1])]])),
             type=tx[2],paratype=x,Group="HC",stringsAsFactors = F)
})

tx<-c("_sh_grp","SH Group Mean")
SH_Grp_ALL<-lapply(pars, function(x){
  if(x=="beta_1"){y<-"beta1"}else{y<-x}
  do.call(rbind,
          lapply(1:3,function(ix){
            data.frame(value=as.numeric(as.character(RLXR[[paste0(x,"_m")]])) + as.numeric(as.character(RLXR[[paste0(y,"_sh_m")]])) + as.numeric(as.character(RLXR[[paste0(y,tx[1])]][,ix])) + as.numeric(as.character(RLXR[[paste0(y,"_grp")]][,ix])),Group=ix,
                       type=paste0(ix,"_",tx[2]),paratype=x,stringsAsFactors = F)  
          })
  )
})
allTOTAL<-do.call(rbind,c(HC_SH_ALL,SH_Grp_ALL))

ggplot(allTOTAL,aes(x=paratype,y=value,fill=Group,color=Group))+geom_violin()+ theme_bw()

###################################

tx<-c("_grp","Group Difference")

Grp_diff<-lapply(pars, function(x){
  if(x=="beta_1"){x<-"beta1"}
  lapply(1:3,function(ix){
    data.frame(value=as.numeric(as.character(RLXR[[paste0(x,tx[1])]][,ix])),Group=ix,
               type=paste0(ix,"_",tx[2]),paratype=x,stringsAsFactors = F)  
  })
})



tx<-c("_grp","Group Difference")

Grp_diff<-lapply(pars, function(x){
  if(x=="beta_1"){x<-"beta1"}
  lapply(1:3,function(ix){
    data.frame(value=as.numeric(as.character(RLXR[[paste0(x,tx[1])]][,ix])),Group=ix,
               type=paste0(ix,"_",tx[2]),paratype=x,stringsAsFactors = F)  
  })
})


RLXR<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$extracted_fit
DATALIST<-RL_rgLR_betadist_mp_omega_SH_Grp_allex$data_list
para<-"omega"
xra<-apply(RLXR[[paste0(para,"_normal")]],c(2,3),median)
dfa<-rbind(
data.frame(value=xra[,1],group=as.factor(DATALIST$Group),para="omega",type="baseline"),
data.frame(value=xra[,2],group=as.factor(DATALIST$Group),para="omega",type="threat")
)
ggplot(dfa,aes(group,value,fill=type))+geom_boxplot(color="plum4")+scale_fill_manual(values = c("darkgoldenrod2","darkolivegreen4"))
