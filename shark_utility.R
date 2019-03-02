#Utlity functions:
library(dplyr)

getifswitched<-function(y) {
  y_lag<-dplyr::lag(y)
  return(!y==y_lag)
}

ProcApply<-function(listx=NULL,FUNC=NULL,...,addNAtoNull=T) {
  proc_ls<-lapply(X = listx,FUN = FUNC,... = ...)
  if(addNAtoNull){
    allnames<-unique(unlist(lapply(proc_ls,names)))
    proc_ls<-lapply(proc_ls,function(lsx){
      lsx[allnames[which(!allnames %in% names(lsx))]]<-NA
      return(lsx)
    })
  }
  return(list(list=proc_ls,
              df=do.call(rbind,proc_ls)))
}
findbox<-function() {
  if (Sys.getenv("USER")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box"
  } else if (Sys.getenv("USER")=="jiazhou") {boxdir <- "/Volumes/bek/Box Sync"} else {
    boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}
  return(boxdir)
}
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}

#Label probability function
lableVar<-function(dfx) {
  if (length(grep("if",names(dfx)))>0) {
    for (jx in grep("if*",names(dfx))) {
      as.logical(dfx[[jx]])->temp
      dfx[which(temp),jx]<-gsub("if","",names(dfx)[jx])
      dfx[which(!temp),jx]<-paste0("Not_",gsub("if","",names(dfx)[jx]))
    } }
  return(dfx)
}
#clean up list function
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}
#Generate probability function
genProbability<-function(dfx,condition=c("Context","Emotion"),response=c("FaceResponseText"),excludeNA=T,missresp=NA) {
  if (excludeNA) {
    if (is.na(missresp)) {
      dfx<-dfx[which(!is.na(dfx[[response]])),] } else {dfx<-dfx[which(dfx[[response]]!=missresp),]}
  }
  dfx<-droplevels(dfx)
  #whichone<-c(condition,response)
  interaction(dfx[condition])->interactions
  
  nwx<-do.call(rbind,lapply(attributes(as.factor(dfx[[response]]))$levels, function(resp) {
    prob<-data.frame(
      p=sapply(attributes(interactions)$levels, function(x) {
        ( length(which(as.character(dfx[[response]])==resp & interactions==x)) / length(which(interactions==x)) ) -> px
        return(px)
      }),
      resp=resp)
    for (n in 1:length(condition)) {
      prob[condition[n]]<-sapply(strsplit(rownames(prob),split = ".",fixed = T),"[[",n)
    }
    rownames(prob)<-NULL
    prob$ID<-unique(dfx$ID)
    lableVar(prob)
  }) )
  
  return(nwx)
}


vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

shark_proc<-function(dfx) {
  dfx$ID <- as.factor(dfx$ID)
  
  dfx$keycode1[dfx$keycode1==0]<-NA
  dfx$keycode2[dfx$keycode2==0]<-NA
  dfx$choice1[dfx$choice1==0]<-NA
  dfx$choice2[dfx$choice2==0]<-NA
  
  dfx$rocketLRswitch<- !dfx$keycode1 == dfx$choice1 #if rocket switched sides
  dfx$ChoiceKey<- paste(dfx$keycode1,dfx$keycode2,sep = "&")
  dfx$ChoiceKey[is.na(dfx$keycode1) | is.na(dfx$keycode2)]<-NA
  
  
  dfx$DecRecode<-NA
  dfx$DecRecode[dfx$state==2 & dfx$choice2==1]<-"P2L"
  dfx$DecRecode[dfx$state==2 & dfx$choice2==2]<-"P2R"
  dfx$DecRecode[dfx$state==3 & dfx$choice2==1]<-"P3L"
  dfx$DecRecode[dfx$state==3 & dfx$choice2==2]<-"P3R"
  
  dfx$ChoiceFree<- paste(dfx$choice1,dfx$DecRecode,sep = "&")
  dfx$ChoiceFree[is.na(dfx$choice1) | is.na(dfx$DecRecode)]<-NA
  
  dfx$ifRare <- (dfx$choice1+2)/dfx$choice1 == dfx$state #common FLASE; rare TRUE
  dfx$ifRare[is.na(dfx$choice1) | is.na(dfx$choice2)]<-NA
  dfx$Transition<-plyr::mapvalues(dfx$ifRare,from = c("TRUE","FALSE"),to = c("Rare","Common"))
  dfx$ifSwitched1 <- getifswitched(dfx$choice1)
  dfx$ifSwitched2 <- getifswitched(dfx$choice2)
  dfx$ifSwitchedKey<- getifswitched(dfx$ChoiceKey)
  dfx$ifSwitchedFree<-getifswitched(dfx$ChoiceFree)
  dfx$ifReinf <- as.factor(as.logical(dfx$won))
  dfx$ifReinf<-factor(dfx$ifReinf,levels=c("TRUE","FALSE"))
  dfx$RewardType<-plyr::mapvalues(dfx$ifReinf,from = c("TRUE","FALSE"),to = c("Reward","No Reward"))
  dfx$ifSharkBlock <- (dfx$contingency==1 & (dfx$trial %in% c(1:25,51:75))) | (dfx$contingency==2 & (dfx$trial %in% c(26:50,76:100))) 
  dfx$BlockType<-plyr::mapvalues(dfx$ifSharkBlock,from = c("TRUE","FALSE"),to = c("Shark","Baseline"))
  #To ensure sequential order before lag and lead
  dfx<-dfx[with(dfx,order(ID,trial)),]
  
  dfx$rts1_scale<-scale(dfx$rts1,center = T,scale = F)
  dfx$rts2_scale<-scale(dfx$rts2,center = T,scale = F)
  
  for (ix in c("choice1","BlockType","Transition",
               "choice2",
               "keycode1",
               "keycode2",
               "ifReinf",
               "state",
               "rts1","rts1_scale",
               "rts2","rts2_scale",
               "ifRare",
               "ChoiceFree",
               "ifSwitchedFree",
               "ifSwitchedKey",
               "ChoiceKey",
               "DecRecode"
  )) {
    d<-dfx[,ix]
    dfx[paste0(ix,"_lag")]<-lag(d)
    dfx[paste0(ix,"_lead")]<-lead(d)
  }
  
  dfx$ConMotor1<-as.factor(dfx$keycode1 == dfx$keycode2_lag)
  dfx$ConMotor2<-as.factor(dfx$keycode2 == dfx$keycode1)
  dfx$ConMotor1_lead<-as.factor(dfx$keycode1_lead == dfx$keycode2)
  dfx$ConMotor2_lead<-as.factor(dfx$keycode2_lead == dfx$keycode1_lead)
  dfx$Stay1 <- as.factor(dfx$choice1 == dfx$choice1_lag)
  dfx$Stay2 <- as.factor(dfx$choice2 == dfx$choice2_lag)
  dfx$Stay1_lead <- as.factor(dfx$choice1 == dfx$choice1_lead)
  dfx$Stay2_lead <- as.factor(dfx$choice2 == dfx$choice2_lead)
  dfx$SameKey1 <- as.factor(dfx$keycode1 == dfx$keycode1_lag)
  dfx$SameKey2 <- as.factor(dfx$keycode2 == dfx$keycode2_lag)
  dfx$SameKey1_lead <- as.factor(dfx$keycode1 == dfx$keycode1_lead)
  dfx$SameKey2_lead <- as.factor(dfx$keycode2 == dfx$keycode2_lead)
  dfx$SameState <- as.factor(dfx$state == dfx$state_lag)
  dfx$SameState_lead <- as.factor(dfx$state == dfx$state_lead)
  

  
  meh<-lapply(dfx,class)
  meh$trial<-NULL
  for (ik in names(meh)) {
    if (meh[[ik]] %in% c("logical","integer","character")) {
      dfx[,ik]<-as.factor(dfx[,ik])
    }
  }
  dfx$Block = ceiling(dfx$trial/25)
  dfx$Run = ceiling(dfx$trial/50)
  dfx$Missed <- dfx$rts1==0 | dfx$rts2==0
  dfx$Outlier <- dfx$rts1<.2 | dfx$rts2<.2 | dfx$rts1 > 4 | dfx$rts2 > 4
  return(dfx)
}

shark_exclusion<-function(dfx=NULL, missthres=NA,comreinfstaythres=NA,P_staycomreinfchance=NA,returnstats=F) {
  if(!is.na(missthres)){
  p_miss<-(length(which(is.na(dfx$choice1))) / length(dfx$choice1))
  p_miss_if<- p_miss < missthres
  } else {p_miss_if<-TRUE}
  
  if(!is.na(comreinfstaythres)){
    z<-genProbability(dfx,condition=c('ifReinf','ifRare'),response=c("ifSwitched1"))
    p_com_reinf_stay<-z$p[z$ifRare=="Not_Rare" & z$ifReinf=="Reinf" & z$resp=="FALSE"]
    if_com_reinf_stay <- p_com_reinf_stay > comreinfstaythres
  } else {if_com_reinf_stay<-TRUE}
  
 if(!is.na(P_staycomreinfchance)){
   #Get chance p of stay given common reinf, 
   p_com_reinf_stay_chance<-pbinom(length(which(!as.logical(dfx$ifRare) & as.logical(dfx$ifReinf) & as.logical(dfx$Stay1))), 
                                   length(which(!as.logical(dfx$ifRare) & as.logical(dfx$ifReinf))), 0.5)
   if_chance_comreinfstay <- p_com_reinf_stay_chance > P_staycomreinfchance
 }else{if_chance_comreinfstay<-TRUE}
  
  
 
  
  
  if(returnstats){data.frame(p_miss=p_miss,p_comreinfstay=p_com_reinf_stay,p_chancecomreinfstay=p_com_reinf_stay_chance)}else{
  if (p_miss_if && if_com_reinf_stay && if_chance_comreinfstay) {return(dfx)} else {return(NULL)}
  }
}

getdata<-function(scdate,xdata,limt=365,ignorelimt=F) {
  xdata$diff<-abs(as.Date(xdata$CDate)-as.Date(scdate))
  if (any(!is.na(xdata$diff))) {
    if (any(xdata$diff <= limt) | ignorelimt){
      xydata<-xdata[which(xdata$diff==min(xdata$diff)),-grep(paste('ID','CDate','diff',sep = "|"),names(xdata))]
      xydata$status<-TRUE
      return(xydata)
    } else {
      xdata[1,]<-NA
      xdata<-xdata[1,-grep(paste('ID','CDate','diff',sep = "|"),names(xdata))]
      xdata$status<-FALSE
      return(xdata)}
  } else {
    xdata[1,]<-NA
    xdata<-xdata[1,-grep(paste('ID','CDate','diff',sep = "|"),names(xdata))]
    xdata$status<-FALSE
    return(xdata)}
}

shark_subsummary<-function(shark_data_proc=shark_data_proc){
  jk<-lapply(shark_data_proc,function(dfx) {
    data.frame(ID=unique(dfx$ID)
    )
  })
}



shark_mat<-function(shark_mat) {
  varix<-c("choice1","rts1", "stim1.ons.sl","stim1.ons.ms","choice1.ons.sl","choice1.ons.ms","swap.hist","keycode1"
           ,"state","choice2","rts2","stim2.ons.sl","stim2.ons.ms","choice2.ons.sl","choice2.ons.ms","keycode2",
           "rew.ons.sl","rew.ons.ms") 
  shark_mat_df<-as.data.frame(lapply(shark_mat[varix],drop))
  shark_mat_df$sharkattack<-FALSE
  attack<-drop(shark_mat$attack)
  shark_mat_df$sharkattack[attack]<-TRUE
  shark_mat_df$contingency<-drop(shark_mat$contingency)
  shark_mat_df$ID<-drop(shark_mat$ID)
  shark_mat_df$won<-drop(shark_mat$money)
  jitter<-as.data.frame(shark_mat$jitter.time)
  names(jitter)<-c("jitter.time1","jitter.time2")
  shark_mat_df<-cbind(shark_mat_df,jitter)
  shark_mat_df$trial<-seq_along(shark_mat_df$choice1)
  return(shark_mat_df)
}

shark.ml.proc<-function(bdf=NULL,include.clinical=T,include.lag=T,include.neruopsy=T,include.demo=T,removevar=NULL){
  verybasic<-c("ID","trial","contingency","ifSharkBlock","sharkattack","Run","Block","rocketLRswitch")
  basevar<-c("choice1","rts1","keycode1","state","choice2","rts2","keycode2","ifReinf")
  demovar<-c("EXPLORE AGE","MAX LETHALITY","GENDER TEXT","ETHNICITY TEXT","RACE TEXT","EDUCATION","MARITAL TEXT")
  clinicalvar<-c("GROUP1245")
  neuropsyvar<-c("DRS.ATTENT","DRS.INITIAT","DRS.CONST","DRS.MEM","DRS.TOTA_MDRS","EXIT.EXITtot","WTAR.WTARRAW","WTAR.WTARSS")
  
  xvars<-c(verybasic,basevar)
  
  if (include.lag) {
    xvars<-c(xvars,paste(basevar,"lag",sep = "_"))
  }
  if (include.demo) {
    xvars<-c(xvars,demovar)
  }
  if (include.clinical) {
    xvars<-c(xvars,clinicalvar)
  }
  if (include.neruopsy) {
    xvars<-c(xvars,neuropsyvar)
  }
  
  if (!is.null(removevar)){
    xvars<-xvars[!xvars %in% removevar]
  }
  
  if(any(!xvars %in% names(bdf))){
    message(paste("These variable names are not in bdf, will take them out for now: ",xvars[!xvars %in% names(bdf)],sep = " ",collapse = " "))
    xvars<-xvars[xvars %in% names(bdf)]
  }
  
  return(list(data=bdf[xvars],
              base=c(verybasic,basevar)[c(verybasic,basevar) %in% names(bdf) & !c(verybasic,basevar) %in% removevar],
              demo=demovar[demovar %in% names(bdf) & !demovar %in% removevar],
              clinical=clinicalvar[clinicalvar %in% names(bdf) & !clinicalvar %in% removevar],
              neuropsy=neuropsyvar[neuropsyvar %in% names(bdf) & !neuropsyvar %in% removevar]))
  
}



shark_fsl<-function(dfx=NULL) {
  dfx$Trial<-as.numeric(unlist(lapply(split(dfx$trial,dfx$Run),seq_along)))
  #
  #Gen QC regressors:
  dfx$QC_OUT<-sample(1:0,length(dfx$Trial),replace = T)
  #Motor   #left is 1 and right is 2
  dfx$LeftRight1<-plyr::mapvalues(dfx$keycode1,from=(1:2),to=c(-1,1))
  dfx$LeftRight2<-plyr::mapvalues(dfx$keycode2,from=(1:2),to=c(-1,1))
  #Task Variables:
  dfx$SharkBlock<-as.numeric(as.character(plyr::mapvalues(dfx$ifSharkBlock,from=(c("TRUE","FALSE")),to=c(1,-1))))
  dfx$SharkBlock_lag<-dplyr::lag(dfx$SharkBlock)
  dfx$Tran<-as.numeric(as.character(plyr::mapvalues(dfx$ifRare,from=(c("TRUE","FALSE")),to=c(1,-1))))
  dfx$Tran_lag<-dplyr::lag(dfx$Tran)
  dfx$Reinf<-as.numeric(as.character(plyr::mapvalues(dfx$ifReinf,from=(c("TRUE","FALSE")),to=c(1,-1))))
  dfx$Reinf_lag<-dplyr::lag(dfx$Reinf)
  #Regressors:
  dfx$ModelBase<-dfx$Tran * dfx$Reinf
  dfx$ModelBase_lag<-dplyr::lag(dfx$ModelBase)
  
  dfx$ModelBaseShark<-dfx$Tran * dfx$Reinf * dfx$SharkBlock
  dfx$ModelBase_lagShark<-dfx$ModelBase_lag * dfx$SharkBlock
  
  dfx$RareNotReinf<-as.numeric(as.logical(dfx$ifRare) & as.logical(dfx$ifReinf))
  dfx$RareNotReinf_lag<-dplyr::lag(dfx$RareNotReinf)
  
  dfx_sp<-split(dfx,dfx$Block)
  shark_dfx<-do.call(rbind,cleanuplist(lapply(dfx_sp,function(spx) {
    sbj<-data.frame(onset=spx$stim1.ons.ms[1]/1000,
               duration=((spx$rew.ons.ms[length(spx$ID)]-spx$stim1.ons.ms[1])/1000),
               run=unique(spx$Run))
    
     if(unique(spx$SharkBlock)==1) {sbj$type<-1} else {sbj$type<-(-1)}
      return(sbj)
  })))
  shark_dfx$Trial<-rep(1:2,2)
  
  finalist<-list(Decision1=data.frame(event="Decision1",
                                      onset=dfx$stim1.ons.ms/1000,
                                      duration=dfx$rts1,
                                      run=dfx$Run,
                                      trial=dfx$Trial),
                 Decision2=data.frame(event="Decision2",
                                      onset=dfx$stim2.ons.ms/1000,
                                      duration=dfx$rts2,
                                      run=dfx$Run,
                                      trial=dfx$Trial),
                 Outcome=data.frame(event="Outcome",
                                     onset=dfx$rew.ons.ms/1000,
                                     duration=1.5,
                                     run=dfx$Run,
                                     trial=dfx$Trial),
                 SharkBlock=data.frame(event="SharkBlock",
                                     onset=shark_dfx$onset,
                                     duration=shark_dfx$duration,
                                     run=shark_dfx$run,
                                     trial=shark_dfx$Trial),
                 QC=data.frame(event="QC",
                               onset=dfx$stim1.ons.ms/1000,
                               duration=dfx$rts1,
                               run=dfx$Run,
                               trial=dfx$Trial)
  )
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  
  value<-as.list(dfx)
  value$SharkBlockValue<-shark_dfx$type
  output<-list(event.list=finalist,output.df=dfx,value=value)
  return(output)
}

shark_stan_prep<-function(shark_split=NULL){
  nS=length(shark_split)
  nT=max(sapply(shark_split,nrow))
  shark_stan<-list(
    nS=nS,nT=nT,
    grp_dep=array(0,dim=c(nS)),grp_ide=array(0,dim = c(nS)),grp_att=array(0,dim=c(nS)),
    choice=array(0,dim=c(nS,nT,2)),
    rt=array(0,dim=c(nS,nT,2)),
    motorchoice=array(0,dim=c(nS,nT,2)),
    state_2=array(0,dim=c(nS,nT)),
    shark=array(0,dim=c(nS,nT)), 
    reward=array(0,dim=c(nS,nT)),
    missing_choice=array(0,dim=c(nS,nT,2)),
    skip_choice=array(0,dim=c(nS,nT,2)),
    missing_reward=array(0,dim=c(nS,nT)),
    ID=array(0,dim=nS), Group=array(0,dim=nS)
  )
  for(s in 1:nS) {
    #message(s)
    dfx<-shark_split[[s]]
    shark_stan$ID[s]<-unique(as.character(dfx$ID))
    shark_stan$Group[s]<-unique(as.character(dfx$GROUP1245))
    shark_stan$grp_hc[s]=as.numeric(unique(as.character(dfx$GROUP1245))=="1")
    shark_stan$grp_dep[s]=as.numeric(unique(as.character(dfx$GROUP1245))=="2")
    shark_stan$grp_ide[s]=as.numeric(unique(as.character(dfx$GROUP1245))=="4")
    shark_stan$grp_att[s]=as.numeric(unique(as.character(dfx$GROUP1245))=="5")
    shark_stan$grp_clinical[s]=as.numeric(unique(as.character(dfx$GROUP1245))!="1")
    shark_stan$choice[s,1:nrow(dfx),1]<-as.numeric(dfx$choice1)-1 #Choice is 0 or 1
    shark_stan$choice[s,1:nrow(dfx),2]<-as.numeric(dfx$choice2)-1
    shark_stan$rt[s,1:nrow(dfx),1]<-(as.numeric(dfx$rts1)) 
    shark_stan$rt[s,1:nrow(dfx),2]<-(as.numeric(dfx$rts2)) 
    shark_stan$state_2[s,1:nrow(dfx)]<-dfx$state-1 #State is 1 or 2
    shark_stan$reward[s,1:nrow(dfx)]<-as.numeric(dfx$RewardType=="Reward") #Reward is 0 or 1
    shark_stan$shark[s,1:nrow(dfx)]<-as.numeric(dfx$ifSharkBlock==TRUE) #SharkBlock is 0 or 1
    shark_stan$motorchoice[s,1:nrow(dfx),1]<-as.numeric(ifelse(dfx$keycode1==1,1,-1)) 
    shark_stan$motorchoice[s,1:nrow(dfx),2]<-as.numeric(ifelse(dfx$keycode2==1,1,-1)) 
    #Deal with missing trials;
    miss_c1<-c(which(is.na(dfx$choice1) ),which(!1:nT %in% 1:nrow(dfx)) )
    miss_c2<-c(which(is.na(dfx$choice2) ),which(!1:nT %in% 1:nrow(dfx)) )
    
    skip_c1<-c(which(dfx$rts1 > 4 | dfx$rts1 < 0.2 | as.logical(dfx$sharkattack)),which(!1:nT %in% 1:nrow(dfx)))
    skip_c2<-c(which(dfx$rts2 > 4 | dfx$rts2 < 0.2 | as.logical(dfx$sharkattack)),which(!1:nT %in% 1:nrow(dfx)))
    #miss_any<-which(dfx$Missed)
    shark_stan$choice[s,miss_c1,1]=0
    shark_stan$choice[s,miss_c2,2]=0
    shark_stan$rt[s,miss_c1,1]=0
    shark_stan$rt[s,miss_c2,2]=0
    shark_stan$motorchoice[s,miss_c1,1]=0
    shark_stan$motorchoice[s,miss_c2,2]=0
    shark_stan$reward[s,miss_c1]=0
    shark_stan$reward[s,miss_c2]=0
    #shark_stan$reward[s,miss_any]=0
    shark_stan$state_2[s,miss_c2]=1 #This is to make sure that state_2 does not contain any NAs because stan doesn't like it
    shark_stan$missing_choice[s,miss_c1,1]=1
    shark_stan$missing_choice[s,miss_c2,2]=1
    
    shark_stan$skip_choice[s,skip_c1,1]=1
    shark_stan$skip_choice[s,skip_c2,2]=1
    #shark_stan$missing_reward[s,miss_any]=1
    dfx<-NULL
  }
  shark_stan$Group<-as.numeric(as.factor(shark_stan$Group))-1
  return(shark_stan)
}

run_shark_stan<-function(data_list=NULL,stanfile=NULL,modelname=NULL,stan_args="default",assignresult=T,add_args=NULL,add_data=NULL,forcererun=F,skipthis=F,
                         savepath="stan_scripts/stan_output",pars=NULL,iter=NULL,chains=NULL,open_shinystan=F,...){
  if(stan_args=="default"){stan_arg<-list(verbose=FALSE,save_warmup=FALSE,...,
                                          pars=c('lp_','prev_choice','tran_count','tran_type'),chains = 4,
                                          include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))}else{stan_arg<-stan_args}
  if(!is.null(pars)){stan_arg$pars<-pars}; if(!is.null(iter)){stan_arg$iter<-iter}; if(!is.null(chains)){stan_arg$chains<-chains}; 
  if(is.null(stan_arg$file)){ if(!is.null(stanfile)){stan_arg$file<-stanfile} else {stop("No stan script specified")} };
  if(is.null(stan_arg$data)){ if(!is.null(data_list)){stan_arg$data<-data_list} else {stop("No data list specified")} };
  if(!is.null(add_data)){stan_arg$data=c(stan_arg$data,add_data)}; if(!is.null(add_args)){stan_arg<-c(stan_arg,add_args)}
  if(is.null(savepath)){savepath<-getwd()}; if(is.null(modelname)){modelname<-paste0("shark_stan",runif(1, 1, 99999))}
  
  #Protect against new updates:
  if(is.null(stan_arg$data$factorizedecay)){is.null(stan_arg$data$factorizedecay)<-0}
  if(forcererun | !file.exists(file.path(savepath,paste0(modelname,".rdata")))){
  templs<-list()
  message("Start Running Stan Model...")
  templs[[paste0("stanfit_",modelname)]]<-do.call(stan,stan_arg)
  message("Completed!")
  templs[["data_list"]]<-data_list
  save(list = c(paste0("stanfit_",modelname),"data_list"),file = file.path(savepath,paste0(modelname,".rdata")),envir =   as.environment(templs))
  } else if (!forcererun & !skipthis) {
    message("This model has been previously ran, and now will load it"); 
    tempenvir<-new.env(); load(file.path(savepath,paste0(modelname,".rdata")),envir = tempenvir)
    templs<-as.list(tempenvir)
  } else {message("This model has been skipped.")}
  if(!skipthis){
  if(assignresult){assign(x = modelname,value = templs,envir = .GlobalEnv)}else{return(templs)}
  if(open_shinystan){launch_shinystan(templs[[paste0("stanfit_",modelname)]])}
  }
}

get_summary_df<-function(output_ls=NULL,pars=c("alpha","beta_1_MF","beta_1_MB"),returnas="data.frame",probs=0.5){
  stan_outfit<-output_ls[[grep("stanfit_",names(output_ls))]]
  stan_ls<-list(ID=output_ls$data_list$ID,Group=output_ls$data_list$Group,row.names = NULL)
  allpars<-lapply(pars,function(parx){
    summary(stan_outfit,pars=parx,probs=probs)$summary[,paste0(probs*100,"%")]
  })
  names(allpars)<-pars
  stan_lsx<-c(allpars,stan_ls)
  if(returnas=="data.frame"){
    message("Please ensure the dimensions of the parameters in query is of the same length otherwise you will encounter error.")
    stan_return<-do.call(data.frame,stan_lsx)
  } else {stan_return<-stan_lsx}
  return(stan_return)
}

model_par_compar<-function(fit_list=NULL,data_list=NULL,pars=c("beta_1_MB","beta_1_MF"),probs=0.5){
  
}

extract_pars<-function(stan_fitoutput=NULL,pars=c("log_lik"),mashdim=list(log_lik=c(1)),FUNC=mean,extracted_df=NULL){
  if(is.null(extracted_df)){
  extracted_df<-extract(stan_fitoutput)
  }
  if(length(pars)>1) {
    tx<-lapply(pars,function(parx){
      #pars[[1]]->parx
      maxtrixtdo<-extracted_df[[parx]]
      marginx<-which(!1:length(dim(maxtrixtdo)) %in% mashdim[[parx]])
      txx<-apply(maxtrixtdo,marginx,FUNC)
      return(txx)
    })
  }else {
    pars[[1]]->parx
    maxtrixtdo<-extracted_df[[parx]]
    marginx<-which(!1:length(dim(maxtrixtdo)) %in% mashdim[[parx]])
    tx<-apply(maxtrixtdo,marginx,FUNC)
  }
  return(tx)
}

shark_get_log_lik<-function(extracted_fit=NULL){
  sh_loglik_matrix<-extract_pars(extracted_df=extracted_fit,pars=c("log_lik"),mashdim=list(log_lik=c(1)),FUNC=mean)
  all_sub<-lapply(1:dim(sh_loglik_matrix)[1],function(xj){
    data.frame(stage1_loglik=sh_loglik_matrix[xj,,1],
               stage2_loglik=sh_loglik_matrix[xj,,2])
  })
  return(all_sub)
}


shark_initfun <- function() {
  list(
    alpha_ddm_s1_m = runif(1, 1, 2),
    alpha_ddm_s2_m = runif(1, 1, 2),
    alpha_ddm_s1_s = runif(1, 0.5, 0.1),
    alpha_ddm_s2_s = runif(1, 0.5, 0.1),
    alpha_ddm_s1_raw = rnorm(nS,0,0.1),
    alpha_ddm_s2_raw = rnorm(nS,0,0.1),
    
    non_dec_s1_m = runif(1, 0.1, 0.2),
    non_dec_s2_m = runif(1, 0.1, 0.2),
    non_dec_s1_s = runif(1, 0.05, 0.15),
    non_dec_s2_s = runif(1, 0.05, 0.15),
    non_dec_s1_raw = rnorm(nS,0,0.1),
    non_dec_s2_raw = rnorm(nS,0,0.1)
  )
}

shark_mkinit <- function(chainnum,nS,transform=T) {
 lapply(1:chainnum,function(xc){
   if(!transform){
  list(
     alpha_ddm_s1_m = 1.5,
     alpha_ddm_s2_m = 1.5,
     alpha_ddm_s1_s = 0.5,
     alpha_ddm_s2_s = 0.5,
     alpha_ddm_s1_raw = rep(0,nS),
     alpha_ddm_s2_raw = rep(0,nS),
     alpha_ddm_s1 = rep(0,nS),
     
     non_dec_s1_m = 0.2,
     non_dec_s2_m = 0.2,
     non_dec_s1_s = 0.1,
     non_dec_s2_s = 0.1,
     non_dec_s1_raw = rep(0,nS),
     non_dec_s2_raw = rep(0,nS),
     alpha_ddm_s2 = rep(0,nS)
   )
   }else {
     list(
       alpha_ddm_s1_m = 0.4054651,
       alpha_ddm_s2_m = 0.4054651,
       alpha_ddm_s1_s = -0.6931472,
       alpha_ddm_s2_s = -0.6931472,
       alpha_ddm_s1_raw = rep(0,nS),
       alpha_ddm_s2_raw = rep(0,nS),
       
       non_dec_s1_m =  -1.609438,
       non_dec_s2_m =  -1.609438,
       non_dec_s1_s = -2.302585,
       non_dec_s2_s = -2.302585,
       non_dec_s1_raw = rep(0,nS),
       non_dec_s2_raw = rep(0,nS)
     )
   }
 })
}

shark_getpar<-function(dx){
  list(alpha_ddm_s1=dx$alpha_ddm_s1_m + (dx$alpha_ddm_s1_s * dx$alpha_ddm_s1_raw),
       alpha_ddm_s2=dx$alpha_ddm_s2_m + (dx$alpha_ddm_s2_s * dx$alpha_ddm_s2_raw),
       non_dec_s1=dx$non_dec_s1_m + (dx$non_dec_s1_s * dx$non_dec_s1_raw),
       non_dec_s2=dx$non_dec_s2_m + (dx$non_dec_s2_s * dx$non_dec_s2_raw),
       dx=dx)
}

inv_logit<-function(x){( exp(x)/(1+exp(x)) )}

