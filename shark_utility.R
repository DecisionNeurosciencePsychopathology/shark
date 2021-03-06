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

getFuncArgs <- function(function_name=NULL){
  gx <- args(name = function_name)
  return(as.list(gx))
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
genPercentage<-function(dfx,condition=c("Context","Emotion"),response=c("FaceResponseText"),excludeNA=T,missresp=NA,lablevar=T) {
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
    if(lablevar){
      lableVar(prob)
    } 
    return(prob)
  }) )
  
  return(nwx)
}

behav_qc_general<-function(datalist=NULL,p_name="",logic_sp=NULL,resp_toget=NULL,resp_var=NULL,rt_var=NULL){
  all_stats<-lapply(datalist,function(dfa){
    # print(unique(dfa$ID))
    if(nrow(dfa)>0){
      if(!is.null(resp_toget)){
        gp<-genPercentage(dfa,condition = logic_sp,response = resp_var,lablevar = F)
        gp<-gp[gp$resp==resp_toget,]
        
        if(!is.null(gp) && nrow(gp)>0){
          gpp<-gp$p[apply(gp[logic_sp],1,function(x){!any(!as.logical(x))})]
        }else {gpp<-NA}
      } else {gpp<-NA}
      
      if(!is.null(resp_var)){
        y<-rle(as.character(dfa[[resp_var]]))
        max_rep_l<-max(sapply(unique(dfa[[resp_var]]),function(toget){
          
          max(y$lengths[y$values %in% toget])
          
        })) / nrow(dfa)
        sum_rep<-sum(y$lengths-1) / nrow(dfa)
      } else {
        max_rep_l<-NA
        sum_rep<-NA
      }
      
      if(any(as.numeric(dfa[[rt_var]])>900,na.rm = T)) {
        rt<-as.numeric(dfa[[rt_var]])/1000
      } else {rt<-as.numeric(dfa[[rt_var]])}
      rt[rt==0]<-NA
      
      if(any(rt<0.05,na.rm = T)){
        rtp<-length(which(rt<0.1)) / length(rt)
      } else {rtp<-0}
      
      rt_mean<-mean(rt,na.rm=T)
      rt_swing<-sd(-(1/rt),na.rm = T)
      
      
      
      data.frame(sum_rep=sum_rep,max_rep=max_rep_l,logic_p=gpp[1],rt_ptfast=rtp,rt_mean=rt_mean,rt_swing=rt_swing,pra_name=p_name)
    } else {
      data.frame(sum_rep=NA,max_rep=NA,logic_p=NA,rt_ptfast=NA,rt_mean=NA,rt_swing=NA,pra_name=p_name)
    }
  })
  df_stats<-do.call(rbind,all_stats)
  df_stats$ID<-names(all_stats)
  rownames(df_stats)<-NULL
  return(df_stats)
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
#shark_data$`210100`->dfx
shark_proc<-function(dfx,rt_range=c(0.2,4)) {
  if(nrow(dfx)!=100) {message("Subject: ",unique(dfx$ID)," has ",nrow(dfx)," trials instead of 100; some calculations might be off.")}
  
  #Remove output for missing choice and etc
  dfx$keycode1[dfx$keycode1==0]<-NA
  dfx$keycode2[dfx$keycode2==0]<-NA
  dfx$choice1[dfx$choice1==0]<-NA
  dfx$choice2[dfx$choice2==0]<-NA
  dfx$rts1[dfx$rts1==0] <- NA
  dfx$rts2[dfx$rts2==0] <- NA
  dfx$state[dfx$state==0]<-NA
  
  
  
  dfx$Block = ceiling(dfx$trial/25)
  dfx$Run = ceiling(dfx$trial/50)
  
  dfx$MissedS1 <- is.na(dfx$rts1) 
  dfx$MissedS2 <- is.na(dfx$rts2)
  dfx$Missed <- dfx$MissedS1 | dfx$MissedS2
  
  dfx$OutlierS1 <- dfx$rts1 < rt_range[1] |  dfx$rts1 > rt_range[2]
  dfx$OutlierS2 <- dfx$rts2 < rt_range[1] |  dfx$rts2 > rt_range[2]
  dfx$Outlier <- dfx$OutlierS1 | dfx$OutlierS2
  
  dfx$RocketSwitch<- ifelse(as.logical(dfx$swap.hist),"Switch","Same") #if rocket switched sides
  
  
  dfx$KeySequence<- paste(dfx$keycode1,dfx$keycode2,sep = "&")
  dfx$KeySequence[dfx$Missed]<-NA
  
  dfx$won[dfx$Missed] <- NA
  
  # dfx$DecRecode<-NA
  # dfx$DecRecode[dfx$state==2 & dfx$choice2==1]<-"P2L"
  # dfx$DecRecode[dfx$state==2 & dfx$choice2==2]<-"P2R"
  # dfx$DecRecode[dfx$state==3 & dfx$choice2==1]<-"P3L"
  # dfx$DecRecode[dfx$state==3 & dfx$choice2==2]<-"P3R"
  
  dfx$ChoiceSequence<- paste(dfx$choice1,dfx$choice2,sep = "&")
  dfx$ChoiceSequence[dfx$Missed]<-NA
  
  #Rewrite this so that it is clear for later
  dfx$Transition <- NA
  dfx$Transition[dfx$choice1==1 & dfx$state==2] <- "Common"
  dfx$Transition[dfx$choice1==2 & dfx$state==3] <- "Common"
  
  dfx$Transition[dfx$choice1==1 & dfx$state==3] <- "Rare"
  dfx$Transition[dfx$choice1==2 & dfx$state==2] <- "Rare"
  
  
  dfx$RewardType <- ifelse(as.logical(dfx$won),"Reward","Omission")
  dfx$ifSharkBlock <- (dfx$contingency==1 & (dfx$trial %in% c(1:25,51:75))) | (dfx$contingency==2 & (dfx$trial %in% c(26:50,76:100))) 
  dfx$BlockType<-plyr::mapvalues(dfx$ifSharkBlock,from = c("TRUE","FALSE"),to = c("Shark","Baseline"))
  
  #Set baselines for each regressors: 
  dfx$RewardType <- factor(dfx$RewardType,levels = c("Omission","Reward"))
  dfx$Transition <- factor(dfx$Transition,levels = c("Common","Rare"))
  dfx$BlockType  <- factor(dfx$BlockType,levels = c("Baseline","Shark"))
  
  #To ensure sequential order before lag and lead
  dfx<-dfx[with(dfx,order(ID,trial)),]
  
  dfx$rts1_scale<-as.numeric(scale(dfx$rts1))
  dfx$rts2_scale<-as.numeric(scale(dfx$rts2))
  
  for (ix in c("choice1","BlockType","Transition",
               "choice2",
               "keycode1",
               "keycode2",
               "RewardType",
               "state",
               "rts1","rts1_scale",
               "rts2","rts2_scale"
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
  dfx$SameState_lead <- as.factor(dfx$state_lead == dfx$state)
  
  return(dfx)
}

shark_summarize<-function(dfx=NULL,useable_vars=c("Missed","Outlier")) {
  
  dfx$not_useable <- apply(dfx[useable_vars],1,any)
  
  df_o_miss<-data.frame(Type = "Overall",
                        not_useable   = length(which(dfx$not_useable)) / nrow(dfx))
  
  df_blcok_miss <- aggregate(not_useable~Block,data = dfx,FUN = function(x){length(which(x)) / length(x)})
  df_blcok_miss$Type <- paste(colnames(df_blcok_miss)[1],df_blcok_miss$Block)
  df_blcok_miss$Block <- NULL
  
  df_shark_miss <- aggregate(not_useable~BlockType,data = dfx,FUN = function(x){length(which(x)) / length(x)})
  colnames(df_shark_miss)[1] <- "Type"
  
  df_run_miss <- aggregate(not_useable~Run,data = dfx,FUN = function(x){length(which(x)) / length(x)})
  df_run_miss$Type <- paste(colnames(df_run_miss)[1],df_run_miss$Run)
  df_run_miss$Run <- NULL
  
  df_miss <- rbind(df_o_miss,df_blcok_miss,df_run_miss,df_shark_miss)
  df_miss$ID <- unique(dfx$ID)
  #  
  #  p_miss<-(length(which(is.na(dfx$choice1))) / length(dfx$choice1))
  #  if(!is.na(missthres)){
  #  p_miss_if<- p_miss < missthres
  #  } else {p_miss_if<-TRUE;}
  #  
  #  z<-genPercentage(dfx,condition=c('ifReinf','ifRare'),response=c("ifSwitched1"))
  #  p_com_reinf_stay<-z$p[z$ifRare=="Not_Rare" & z$ifReinf=="Reinf" & z$resp=="FALSE"]
  #  if(!is.na(comreinfstaythres)){
  #    if_com_reinf_stay <- p_com_reinf_stay > comreinfstaythres
  #  } else {if_com_reinf_stay<-TRUE;}
  # 
  #  p_com_reinf_stay_chance<-pbinom(length(which(!as.logical(dfx$ifRare) & as.logical(dfx$ifReinf) & as.logical(dfx$Stay1))), 
  #                                  length(which(!as.logical(dfx$ifRare) & as.logical(dfx$ifReinf))), 0.5)
  # if(!is.na(P_staycomreinfchance)){
  #   #Get chance p of stay given common reinf, 
  #   if_chance_comreinfstay <- p_com_reinf_stay_chance > P_staycomreinfchance
  # }else{if_chance_comreinfstay<-TRUE;}
  # 
  #  if(returnstats){data.frame(p_miss=p_miss,p_comreinfstay=p_com_reinf_stay,p_chancecomreinfstay=p_com_reinf_stay_chance)}else{
  #  if (p_miss_if && if_com_reinf_stay && if_chance_comreinfstay) {return(dfx)} else {return(NULL)}
  #  }
  return(df_miss)
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



shark_fsl<-function(dfx=NULL,comboRLpara=F,RLparadf=NULL,design_grid=NULL) {
  #print(unique(dfx$ID))
  dfx$Trial<-dfx$trial
  if(comboRLpara && !is.null(RLparadf)){
    dfx<-merge(dfx,RLparadf,all.x = T,by = c("ID","Trial"))
    for(xrz in c("RPE_MB","RPE_MF","RPE_diff","delta_2")){
      dfx[[xrz]]<-as.numeric(scale(dfx[[xrz]]))
    }
  }
  
  dfx<-dfx[order(dfx$Trial),]
  dfx$Trial<-as.numeric(unlist(lapply(split(dfx$trial,dfx$Run),seq_along)))
  
  if(is.null(design_grid$shark_stage)) {
    no_mod = TRUE
  } else {
    no_mod = FALSE
  }
  # #Gen QC regressors:
  # dfx$QC_OUT<-sample(1:0,length(dfx$Trial),replace = T)
  # #Motor   #left is 1 and right is 2
  # dfx$LeftRight1<-plyr::mapvalues(dfx$keycode1,from=(1:2),to=c(-1,1))
  # dfx$LeftRight2<-plyr::mapvalues(dfx$keycode2,from=(1:2),to=c(-1,1))
  # #Task Variables:
  # dfx$SharkBlock<-as.numeric(as.character(plyr::mapvalues(dfx$ifSharkBlock,from=(c("TRUE","FALSE")),to=c(1,-1))))
  # dfx$SharkBlock_lag<-dplyr::lag(dfx$SharkBlock)
  # dfx$Tran<-as.numeric(as.character(plyr::mapvalues(dfx$ifRare,from=(c("TRUE","FALSE")),to=c(1,-1))))
  # dfx$Tran_lag<-dplyr::lag(dfx$Tran)
  # dfx$Reinf<-as.numeric(as.character(plyr::mapvalues(dfx$ifReinf,from=(c("TRUE","FALSE")),to=c(1,-1))))
  # dfx$Reinf_lag<-dplyr::lag(dfx$Reinf)
  # #Regressors:
  # dfx$ModelBase<-dfx$Tran * dfx$Reinf
  # dfx$ModelBase_lag<-dplyr::lag(dfx$ModelBase)
  # 
  # dfx$ModelBaseShark<-dfx$Tran * dfx$Reinf * dfx$SharkBlock
  # dfx$ModelBase_lagShark<-dfx$ModelBase_lag * dfx$SharkBlock
  # 
  # dfx$RareNotReinf<-as.numeric(as.logical(dfx$ifRare) & !as.logical(dfx$ifReinf))
  # dfx$RareNotReinf_lag<-dplyr::lag(dfx$RareNotReinf)
  # 
  # dfx$RareReinf<-as.numeric(as.logical(dfx$ifRare)) * as.numeric(as.logical(dfx$ifReinf)) 
  # dfx$RareReinf_lag<-dplyr::lag(dfx$RareNotReinf)
  
  # #Shark 
  # dfx$SharkTran<-dfx$Tran * as.numeric(as.logical(dfx$ifSharkBlock)) 
  # dfx$SharkTran_lag<-dfx$Tran_lag * as.numeric(as.logical(dfx$ifSharkBlock)) 
  # 
  # dfx$SharkReinf<-dfx$Reinf * as.numeric(as.logical(dfx$ifSharkBlock)) 
  # dfx$SharkReinf_lag<-dfx$Reinf_lag * as.numeric(as.logical(dfx$ifSharkBlock)) 
  # 
  # dfx$SharkRR<-dfx$RareReinf * as.numeric(as.logical(dfx$ifSharkBlock)) 
  # dfx$SharkRR_lag<-dfx$RareReinf_lag * as.numeric(as.logical(dfx$ifSharkBlock)) 
  
  ##Specify New Model Here :
  dfx$reward_lag <- 0
  dfx$reward_lag[which(as.character(dfx$RewardType_lag)=="Reward")] <- 1
  
  dfx$trax_lag <- 0
  dfx$trax_lag[which(as.character(dfx$Transition_lag) == "Rare")] <- 1
  
  dfx$RocketSwitch_num <- as.numeric(dfx$RocketSwitch == "Switch")
  
  dfx$rewXtrax_lag <- dfx$reward_lag * dfx$trax_lag
  
  dfx$trax <- 0
  dfx$trax[which(as.character(dfx$Transition)=="Rare")] <- 1
  
  dfx$reward_cur <- 0
  dfx$reward_cur[which(as.character(dfx$RewardType)=="Reward")] <- 1
  
  dfx$rewXtrax <- dfx$trax * dfx$reward_cur
  
  
  timing_s1 <- dfx[c("stim1.ons.ms","stim2.ons.ms","rts1","Run","Trial")]
  timing_s2 <- dfx[c("stim2.ons.ms","rts2","rew.ons.ms","Run","Trial")]
  
  #mu_beta is the value of common stage
  dfx$mu_beta_trx <- NA
  dfx$value_stage <- NA
  dfx$pe_planet_beta <- NA
  alpha_comm = 1 
  beta_rare = 1
  for (i in 1:nrow(dfx)) {
    if(!is.na(dfx$choice1[i])) {
      dfx$mu_beta_trx[i]  = (alpha_comm / (alpha_comm+beta_rare));
      if((as.character(dfx$Transition)=="Common")[i]) {
        alpha_comm <- alpha_comm + 1
        dfx$pe_planet_beta[i] <- 1 - dfx$mu_beta_trx[i]
      } else {
        beta_rare <- beta_rare + 1
        dfx$pe_planet_beta[i] <- 0 - dfx$mu_beta_trx[i]
      }
    }
  }
  
  
  
  ###Maybe later try to model the missing trials? For now consider them noise and baseline them;
  if(!no_mod) {
    timing_s1<-timing_s1[which(!is.na(timing_s1$rts1)),]
    timing_s1$nTrial<-ave(timing_s1$Run,timing_s1$Run,FUN = seq_along)
    timing_s1$runXtrial <- paste0(timing_s1$Run,"_XTXTX_",timing_s1$Trial)
    
    timing_s2<-timing_s2[which(!is.na(timing_s2$rts2)),]
    timing_s2$nTrial<-ave(timing_s2$Run,timing_s2$Run,FUN = seq_along)
    timing_s2$runXtrial <- paste0(timing_s2$Run,"_XTXTX_",timing_s2$Trial)
  } 
  timing_ls <- list(s1 = timing_s1, s2 = timing_s2)
  
  finalist<-list(Decision1=data.frame(event="Decision1",
                                      onset=timing_s1$stim1.ons.ms/1000,
                                      duration=timing_s1$rts1,
                                      run=timing_s1$Run,
                                      trial=timing_s1$nTrial),
                 PlanetOnSet=data.frame(event="PlanetOnSet",
                                              onset=timing_s1$stim2.ons.ms/1000,
                                              duration=0.5,
                                              run=timing_s1$Run,
                                              trial=timing_s1$nTrial),
                 Decision2=data.frame(event="Decision2",
                                      onset=timing_s2$stim2.ons.ms/1000,
                                      duration=timing_s2$rts2,
                                      run=timing_s2$Run,
                                      trial=timing_s2$nTrial),
                 Outcome=data.frame(event="Outcome",
                                    onset=timing_s2$rew.ons.ms/1000,
                                    duration=0.5,
                                    run=timing_s2$Run,
                                    trial=timing_s2$nTrial)
                 # # SharkBlock=data.frame(event="SharkBlock",
                 # #                       onset=shark_dfx$onset,
                 # #                       duration=shark_dfx$duration,
                 # #                       run=shark_dfx$run,
                 # #                       trial=shark_dfx$Trial),
                 # QC=data.frame(event="QC",
                 #               onset=dfx$stim1.ons.ms/1000,
                 #               duration=dfx$rts1,
                 #               run=dfx$Run,
                 #               trial=dfx$Trial)
  )
  finalist[["allconcat"]]<-do.call(rbind,finalist)
  
  value<-as.list(dfx)
  
  if(!no_mod) {
    dfx$runXtrial <- paste0(dfx$Run,"_XTXTX_",dfx$Trial)
    
    for (i in which(!grepl("_evt",design_grid$valuefrom))) {
      value[[design_grid$valuefrom[i]]] <- dfx[[design_grid$valuefrom[i]]][match(timing_ls[[design_grid$shark_stage[i]]]$runXtrial,dfx$runXtrial)]
    }
  }
  
  
  # value$SharkBlockValue<-shark_dfx$type
  
  #Update version:
  sublvldfx<-unique(dfx[,names(dfx)[apply(dfx,2,function(x){length(unique(x))})==1]])
  
  output<-list(event.list=finalist,output.df=dfx,value=value,ID=unique(dfx$ID),
               #Here's for the new version:
               sublevel=sublvldfx,triallvl=dfx
  )
  return(output)
}

shark_sim_stan_prep<-function(df){
  sp <- split(df,df$uID)
  nS <- length(sp)
  nT <- max(sapply(sp,nrow))
  
  shark_stan<-list(
    nS=nS,nT=nT,
    choice=array(0,dim=c(nS,nT,2)),
    state_2=array(0,dim=c(nS,nT)),
    reward=array(0,dim=c(nS,nT))
  )
  
  for (s in 1:nS) {
    shark_stan$choice[s,,1]<-sp[[s]]$s1_choice -1
    shark_stan$choice[s,,2]<-sp[[s]]$s2_choice -1
    shark_stan$state_2[s,]<-sp[[s]]$s2_state-1
    shark_stan$reward[s,]<-sp[[s]]$reward
  }
  return(shark_stan)
}

shark_stan_prep<-function(shark_split=NULL,grpchoice="all"){
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
    shark_stan$Group[s]<-unique(as.character(dfx$Group))
    shark_stan$grp_hc[s]=as.numeric(unique(as.character(dfx$Group))=="1")
    shark_stan$grp_dep[s]=as.numeric(unique(as.character(dfx$Group))=="2")
    shark_stan$grp_ide[s]=as.numeric(unique(as.character(dfx$Group))=="4")
    shark_stan$grp_att[s]=as.numeric(unique(as.character(dfx$Group))=="5")
    shark_stan$grp_clinical[s]=as.numeric(unique(as.character(dfx$Group))!="1")
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
    
    skip_c1<-c(which(dfx$rts1 > 5 | dfx$rts1 < 0.15 | as.logical(dfx$sharkattack)),which(!1:nT %in% 1:nrow(dfx)))
    skip_c2<-c(which(dfx$rts2 > 5 | dfx$rts2 < 0.15 | as.logical(dfx$sharkattack)),which(!1:nT %in% 1:nrow(dfx)))
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
  
  switch (grpchoice,
          "all" = {
            shark_stan$Group<-(as.numeric(as.factor(shark_stan$Group)))-1
            shark_stan$GroupNum<-3
          },
          "hc_dep" = {
            shark_stan$Group[as.numeric(shark_stan$Group) > 1]<-2
            shark_stan$Group<-(as.numeric(as.factor(shark_stan$Group)))-1
            shark_stan$GroupNum<-2
          },
          "hc_dep_sui" = {
            shark_stan$Group[as.numeric(shark_stan$Group) > 2]<-3
            shark_stan$Group<-(as.numeric(as.factor(shark_stan$Group)))-1
            shark_stan$GroupNum<-3
          }
  )
  
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
    templs[["stanfit"]]<-do.call(stan,stan_arg)
    message("Completed!")
    templs[["data_list"]]<-data_list
    templs[["extracted_fit"]]<-rstan::extract(object = templs[["stanfit"]])
    save(list = c("stanfit","data_list","extracted_fit"),file = file.path(savepath,paste0(modelname,".rdata")),envir =   as.environment(templs))
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

shark_prep_mulT<-function(sdf,tlag){
  #print(unique(sdf$ID))
  sdf$Reward<-factor(gsub(" ","",sdf$RewardType),levels = c("Reward","NoReward"))
  sdf$uCond<-interaction(sdf[c("Transition","Reward")],sep = "_")
  for(uc in levels(sdf$uCond)){
    sdf[[uc]]<-0
    sdf[[uc]][sdf$uCond %in% uc & !is.na(sdf$choice1)]<-gsub(2,-1,sdf$choice1)[sdf$uCond %in% uc & !is.na(sdf$choice1)]
    sdf[[uc]][is.na(sdf$choice1)]<-NA
    sdf[[uc]][is.na(sdf$uCond)]<-NA
    sdf[[uc]]<-as.numeric(sdf[[uc]])
    for(tg in 0:tlag){
      sdf[[paste(uc,tg,sep="_")]]<-dplyr::lag(sdf[[uc]],tg)
    }
    
    sdf$Stay1_lag<-dplyr::lag(sdf$Stay1,1)
    sdf$ChoiceS1<-as.numeric(gsub(2,0,sdf$choice1))
    sdf$ChoiceS1_lag<-dplyr::lag(sdf$ChoiceS1,1)
    sdf$ChoiceS1_lead<-dplyr::lead(sdf$ChoiceS1,1)
  }
  
  return(sdf)
}

stan_ex_clean<-function(lsx=NULL,FUNCX=median){
  if(length(dim(lsx))>1){
    gx<-apply(lsx,2:length(dim(lsx)),FUNCX)
  } else {
    gx<-FUNCX(lsx)
  }
  gx[gx< (-990)]<-NA
  return(gx)
}

colVars <- function (a){
  diff <- a - matrix (colMeans(a), nrow(a), ncol(a), byrow=TRUE)
  vars <- colMeans (diff^2)*nrow(a)/(nrow(a)-1)
  return (vars)
}

get_WAIC <- function (log_lik){
  log_lik[is.na(log_lik)]<-0
  log_lik[log_lik< (-990)]<-0
  lppd <- sum (log (colMeans(exp(log_lik))))
  p_waic_1 <- 2*sum (log(colMeans(exp(log_lik))) - colMeans(log_lik))
  p_waic_2 <- sum (colVars(log_lik))
  waic_2 <- -2*lppd + 2*p_waic_2
  return (list (waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1))
}









