# import, check and prepare for analyses data from two bandit samples

#setwd("~/Box Sync/skinner/projects_analyses/Project Shark/processed_data")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(reshape2)
library(corrplot)
library(lsmeans)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(VIM)
library(multcompView)
library(stargazer)
library(emmeans)

###################
source("shark_utility.R")
if(F){
  #reloading the data use this sript:
  source("./behavioral/shark_beh_analyses_import_process.R")
}
load(file = "shark_data.RData") 
message("Number of subject before clean up: ",length(shark_data_proc))
#First we should remove subject who have 
o_miss_df <- do.call(rbind,lapply(shark_data_proc,shark_summarize,useable_vars=c("Missed","Outlier")))
hist(o_miss_df$not_useable[o_miss_df$Type=="Overall"])
ex_miss_ID<-o_miss_df$ID[o_miss_df$Type=="Overall" & o_miss_df$not_useable > 0.3]
library(ggplot2)
ggplot(o_miss_df,aes(x=not_useable))+geom_histogram()+facet_wrap(~Type)


block_miss_df <- o_miss_df[grep("Block",o_miss_df$Type),]
block_miss_df$Outliers <- block_miss_df$not_useable > 0.2

ggplot(blcok_miss_df[block_miss_df$not_useable>0.05,],aes(x=Type,color=Type,y=not_useable))+
  geom_boxplot()+geom_point(position = position_dodge2(width=0.3))

ID_blocks_ex <- do.call(paste,block_miss_df[block_miss_df$Outliers,c("ID","Type")])
subj_block_df <- aggregate(Outliers~ID,data = block_miss_df,FUN = function(x){length(which(x))/4})
block_max_ex<-subj_block_df$ID[which(subj_block_df$Outliers > 0.5)]


excludeIDs <- block_max_ex
#shark_data_proc_exclude<-cleanuplist(lapply(shark_data_proc,shark_exclusion, missthres=0.2,P_staycomreinfchance=NA,comreinfstaythres=NA,returnstats=F))
# shark_behav_qc_mo<-behav_qc_general(datalist=shark_data_proc,p_name="shark",logic_sp=c("ifRare_lag","ifReinf_lag","Stay1"),
#                                     resp_var="keycode1",resp_toget="1",rt_var="rts1")
# shark_behav_qc_ch<-behav_qc_general(datalist=shark_data_proc,p_name="shark",logic_sp=c("ifRare_lag","ifReinf_lag","Stay1"),
#                                     resp_var="choice1",resp_toget="1",rt_var="rts1")




#  excludeIDs<-unique(c(ex_miss_ID,shark_behav_qc_mo$ID[which(shark_behav_qc_mo$max_rep > 0.4)],
#                       shark_behav_qc_ch$ID[which(shark_behav_qc_ch$max_rep >= 0.8)]))
# 
shark_data_proc_exclude<-shark_data_proc[which(!names(shark_data_proc) %in% excludeIDs)]
shark_data_proc_exclude <- shark_data_proc_exclude
message("Number of subject AFTER clean up: ",length(shark_data_proc_exclude))

bdf <- merge(do.call(rbind,shark_data_proc_exclude),subject_df,by.x = "ID",by.y = "id",all.x = T)
bdf <- bdf[which(!paste(bdf$ID,"Block",bdf$Block) %in% ID_blocks_ex),]
#relevel 
bdf$RewardType <- factor(bdf$RewardType,levels = c("Omission","Reward"))
bdf$Transition <- factor(bdf$Transition,levels = c("Common","Rare"))
bdf$GroupNEW <- factor(bdf$GroupNEW,c("HC","ATT","DNA_HI","DNA_LI"))
bdf$GroupATT <- factor(bdf$GroupATT,c("HC","ATT","DEP","IDE","DNA"))
#Set things to ignore:
bdf$exclude_trial <- bdf$Outlier | bdf$Missed | as.logical(as.character(bdf$sharkattack))

bdf_HC <- bdf[which(bdf$ID %in% as.character(subject_df$id[which(subject_df$Group == "HC")])),]

rep_date <- "2018-09-14"
message(length(as.character(subject_df$id[which(subject_df$scandate < rep_date)])))
bdf_replicate <- bdf[which(bdf$ID %in% as.character(subject_df$id[which(subject_df$scandate < rep_date)])),]

df_to_use <- bdf_replicate
df_to_use$GroupATT<-droplevels(df_to_use$GroupATT)
stop("Finished loading essentials")
#Model task not working: #Let's hope that this is not:

shark_basic <- lme4::glmer(Stay1_lead ~ RocketSwitch + 
                             Transition * RewardType * BlockType +
                             Transition * RewardType * GroupNEW + 
                          (1 | ID),
                        family = binomial(),
                        data = bdf[which(!bdf$exclude_trial),],
                        lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(shark_basic)
car::Anova(shark_basic, type = 'III')
car::vif(shark_basic)
sjPlot::plot_model(shark_basic,type = "pred",transform = NULL,terms=c("RocketSwitch","RewardType"))
sjPlot::plot_model(shark_basic,type = "emm",terms=c("Transition","RewardType","GroupNEW"))
emmeansx<-emmeans(shark_basic,specs= ~ RewardType*Transition|GroupNEW,
                  data=bdf[which(!bdf$exclude_trial),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = emmean - 2*SE, 
    ymax = emmean + 2*SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~GroupNEW,ncol=2)





shark_basic_hc <- lme4::glmer(Stay1_lead ~ Stay1 + Transition*RewardType*SameKey1_lead + 
                          Transition * RewardType * BlockType + 
                          (1 | ID),
                        family = binomial(),
                        data = bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GroupNEW == "HC"),],
                        lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(shark_basic_hc)
car::Anova(shark_basic_hc, type = 'III')
sjPlot::plot_model(shark_basic_hc,type = "pred",terms=c("RewardType","SameKey1_lead"))


shark_basic_hc <- lme4::glmer(as.factor(keycode1_lead) ~ 
                                Transition * RewardType * as.factor(keycode1) *rocketLRswitch +
                               # Transition * RewardType * rocketLRswitch + 
                                (1 | ID),
                              family = binomial(),
                              data = bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GroupNEW == "HC"),],
                              lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

#Star Model Currently:

shark_basic_group <- lme4::glmer(Stay1_lead ~ Stay1  + SameKey1_lead + 
                        Transition * RewardType * GroupNEW + 
                        Transition * RewardType * BlockType + 
                 (1 | ID),
                 family = binomial(),
                 data = bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],
                 lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(shark_basic_group)
car::Anova(shark_basic_group, type = 'III')
car::vif(shark_basic_group)
sjPlot::plot_model(shark_basic_group,type = "pred",terms=c("Transition","RewardType","GroupNEW"))

shark_valenceRPE<-lme4::glmer(choice2 ~ choice2_lag + state * Transition + (Transition * RewardType | ID), 
                              family = binominal(), data = bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],
	lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(shark_valenceRPE)
car::Anova(shark_valenceRPE,type='III')



grx<-ggpredict(shark_gm,terms = c("Transition","RewardType"),x.as.factor = T,type="fe")

ggplot(grx,aes(x,predicted,fill=group))+geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
  ymin = conf.low, 
  ymax = conf.high),stat = "identity",position = position_dodge(.9),width=0.1)+ 
  scale_fill_manual(values=c("indianred3", "steelblue3"))






a<-sjPlot::plot_model(shark_gm,type = "pred",terms = c("RewardType","Transition","BlockType"),
                      se = T,show.values = T,pred.type = "fe")

ggsave(a,device = "pdf",filename = "choice_glmer.pdf",width = 6,height = 6)

emmeansx<-emmeans(shark_basic,specs= ~ RewardType*Transition*BlockType|Group,
                  data=bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],type = "response")
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=prob, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = prob - 2*SE, 
    ymax = prob + 2*SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~Group+BlockType,ncol=2)


m1shark_hc <- glmer(Stay1_lead ~  Stay1  + SameKey1_lead +
                   #Transition * ifReinf * GROUP1245 *BlockType +   #Use for 4 ways
                   Transition * RewardType     #HC Model
                   + (1| ID),
                 family = binomial(),
                 data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),],
                 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark_hc)
car::Anova(m1shark_hc, type = 'III')

coef_shark_hc<-coef(m1shark_hc)$ID
shark_coef_hc<-as.data.frame(as.matrix(coef_shark_hc))
high_mb_hc<-rownames(shark_coef_hc)[which(shark_coef_hc$`RewardTypeNo Reward:TransitionRare` > median(shark_coef_hc$`RewardTypeNo Reward:TransitionRare`))]
###################

#RT Models:
rtshark1<-lmerTest::lmer(rts1_lead ~ scale(rts1) + RocketSwitch +
                           Transition * RewardType  * GroupNEW + 
                           Transition * RewardType  * BlockType + 
                       (1 | ID), REML = T,
                     data =  bdf[which(!bdf$exclude_trial),])
summary(rtshark1)
car::Anova(rtshark1,"III")
sjPlot::plot_model(rtshark1,type = "pred",terms=c("GroupNEW","BlockType"))
library(emmeans)
emmeansx<-emmeans(rtshark1,  ~ RewardType*Transition  ,
                  data=df_to_use[which(!df_to_use$exclude_trial),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(color=RewardType, y=emmean, x=Transition)) + geom_boxplot(aes(
  lower = emmean - SE, 
  upper = emmean + SE, 
  middle = emmean, 
  ymin = emmean - 3*SE, 
  ymax = emmean + 3*SE),
  stat = "identity") #+ facet_wrap(~BlockType,ncol = 2)




summary(rtshark1)   
car::Anova(rtshark1, type = 'III')

ggplot(bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),], aes(y = Transition,x=rts1_lead)) +
  geom_density_ridges(scale = 4) + theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) 

library(emmeans)
emmeansx<-emmeans(rtshark1,specs= ~ RewardType*Transition*BlockType|depress,
                  data=bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],type = "response")
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = emmean - 2*SE, 
    ymax = emmean + 2*SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~depress+BlockType,ncol = 2)


df_to_use <- bdf_HC

rtshark2<-lmerTest::lmer(rts1_lead ~ rts1_scale + Stay1 + SameKey1_lead + 
                       Transition * RewardType * BlockType + 
                       (1 | ID),REML =T,
                     data =  df_to_use[which(!df_to_use$exclude_trial),])
summary(rtshark2)
car::Anova(rtshark2,"III")
sjPlot::plot_model(rtshark2,type = "pred",terms = c("RewardType","Transition"),
                   se = F,show.values = T,pred.type = "fe")

anova(rtshark1,rtshark2, type = 'III')


car::Anova(rtshark1, type = 'III')
coefs <- data.frame(coef(summary(rtshark1)))
# use normal distribution to approximate p-value
coefs$p.z <- round(2 * (1 - pnorm(abs(coefs$t.value))),3)
coefs



ggplot(data = df_to_use[which(!df_to_use$exclude_trial),],
       aes(y = log(1/rts1_lead),x=RewardType,color=Transition))+geom_boxplot()+facet_wrap(~Group+BlockType,ncol = 2)


# multicollinearity diagnostics

# collinearity checks

bdf$RewardType<-plyr::mapvalues(bdf$ifReinf,from = c("TRUE","FALSE"),to = c("Reward","No Reward"))
#Plots
library(emmeans)
emmeansx<-emmeans(m1shark,  ~ RewardType*Transition*BlockType |GROUP1245,
                  data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = emmean - SE, 
    ymax = emmean + SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~GROUP1245+BlockType,ncol = 2)

emmeansx<-emmeans(m1shark_hc,  ~RewardType*Transition | BlockType,
                  data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
  #lower = emmean - SE, 
  #upper = emmean + SE, 
  #middle = emmean, 
  ymin = emmean - SE, 
  ymax = emmean + SE),
  stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~BlockType,ncol = 2)



ggplot(data=dfemmeans, aes(x=RewardType, y=rts1_lead, color=Transition)) + geom_boxplot(aes(
  lower = emmean - SE, 
  upper = emmean + SE, 
  middle = emmean, 
  ymin = emmean - 3*SE, 
  ymax = emmean + 3*SE),
  stat = "identity") + facet_wrap(~GROUP1245+BlockType,ncol = 2)


emmeansx_rt<-emmeans(rtshark1_hc,  ~RewardType*Transition | BlockType ,
                     data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),])
dfemmeans<-as.data.frame(emmeansx_rt)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, color=Transition)) + geom_boxplot(aes(
  lower = emmean - SE, 
  upper = emmean + SE, 
  middle = emmean, 
  ymin = emmean - 3*SE, 
  ymax = emmean + 3*SE),
  stat = "identity") + facet_wrap(~BlockType,ncol = 2)

####multi-trials analysis;
tlag=3
shark_mulT<-lapply(shark_sp,shark_prep_mulT,tlag=tlag)
shark_mulT_df<-do.call(rbind,shark_mulT)
addonto<-unlist(lapply(1:tlag-1,function(x){paste(levels(shark_mulT_df$uCond),x,sep="_")}))

model_formula<-as.formula( paste("ChoiceS1_lead ~ ",paste(addonto,collapse = " + "),
                                 "+ (",1," | ID)",sep = "") )
model_sub_formula<-as.formula( paste("ChoiceS1_lead ~ ",paste(addonto,collapse = " + "),
                                     "+ (",paste(addonto,collapse = " + ")," | ID)",sep = "") )

# 
# shark_gm_mulT<-lmer(model_formula,REML=T,
#                      data = shark_mulT_df[(!shark_mulT_df$Outlier & !shark_mulT_df$Missed & !as.logical(shark_mulT_df$sharkattack)),],
#                      lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
shark_gm_mulT<-glmer(model_formula,family = binomial(),
                     data = shark_mulT_df[(!shark_mulT_df$Outlier & !shark_mulT_df$Missed & !as.logical(shark_mulT_df$sharkattack)),],
                     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# shark_sub__gm_mulT<-glmer(model_sub_formula,family = binomial(),
#                      data = shark_mulT_df[(!shark_mulT_df$Outlier & !shark_mulT_df$Missed & !as.logical(shark_mulT_df$sharkattack)),],
#                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# save(shark_sub__gm_mulT,file = "timelag_sub_shark.rdata")

summary(shark_gm_mulT)
car::Anova(shark_gm_mulT)
vif.lme(shark_gm_mulT)
shark_gm_mulT_coef<-as.data.frame(summary(shark_gm_mulT)$coefficients,stringsAsFactors = F)
shark_gm_mulT_coef$L1<-rownames(shark_gm_mulT_coef)
shark_gm_mulT_coef<-shark_gm_mulT_coef[shark_gm_mulT_coef$L1!="(Intercept)",]

# frx<-do.call(rbind,lapply(0:(tlag-1),function(zk){
#   UR=shark_gm_mulT_coef[[paste("Rare_Reward",zk,sep="_")]]
#   CR=shark_gm_mulT_coef[[paste("Common_Reward",zk,sep="_")]]
#   UO=shark_gm_mulT_coef[[paste("Rare_NoReward",zk,sep="_")]]
#   CO=shark_gm_mulT_coef[[paste("Common_NoReward",zk,sep="_")]]
#   data.frame(
#   mf_index=(UR+CR) - (UO+CO),
#   mb_index=(UR-CR) + (UO-CO),
#   timepoint=(zk+1)
#   )
# }))

#m_codf<-melt(shark_gm_mulT_coef)
shark_gm_mulT_coef$RewardType<-sapply(strsplit(shark_gm_mulT_coef$L1,"_"),`[`,2)
shark_gm_mulT_coef$TransType<-sapply(strsplit(shark_gm_mulT_coef$L1,"_"),`[`,1)
shark_gm_mulT_coef$Timepoint<-as.numeric(sapply(strsplit(shark_gm_mulT_coef$L1,"_"),`[`,3))
shark_gm_mulT_coef$SE<-shark_gm_mulT_coef$`Std. Error`

pd <- position_dodge(0.1)
ggplot(shark_gm_mulT_coef[!is.na(shark_gm_mulT_coef$Timepoint),], aes(x=Timepoint, y=Estimate, colour=RewardType,lty=TransType)) + 
  geom_errorbar(aes(ymin=Estimate-SE, ymax=Estimate+SE), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

save(list = ls(all.names = TRUE), file = "shark2.RData") 


