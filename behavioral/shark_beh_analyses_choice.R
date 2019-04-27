# import, check and prepare for analyses data from two bandit samples

#setwd("~/Box Sync/skinner/projects_analyses/Project Shark/processed_data")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(reshape2)
library(ggbiplot)
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

load(file = "shark1.RData") 

#Model task not working: #Let's hope that this is not:

m0null<-glmer(ifSwitchedKey_lead ~ 
                ifSwitchedKey +  ifRare * ifReinf * ifSharkBlock  +  
                (ifReinf * ifRare | ID/Run),
              family = binomial(),
              data = bdf[!bdf$Outlier,],
              glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m0null)
#Okay it seems like this is not true because they are either just guessing or the task is working somewhat

#Now we test basic model with 



# #Let's try total model free, 4 arm bandit;
# m1_free <- glmer(choice1_lead  ~ 
#                      SameKey1_lead +  DecRecode*ifRare * ifReinf * ifSharkBlock  +  
#                (1| ID/Run),
#              family = binomial(),
#              data = bdf[!bdf$Outlier,],
#              glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(m1_free)
# car::Anova(m1_free, type = 'III')

ggplot()

# write factors for reward and transition with deviation coding
bdf$rewardD <- bdf$RewardType
contrasts(bdf$rewardD) = contr.sum(2)
bdf$transitionD <- bdf$Transition
contrasts(bdf$transitionD) = contr.sum(2)


#Star Model Currently:

shark_gm <- lme4::glmer(Stay1_lead ~ Stay1  + SameKey1_lead + Transition * RewardType * BlockType * Group+ 
                 (Transition * RewardType| ID),
                 family = binomial(),
                 data = bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],
                 lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(shark_gm)
car::Anova(shark_gm, type = 'III')
car::vif(shark_gm)

a<-sjPlot::plot_model(shark_gm,type = "pred",terms = c("RewardType","Transition","BlockType"),se = T,show.values = T,pred.type = "fe")

ggsave(a,device = "pdf",filename = "choice_glmer.pdf",width = 6,height = 6)

emmeansx<-emmeans(shark_gm,specs= ~ RewardType*Transition*BlockType|Group,data=bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],type = "response")
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
rtshark1<-lmerTest::lmer(rts1_lead ~ rts1_scale + Stay1 + SameKey1_lead + 
                       # Transition * RewardType * GROUP1245 +
                       # Transition * RewardType * BlockType +
                       # Transition * GROUP1245 * BlockType +
                       # RewardType * GROUP1245 * BlockType +
                       Transition * RewardType * BlockType * Group +   #Use for 4 ways
                       (1 | ID),REML = T,
                     data =  bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),]
                     )


summary(rtshark1)   
car::Anova(rtshark1, type = 'III')
sjPlot::plot_model(rtshark1,type = "pred",terms=c("RewardType","Transition"))
sjPlot::plot_model(rtshark1,type = "pred",terms=c("Transition","BlockType"))
sjPlot::plot_model(rtshark1,type = "pred",terms=c("Transition","Group"))
sjPlot::plot_model(rtshark1,type = "pred",terms=c("BlockType","RewardType","Transition"))
sjPlot::plot_model(rtshark1,type = "pred",terms=c("Group","RewardType","Transition"))

ggplot(bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),], aes(y = Transition,x=rts1_lead)) +
  geom_density_ridges(scale = 4) + theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) 


emmeansx<-emmeans(rtshark1,specs= ~ RewardType*Transition*BlockType|Group,data=bdf[which(!bdf$Missed & !as.logical(bdf$sharkattack)),],type = "response")
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = emmean - 2*SE, 
    ymax = emmean + 2*SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~Group+BlockType,ncol = 2)



rtshark2<-lmerTest::lmer(formula = (rts1_lead) ~ scale(rts1_scale) + Stay1 + SameKey1_lead + 
                       # Transition * RewardType * GROUP1245 +
                       # Transition * RewardType * BlockType +
                       # Transition * GROUP1245 * BlockType +
                       # RewardType * GROUP1245 * BlockType +
                       (Transition + RewardType + BlockType + Group)^3 +   #Use for 4 ways
                       (1 | ID),REML = F,
                     data =  bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),])

anova(rtshark1,rtshark2, type = 'III')

summary(rtshark1)     

car::Anova(rtshark1, type = 'III')
coefs <- data.frame(coef(summary(rtshark1)))
# use normal distribution to approximate p-value
coefs$p.z <- round(2 * (1 - pnorm(abs(coefs$t.value))),3)
coefs

rtshark1_hc<-lme4::lmer(formula = 1/rts1_scale_lead ~ scale(rts1_scale) + scale(rts2_scale) + Stay1 + SameKey1_lead + 
                       # Transition * RewardType * GROUP1245 +
                       # Transition * RewardType * BlockType +
                       # Transition * GROUP1245 * BlockType +
                       # RewardType * GROUP1245 * BlockType +
                      #  Transition * RewardType * GROUP1245 *BlockType +   #Use for 4 ways
                       Transition * RewardType * BlockType +   #HC Model
                       (1 | ID/Run),REML = F,
                     data =  bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),],
                     control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(rtshark1_hc)                      
car::Anova(rtshark1_hc, type = 'III')
coefs <- data.frame(coef(summary(rtshark1_hc)))
# use normal distribution to approximate p-value
coefs$p.z <- round(2 * (1 - pnorm(abs(coefs$t.value))),3)
coefs



ggplot(data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),],aes(y = rts1_lead,x=RewardType,color=Transition))+geom_boxplot()+facet_wrap(~Group+BlockType,ncol = 2)


# multicollinearity diagnostics

# collinearity checks

bdf$RewardType<-plyr::mapvalues(bdf$ifReinf,from = c("TRUE","FALSE"),to = c("Reward","No Reward"))
#Plots
library(emmeans)
emmeansx<-emmeans(m1shark,  ~ RewardType*Transition*BlockType |GROUP1245,data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
    #lower = emmean - SE, 
    #upper = emmean + SE, 
    #middle = emmean, 
    ymin = emmean - SE, 
    ymax = emmean + SE),
    stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~GROUP1245+BlockType,ncol = 2)

emmeansx<-emmeans(m1shark_hc,  ~RewardType*Transition | BlockType,data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, fill=Transition)) + 
  geom_bar(stat = "identity",position = "dodge",color="black") +geom_errorbar(aes(
  #lower = emmean - SE, 
  #upper = emmean + SE, 
  #middle = emmean, 
  ymin = emmean - SE, 
  ymax = emmean + SE),
  stat = "identity",position=position_dodge(.9),width=.2) + facet_wrap(~BlockType,ncol = 2)

emmeansx<-emmeans(rtshark1,  ~ RewardType*Transition*BlockType | GROUP1245 ,data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, color=Transition)) + geom_boxplot(aes(
  lower = emmean - SE, 
  upper = emmean + SE, 
  middle = emmean, 
  ymin = emmean - 3*SE, 
  ymax = emmean + 3*SE),
  stat = "identity") + facet_wrap(~GROUP1245+BlockType,ncol = 2)

ggplot(data=dfemmeans, aes(x=RewardType, y=rts1_lead, color=Transition)) + geom_boxplot(aes(
  lower = emmean - SE, 
  upper = emmean + SE, 
  middle = emmean, 
  ymin = emmean - 3*SE, 
  ymax = emmean + 3*SE),
  stat = "identity") + facet_wrap(~GROUP1245+BlockType,ncol = 2)


emmeansx_rt<-emmeans(rtshark1_hc,  ~RewardType*Transition | BlockType ,data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),])
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

model_formula<-as.formula( paste("ChoiceS1_lead ~ ",paste(addonto,collapse = " + "),"+ (",1," | ID)",sep = "") )
model_sub_formula<-as.formula( paste("ChoiceS1_lead ~ ",paste(addonto,collapse = " + "),"+ (",paste(addonto,collapse = " + ")," | ID)",sep = "") )

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


