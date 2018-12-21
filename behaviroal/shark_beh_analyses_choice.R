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



# model-baseness as a function of shark (threat)
# whether they are going to repeat the stage 1 choice
m1 <- glmer(Stay1_lead ~ 
             Stay1  + SameKey1_lead +  ifRare * ifReinf * GROUP1245 + (ifReinf * ifRare | ID:Run),
            family = binomial(),
            data = bdf[!bdf$Outlier & !bdf$Missed & !as.logical(bdf$ifSharkBlock),],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1)
car::Anova(m1, type = 'III')


m1shark_nf <- glmer(Stay1_lead ~ 
                      Stay1  + SameKey1_lead +  
                      Transition * ifReinf * GROUP1245 + 
                      Transition * ifReinf * BlockType + 
                      Transition * GROUP1245 * BlockType + 
                      ifReinf * GROUP1245 * BlockType + 
                      (ifReinf * ifRare | ID:Run),
                    #(1| ID:Run),
                    family = binomial(),
                    data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),],
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark_nf)
car::Anova(m1shark_nf, type = 'III')

m1shark <- glmer(Stay1_lead ~ 
                   Stay1  + SameKey1_lead +  Transition * ifReinf *BlockType* GROUP1245 + (ifReinf * ifRare | ID:Run),
                 family = binomial(),
                 data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),],
                 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark)
car::Anova(m1shark, type = 'III')



m1shark_hc <- glmer(Stay1_lead ~ 
                      Stay1  + SameKey1_lead +  Transition * ifReinf * BlockType + (ifReinf * ifRare | ID:Run),
                    family = binomial(),
                    data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),],
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark_hc)
car::Anova(m1shark_hc, type = 'III')


m1shark_dg <- glmer(Stay1_lead ~ 
                      Stay1  + SameKey1_lead +  ifRare * ifReinf * depress *ifSharkBlock + (ifReinf * ifRare | ID:Run),
                    family = binomial(),
                    data = bdf[(!bdf$Outlier & !bdf$Missed),],
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark_dg)
car::Anova(m1shark_dg, type = 'III')

m1edu <- glmer(Stay1_lead ~ 
                 Stay1  + SameKey1_lead +  ifRare * ifReinf * GROUP1245 + ifRare*ifReinf*GROUP1245*ifSharkBlock* scale(EDUCATION,center = T) + (ifReinf * ifRare | ID:Run),
               family = binomial(),
               data = bdf[!bdf$Outlier & !bdf$Missed & bdf$GROUP1245 %in% c("1","5"),],
               glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1edu)
car::Anova(m1edu, type = 'III')

m1exit <- glmer(Stay1_lead ~ 
                  Stay1  + SameKey1_lead +  ifRare * ifReinf * GROUP1245 + ifRare*ifReinf*GROUP1245*ifSharkBlock* scale(EXIT.EXITtot,center = T) + (ifReinf * ifRare | ID:Run),
                family = binomial(),
                data = bdf[!bdf$Outlier & !bdf$Missed & bdf$GROUP1245 %in% c("1","5"),],
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1exit)
car::Anova(m1exit, type = 'III')

m1edu_gp <- glmer(Stay1_lead ~ 
                    Stay1  + SameKey1_lead +  ifRare * ifReinf * GROUP1245 + ifRare*ifReinf*GROUP1245*ifSharkBlock* edu_group  + (ifReinf * ifRare | ID:Run),
                  family = binomial(),
                  data = bdf[!bdf$Outlier & !bdf$Missed & bdf$GROUP1245 %in% c("1","5"),],
                  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1edu_gp)
car::Anova(m1edu_gp, type = 'III')

ggplot(bdf[!bdf$Outlier & !bdf$Missed,],mapping = aes(x=ifRare,y=as.numeric(Stay1_lead)-1,color=ifReinf))  + facet_wrap( ~ ifSharkBlock+GROUP1245, ncol=2) +
  stat_summary(fun.y=mean, geom="bar",alpha=0.2) +
  stat_summary(fun.data = mean_cl_boot,geom="errorbar", width=0.1) 



m1_S <- glmer(Stay1_lead ~ 
                SameKey1_lead +  ifRare * ifReinf * ifSharkBlock+
                (ifReinf * ifRare | ID) + (1 | ID:Run),
              family = binomial(),
              data = bdf[!bdf$Outlier,],
              glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1_S)
car::Anova(m1_S, type = 'III')

m1_S_1 <- glmer(Stay1_lead ~ 
                  SameKey1_lead +  ifRare * ifReinf +
                  (1 | ID/Run),
                family = binomial(),
                data = bdf[!bdf$Outlier,],
                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1_S_1)
car::Anova(m1_S_1, type = 'III')

###MORE BASIC MODEL;

# #Let's try total model free, 4 arm bandit;
# m1_free <- glmer(choice1_lead  ~ 
#                      SameKey1_lead +  DecRecode*ifRare * ifReinf * ifSharkBlock  +  
#                (1| ID/Run),
#              family = binomial(),
#              data = bdf[!bdf$Outlier,],
#              glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(m1_free)
# car::Anova(m1_free, type = 'III')

#Star Model Currently:

m1shark <- glmer(Stay1_lead ~ 
                   Stay1  + SameKey1_lead + 
                   Transition * RewardType * GROUP1245 +
                   Transition * RewardType * BlockType +
                   Transition * GROUP1245 * BlockType +
                   RewardType * GROUP1245 * BlockType +
                   #Transition * RewardType * GROUP1245 *BlockType +   #Use for 4 ways
                   + (RewardType * Transition | ID/Run),
                 family = binomial(),
                 data = bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),],
                 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1shark)
car::Anova(m1shark, type = 'III')

m1shark_hc <- glmer(Stay1_lead ~  Stay1  + SameKey1_lead +
                   #Transition * ifReinf * GROUP1245 *BlockType +   #Use for 4 ways
                   Transition * RewardType * BlockType    #HC Model
                   + (RewardType * Transition * BlockType | ID),
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
rtshark1<-lme4::lmer(formula = scale(rts1_scale_lead) ~ scale(rts1_scale) + scale(rts2_scale) + Stay1 + SameKey1_lead + 
                       # Transition * RewardType * GROUP1245 +
                       # Transition * RewardType * BlockType +
                       # Transition * GROUP1245 * BlockType +
                       # RewardType * GROUP1245 * BlockType +
                        Transition * RewardType * GROUP1245 *BlockType +   #Use for 4 ways
                       (RewardType * Transition | ID/Run),REML = F,
                     data =  bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack)),],
                     control=lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                     )
summary(rtshark1)                      
car::Anova(rtshark1, type = 'III')
coefs <- data.frame(coef(summary(rtshark1)))
# use normal distribution to approximate p-value
coefs$p.z <- round(2 * (1 - pnorm(abs(coefs$t.value))),3)
coefs

rtshark1_hc<-lme4::lmer(formula = log(rts1_scale_lead) ~ scale(rts1_scale) + scale(rts2_scale) + Stay1 + SameKey1_lead + 
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

# multicollinearity diagnostics

# collinearity checks
bdf$Transition<-factor(bdf$Transition,levels = c(""))
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

emmeansx<-emmeans(rtshark1,  ~ RewardType*Transition*BlockType | GROUP1245 ,data=bdf[(!bdf$Outlier & !bdf$Missed & !as.logical(bdf$sharkattack) & bdf$GROUP1245==1),])
dfemmeans<-as.data.frame(emmeansx)
ggplot(data=dfemmeans, aes(x=RewardType, y=emmean, color=Transition)) + geom_boxplot(aes(
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



save(list = ls(all.names = TRUE), file = "shark2.RData") 


