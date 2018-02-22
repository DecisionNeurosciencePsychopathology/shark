# import, check and prepare for analyses data from two bandit samples

setwd("~/Box Sync/skinner/projects_analyses/Project Shark/processed_data")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(ggbiplot)
library(corrplot)
library(lsmeans)
library(factoextra)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)
library(multcompView)
library(stargazer)

###################

load(file = "shark1.RData") 

# model-baseness as a function of shark (threat)
m1 <- glmer(stay1lead ~ 
             stay1  + persev1lead + persev2 +  money * common_trials + shark  +  
              (money * common_trials | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1)
car::Anova(m1, type = 'III')

m2 <- glmer(money ~ 
              persev1 + persev2 + money_lag*common_lag*stay1*stay2 + common_trials + shark  +  
              (1 | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m2)
car::Anova(m2, type = 'III')
lsm <- lsmeans::lsmeans(m2, "money_lag", by = c("common_lag", "stay1"))
plot(lsm, horiz = F)

m3 <- glmer(stay2 ~ 
              stay1  +   money_lag * common_lag * common_trials + shark  +  
              (money * common_trials | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m3)
car::Anova(m3, type = 'III')


lsm <- lsmeans::lsmeans(m1, "money", by = "common_trials")
plot(lsm, horiz = F)

rt1 <- lme4::lmer(log(rts1) ~ log(rts1_lag) + log(rts2_lag) +
                        persev1 +  money_lag * common_lag + money_lag * shark +  
                        (1 | id/run),
                        data = bdf[!bdf$outlier,])
summary(rt1)                      
car::Anova(rt1, type = 'III')

lsm <- lsmeans::lsmeans(rt1, "money_lag", by = "common_lag")
plot(lsm, horiz = F)

rt2 <- lme4::lmer(log(rts2) ~ log(rts1) + log(rts2_lag) + persev1 +
                    persev2 +  money_lag * common_lag  + shark + common_trials + 
                    (1 | id/run),
                  data = bdf[!bdf$outlier,])
summary(rt2)                      
car::Anova(rt2, type = 'III')


