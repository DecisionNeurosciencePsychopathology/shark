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
# whether they are going to repeat the stage 1 choice
m1 <- glmer(stay1lead ~ 
             stay1  + persev1lead + persev2 +  money * common_trials + shark  +  
              (money * common_trials | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1)
car::Anova(m1, type = 'III')

m1g <- glmer(stay1lead ~ 
              stay1  + persev1lead + persev2 +  money * common_trials * GROUP12467 + shark * GROUP12467  +  
              (money * common_trials | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m1g)
car::Anova(m1g, type = 'III')


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
              stay1  + persev1 + same_planet * money_lag * shark  +  
              ( 1 | id/run),
            family = binomial(),
            data = bdf[!bdf$outlier,],
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(m3)
car::Anova(m3, type = 'III')
lsm <- lsmeans::lsmeans(m3, "common_trials", by = c("common_lag", "money_lag"))
plot(lsm, horiz = F)

lsm <- lsmeans::lsmeans(m3, "same_planet", by = c("money_lag"))
plot(lsm, horiz = F)



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

rt1g <- lme4::lmer(log(rts1) ~ log(rts1_lag) + log(rts2_lag) +
                    persev1 +  money_lag * common_lag * GROUP12467 + money_lag * shark * GROUP12467 +  
                    (1 | id/run),
                  data = bdf[!bdf$outlier,])
summary(rt1g)                      
car::Anova(rt1g, type = 'III')


rt2 <- lme4::lmer(log(rts2) ~ log(rts1) + log(rts2_lag) + common_trials +
                    stay1  + persev1 + same_planet * money_lag + shark  +  
                    ( 1 | id/run),
                  data = bdf[!bdf$outlier,])
summary(rt2)                      
car::Anova(rt2, type = 'III')

# multicollinearity diagnostics


# collinearity checks
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

save(list = ls(all.names = TRUE), file = "shark2.RData") 


