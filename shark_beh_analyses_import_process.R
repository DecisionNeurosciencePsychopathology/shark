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
# merge and check
trial_df <-
  read_csv("shark_df1.csv")

View(trial_df)
sub_df <-
  read_csv("shark_df2.csv")
View(sub_df)
sub_df = sub_df %>% as_tibble %>% arrange(id)

# check missing data
missing_ind_chars = aggr(
  sub_df,
  col = mdc(1:2),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(sub_df),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# all missingness <8%, could impute

# merge trial-by-trial and subject-level data
bdf <- merge(trial_df, sub_df)

names(bdf)

bdf$money <- as.factor(bdf$money==1)
bdf$common_trials <- as.factor(bdf$common_trials==1)
bdf$id <- as.factor(bdf$id)
bdf$shark <- as.factor(bdf$shark)
# leads and lags
bdf = bdf %>% as_tibble %>% arrange(id, trial)
bdf = bdf %>% group_by(id) %>%
  mutate(
    choice1_lag = lag(choice1),
    choice2_lag = lag(choice2),
    choice1_lead = lead(choice1),
    choice2_lead = lead(choice2),
    keycode1_lag = lag(keycode1),
    keycode2_lag = lag(keycode2),
    keycode1_lead = lead(keycode1),
    keycode2_lead = lead(keycode2),
    money_lag = lag(money),
    state_lag = lag(state),
    rts1_lag = lag(rts1),
    rts2_lag = lag(rts2),
    common_lag = lag(common_trials)
  ) %>% ungroup()

bdf$run = ceil(bdf$trial/25)

# stay and motor perseveration
bdf$stay1 <- as.factor(bdf$choice1 == bdf$choice1_lag)
bdf$stay1lead <- as.factor(bdf$choice1 == bdf$choice1_lead)
bdf$persev1 <- as.factor(bdf$keycode1 == bdf$keycode1_lag)
bdf$persev2 <- as.factor(bdf$keycode2 == bdf$keycode2_lag)
bdf$persev1lead <- as.factor(bdf$keycode1 == bdf$keycode1_lead)
bdf$persev2lead <- as.factor(bdf$keycode2 == bdf$keycode2_lead)
bdf$stay2 <- as.factor(bdf$choice2 == bdf$choice2_lag)
bdf$stay2lead <- as.factor(bdf$choice2 == bdf$choice2_lead)


# inspect RTs

hist(bdf$rts1,1000)
hist(bdf$rts2,1000)

# missed trials -- 2-3%
mean(bdf$rts1==0)
mean(bdf$rts2==0)

bdf$missed <- bdf$rts1==0 | bdf$rts2==0
bdf$outlier <- bdf$rts1<.2 | bdf$rts2<.2 | bdf$rts1 > 4 | bdf$rts2 > 4

# check for stereotypical responding

ggplot(bdf,aes(x = trial, y = keycode1)) + geom_line() + facet_wrap(~id)

ggplot(bdf,aes(x = trial, y = keycode2)) + geom_line() + facet_wrap(~id)


save(list = "bdf", file = "shark1.RData") 
