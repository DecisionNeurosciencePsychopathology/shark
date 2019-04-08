#Older behav stuff:

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