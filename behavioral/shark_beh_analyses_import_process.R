# import, check and prepare for analyses data from two bandit samples

#setwd("~/Box Sync/skinner/projects_analyses/Project Shark/processed_data")



# if (F) {
# missing_ind_chars = aggr(
#   sub_df[,45:72],
#   col = mdc(1:2),
#   numbers = TRUE,
#   sortVars = TRUE,
#   labels = names(sub_df[,45:72]),
#   cex.axis = .7,
#   gap = 3,
#   ylab = c("Proportion of missingness", "Missingness Pattern")
# )

# all missingness <8%, could impute

# merge trial-by-trial and subject-level data
# bdf <- merge(trial_df, sub_df)
# 
# names(bdf)
# }
library(dplyr)
################
#Shark Data Analysis; Model Free Part
#setwd(file.path(getwd(),"behaviroal"))
rootdir = '~/Box/skinner/data/MRI/shark' 
#Functions:
#stay/switch: 
source("shark_utility.R")


#########################################
#########ACTUAL RUN #####################
#########################################

shark_ID<-list.dirs(rootdir,full.names = F,recursive = F)
#This is way too slow, Imma use parallel:
if(file.exists(file.path(rootdir,"shark_rawdata.rdata"))){
  load(file.path(rootdir,"shark_rawdata.rdata"))
} else if (file.exists(file.path("shark_rawdata.rdata"))) {
  load(file.path("shark_rawdata.rdata"))
} else {
  shark_data<-list()
}
shark_ID<-shark_ID[!shark_ID %in% names(shark_data)]
if(length(shark_ID)>0){
  clxg<-parallel::makeForkCluster(nnodes = 4)
  #shark_data_new<-parallel::parLapply(clxg,shark_ID,function(x) {
  shark_data_new<-lapply(shark_ID,function(x) {
    if (length(list.files(file.path(rootdir,x),'*_workspace_ouput.mat'))>0) {
      print(x)
      tryCatch({
        datain<-R.matlab::readMat(file.path(rootdir,x,list.files(file.path(rootdir,x),'*_workspace_ouput.mat')))
        datain$ID<-x
      },error=function(e){print(e)})
      return(datain)} else {return(NULL)}
  })
  parallel::stopCluster(clxg)
  names(shark_data_new)<-shark_ID
  shark_data<-c(shark_data,shark_data_new)
  save(shark_data,file = "shark_rawdata.rdata")
  save(shark_data,file = file.path(rootdir,"shark_rawdata.rdata"))
}
shark_data<-lapply(cleanuplist(shark_data),shark_mat)
shark_data_proc <- lapply(shark_data,shark_proc,rt_range=c(0.2,4))

#plot(sapply(shark_data_proc,function(xj){length(which(xj$choice1==0)) / length(xj$choice1)}))




#shark_wP<-lapply(shark_data_proc_exclude,genProbability,condition=c('ifRare','ifReinf'),response=c("ifSwitched1"))

#shark_allP<-do.call(rbind,shark_wP)
#rownames(shark_allP)<-NULL

#shark_switchrate<-sapply(shark_wP,function(x) {(x$p[x$ifWon=="Won" & x$ifSwitched2=="Not_Switched2" & x$ifRare=="Not_Rare"]/x$p[x$ifWon=="Won" & x$ifSwitched2=="Switched2" & x$ifRare=="Not_Rare"])})

#############################

#hist(bdf$rts1,1000)
#hist(bdf$rts2,1000)

# missed trials -- 2-3%
#mean(bdf$rts1==0)
#mean(bdf$rts2==0)

#Get the clinical data:
if(F){
  source("./behavioral/shark_explore_clini_pull.R")
}
load("explore_subj_df.rdata")




# check for stereotypical responding

# ggplot(bdf,aes(x = trial, y = keycode1)) + geom_line() + facet_wrap(~id)
# 
# ggplot(bdf,aes(x = trial, y = keycode2)) + geom_line() + facet_wrap(~id)


save(list = c("shark_data_proc","subject_df"), file = "shark_data.RData") 

