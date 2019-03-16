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

################
#Shark Data Analysis; Model Free Part
#setwd(file.path(getwd(),"behaviroal"))
rootdir = '/Users/jiazhouchen/Box/skinner/data/eprime/shark' 

library(dplyr)
#Functions:
#stay/switch: 
source("shark_utility.R")


#########################################
#########ACTUAL RUN $####################
#########################################

shark_ID<-list.dirs(rootdir,full.names = F,recursive = F)
#This is way too slow, Imma use parallel:
if(file.exists("shark_rawdata.rdata")){
load("shark_rawdata.rdata")} else {shark_data<-list()}
shark_ID<-shark_ID[!shark_ID %in% names(shark_data)]
if(length(shark_ID)>0){
cl<-parallel::makeForkCluster(nnodes = 4)
shark_data_new<-parallel::parLapply(cl,shark_ID,function(x) {
  if (length(list.files(file.path(rootdir,x),'*_workspace_ouput.mat'))>0) {
    print(x)
    tryCatch({
    datain<-R.matlab::readMat(file.path(rootdir,x,list.files(file.path(rootdir,x),'*_workspace_ouput.mat')))
    datain$ID<-x
    },error=function(e){print(e)})
    return(datain)} else {return(NULL)}
})
parallel::stopCluster(cl)
names(shark_data_new)<-shark_ID
shark_data<-c(shark_data,shark_data_new)
save(shark_data,file = "shark_rawdata.rdata")
}
shark_data<-lapply(cleanuplist(shark_data),shark_mat)
shark_data_proc <- lapply(shark_data,shark_proc)

#plot(sapply(shark_data_proc,function(xj){length(which(xj$choice1==0)) / length(xj$choice1)}))

# NUX<-lapply(xshark_lessthanone,function(xj){
#   jpeg(paste0(unique(xj$ID),'_plot.jpeg'),width = 8,height = 4.5,res = 300,units = "in")
#   plot(xj$trial,xj$choice1,main = unique(xj$ID))
#   points(xj$trial[xj$choice1==2 & xj$ifWon],xj$won[xj$choice1==2 & xj$ifWon]+1,col="red",pch=20)
#   points(xj$trial[xj$choice1==1 & xj$ifWon],xj$won[xj$choice1==1 & xj$ifWon],col="red",pch=20)
#   points(xj$trial[xj$ifRare],xj$ifRare[xj$ifRare]+0.5,col="blue",pch=20)
#   dev.off()
# })
message("Number of subject before clean up: ",length(shark_data_proc))
shark_data_proc_exclude<-cleanuplist(lapply(shark_data_proc,shark_exclusion, missthres=0.5,P_staycomreinfchance=0.00,comreinfstaythres=0.0,returnstats=F))
message("Number of subject AFTER clean up: ",length(shark_data_proc_exclude))


#shark_wP<-lapply(shark_data_proc_exclude,genProbability,condition=c('ifRare','ifReinf'),response=c("ifSwitched1"))

#shark_allP<-do.call(rbind,shark_wP)
#rownames(shark_allP)<-NULL

#shark_switchrate<-sapply(shark_wP,function(x) {(x$p[x$ifWon=="Won" & x$ifSwitched2=="Not_Switched2" & x$ifRare=="Not_Rare"]/x$p[x$ifWon=="Won" & x$ifSwitched2=="Switched2" & x$ifRare=="Not_Rare"])})

#############################
bdf <- do.call(rbind,shark_data_proc_exclude)

# stay and motor perseveration


# inspect RTs

#hist(bdf$rts1,1000)
#hist(bdf$rts2,1000)

# missed trials -- 2-3%
#mean(bdf$rts1==0)
#mean(bdf$rts2==0)







DEMO<-readxl::read_xlsx('./behavioral/ALL_SUBJECTS_DEMO.xlsx')
sDEMO<-DEMO[c('ID','COMMENT','EXPLORE','EXPLORE AGE','EXPLORE TERM','MAX LETHALITY','GENDER TEXT',
              'ETHNICITY TEXT','RACE TEXT','EDUCATION','MARITAL TEXT','DOB','GROUP12467','GROUP1245')]
demovars<-names(sDEMO)
#Some of them don't have scan dates...let's get it from RedCap...
source("/Users/jiazhouchen/Documents/UPMC/RStation/Jiazhou.Startup.R")
jiazhou.startup()
exp_scandate<-bsrc.getform(formname = 'explore',protocol = ptcs$scandb,grabnewinfo = T)[c('registration_redcapid','explore_scandate','explore_group')]
sDEMO<-merge(x=sDEMO,y=exp_scandate,by.y = 'registration_redcapid',by.x = 'ID',all = T)
sDEMO$GROUP12467[is.na(sDEMO$GROUP12467)]<-sDEMO$explore_group[is.na(sDEMO$GROUP12467)]
sDEMO$Group<-sDEMO$GROUP12467
sDEMO$Group[as.numeric(sDEMO$GROUP12467) > 5]<-5

bdf<-merge(x=bdf,y=sDEMO,by.x = 'ID',by.y = 'ID',all.x = T)
bdf$GROUP12467[is.na(bdf$GROUP12467)]<-bdf$explore_group[is.na(bdf$GROUP12467)]
#idmap<-data.frame(og=unique(bdf$id)[!as.character(unique(bdf$id)) %in% sDEMO$ID][paste0("2",unique(bdf$id)[!as.character(unique(bdf$id)) %in% sDEMO$ID]) %in% sDEMO$ID],
#                  nw=paste0("2",unique(bdf$id)[!as.character(unique(bdf$id)) %in% sDEMO$ID])[paste0("2",unique(bdf$id)[!as.character(unique(bdf$id)) %in% sDEMO$ID]) %in% sDEMO$ID])
#bdf$id<-plyr::mapvalues(as.character(bdf$id),from = as.character(idmap$og),to=as.character(idmap$nw),warn_missing = F)
bdf$depress <- as.factor(as.numeric(bdf$Group)>1) #NON-CONTROL


bdf$GROUP1245<-as.factor(bdf$GROUP1245)
bdf$GROUP12467<-as.factor(bdf$GROUP12467)
bdf$Group<-as.factor(bdf$Group)
bdf$ID <- as.factor(bdf$ID)
bdf$`GENDER TEXT`<-as.factor(bdf$`GENDER TEXT`)

bdf$edu_group[which(bdf$EDUCATION>=16)]<-'HIGH'
bdf$edu_group[which(bdf$EDUCATION<16)]<-'LOW'
bdf$edu_group<-as.factor(bdf$edu_group)
#ExtraStuff
DRS<-readxl::read_xlsx('./behavioral/DRS.xlsx')
EXIT<-readxl::read_xlsx('./behavioral/EXIT.xlsx')
EXIT<-EXIT[-which(EXIT$NPCODE>900),]
WTAR<-readxl::read_xlsx('./behavioral/WTAR.xlsx')



#Define function that cleans up the data so we have one data per subject for each form;
subdata<-lapply(unique(as.character(bdf$ID)),function(ID) {
  lsdata=list(DRS=DRS,EXIT=EXIT,WTAR=WTAR)
  #first we order by id then spilt it;
  scandate<-unique(bdf$explore_scandate[bdf$ID==ID])
  lsxdata<-lapply(lsdata, function(x) {
    if ("WTARRAW" %in% names(x)) {ignorelimt=T} else {ignorelimt=F}
    j<-getdata(scdate = scandate,xdata = x[which(x$ID==ID),],limt = 500,ignorelimt=ignorelimt)
    if (any(!j$status)) {print(paste0('This ID: ',ID))}
    j$status<-NULL
    return(j)
  })
  lsxdata$ID<-ID
  return(lsxdata)
})
names(subdata)<-unique(as.character(bdf$ID))


allsub_neuropsy<-do.call(rbind,lapply(subdata,function(yx) {
  #yx$ID<-NULL
  return( do.call(cbind,yx))
}))

allsub_neuropsy$EXIT.NPCODE<-NULL
allsub_neuropsy$DRS.MISSCODE<-NULL

dx_missingneuropsy<-apply(allsub_neuropsy, 1, function(x) {
  if (length(which(is.na(x)))>0) {
    xj<-data.frame(formname=unique(sapply(strsplit(names(allsub_neuropsy)[which(is.na(x))],split = ".",fixed = T),"[[",1)))
    xj$ID<-x[length(x)]
    return(xj)
  } else {return(NULL)}
  })
dx_missingneuropsy[sapply(dx_missingneuropsy, is.null)] <- NULL
miss_neuro<-do.call(rbind,dx_missingneuropsy)
miss_neuro$MISS<-TRUE
miss_neuro_wide<-reshape(miss_neuro,v.names = 'MISS',idvar = 'ID',direction = 'wide',timevar = 'formname',sep = "_")
miss_neuro_wide[is.na(miss_neuro_wide)]<-""
rownames(miss_neuro_wide)<-NULL


miss_neuro_wide
DRS[match(miss_neuro_wide$ID,DRS$ID),]
EXIT[match(miss_neuro_wide$ID,EXIT$ID),]
bdf[match(miss_neuro_wide$ID,bdf$ID),c('ID','explore_scandate')]
write.csv(miss_neuro_wide,'missing_neuropsy.csv')

demo_explore<-merge(x=allsub_neuropsy,y=sDEMO,all = T)
write.csv(demo_explore,"demo_explore.csv")
neruopsyvars<-names(allsub_neuropsy)
bdf<-merge(bdf,allsub_neuropsy,all.x = T)



# check for stereotypical responding

# ggplot(bdf,aes(x = trial, y = keycode1)) + geom_line() + facet_wrap(~id)
# 
# ggplot(bdf,aes(x = trial, y = keycode2)) + geom_line() + facet_wrap(~id)


save(list = "bdf", file = "shark1.RData") 
