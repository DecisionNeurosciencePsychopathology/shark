#Shark fsl
rootdir = '~/Box/skinner/data/eprime/shark' 

require("devtools")
#devtools::install_github("PennStateDEPENdLab/dependlab",force=F)
#devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F)
library("fslpipe")
require("dplyr")
#Script Wide Arguments:
reloaddata<-F
runGroupComparison<-T
runHConly<-F
excludeID<-c("221680","221036","221036_WRONG")

parallenum=parallel::detectCores()/2
source('shark_utility.R')

if(reloaddata){
  source('behaviroal/shark_beh_analyses_import_process.R')
} else {load("shark1.RData")}

getRLpara=F
argu<-list(path.cfg="/Volumes/bek/autopreprocessing_pipeline/Explore/shark.cfg",funcimg.runnumpatt="nfswudktm.*([0-9])_[0-9].nii.gz",
           funcimg.namepatt="nfswudktm*[0-9]_[0-9].nii.gz",path.outroot="/Volumes/bek/explore/shark",
           ssub.func=shark_fsl
           )

#Model Selection
base_model=F
pe_daw=T
#Model Basic Event Mapping;
if(base_model){
  argu$modelname<-"BasicModel"
  argu$path.lvl1grid<-"./grids/basic.csv"
  #argu$model.varinames<-c("LeftRight1","Decision1_evt","Decision2_evt","Feedback_evt")
} 

if(pe_daw){
  argu$modelname<-"PE_MB"
  argu$path.lvl1grid<-"./grids/PE_MB.csv"
  argu$lvl1.CenterScaleAll<-T
  getRLpara<-T
}

if(getRLpara){
  if(file.exists("shark_rl_df.rdata")){
  load("shark_rl_df.rdata")
  }else {
    stop("Please make sure RL output df is saved in the directory.")
  }
}

shark_fsl_data<-lapply(as.character(unique(bdf$ID)),function(ID){
  if(getRLpara) {
    if(!is.null(shark_rl_df[[ID]])){
      return(list(dfx=bdf[which(bdf$ID==ID),],comboRLpara=getRLpara,RLparadf=shark_rl_df[[ID]]))
    } else {return(NULL)}
  } else {
    return(list(dfx=bdf[which(bdf$ID==ID),],comboRLpara=getRLpara,RLparadf=NULL))
  }
})
names(shark_fsl_data)<-as.character(unique(bdf$ID))
shark_fsl_data<-cleanuplist(shark_fsl_data)
shark_fsl_data<-shark_fsl_data[-which(names(shark_fsl_data) %in% excludeID)]
if(runHConly) {
shark_fsl_data<-shark_fsl_data[sapply(shark_fsl_data,function(kr){unique(kr$dfx$Group)==1})]
}


cliniHC<-F
attHC<-F
IDEatt<-F
attDEP<-T
if(runGroupComparison){
  refdf<-unique(bdf[c("ID","Group")])
  names(refdf)<-c("ID","grp")
  refdf<-refdf[!refdf$ID %in% excludeID,]
  
  if(cliniHC){
  #Clinical against controls:
  refdf$grp<-as.numeric(as.numeric(as.character(refdf$grp))>1)+1
  refdf$grp[refdf$grp==1]<-"HC"
  refdf$grp[refdf$grp==2]<-"CLINICAL"
  }
  if(attHC){
    refdf<-refdf[refdf$grp %in% c(1,5),]
    refdf$grp<-as.character(refdf$grp)
    refdf$grp[refdf$grp==1]<-"HC"
    refdf$grp[refdf$grp==5]<-"ATT"
  }
  if(IDEatt){
    refdf<-refdf[refdf$grp %in% c(4,5),]
    refdf$grp<-as.character(refdf$grp)
    refdf$grp[refdf$grp==4]<-"IDE"
    refdf$grp[refdf$grp==5]<-"ATT"
  }
  if(attDEP){
    refdf<-refdf[refdf$grp %in% c(2,5),]
    refdf$grp<-as.character(refdf$grp)
    refdf$grp[refdf$grp=='2']<-"DEP"
    refdf$grp[refdf$grp=='5']<-"ATT"
  }
  # allIDs<-list.dirs(path = file.path(argu$ssub_outputroot,argu$model.name),recursive = F,full.names = F)
  # allIDs<-allIDs[!allIDs %in% c("221036_WRONG","221036")]
  # bdf$Group<-as.character(bdf$Group)
  # 
  # 
  # #Fix it before going in;
  #refdf<-data.frame(ID=allIDs,grp=bdf$Group[match(allIDs,bdf$ID)],stringsAsFactors = F)
  # refdf$grp[refdf$grp!=1]<-"DEP"
  # refdf$grp[refdf$grp==1]<-"HC"
  #allbpd against controls:
  
  argu$supplyidmap<-lapply(split(refdf,refdf$grp),function(rx){
    list(ID=rx$ID,name=unique(rx$grp))
  })
  
  argu$whichttest<-c("unpaired")
  argu$group_id_sep<-unique(refdf$grp)
  print(sapply(argu$supplyidmap,function(x){length(x$ID)}))
  
  #We can now call argu to do it again; 
  #argu$onlyrun<-5:6
}
argu$ssub.datalist<-shark_fsl_data
argu<-check_argu(argu)
stop()



fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="shark_fsl", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=shark_fsl_data #List of ID list of arguments for prep.call.
)




