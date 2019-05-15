#Shark fsl
rootdir = '~/Box/skinner/data/eprime/shark' 

require("devtools")
devtools::install_github("PennStateDEPENdLab/dependlab",force=F)
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F)
library("fslpipe")
require("dplyr")
#Script Wide Arguments:
reloaddata<-F
runGroupComparison<-F
runHConly<-F
excludeID<-c("221680","221036","221036_WRONG")

parallenum=parallel::detectCores()/2
source('shark_utility.R')

if(reloaddata){
  source('behaviroal/shark_beh_analyses_import_process.R')
} else {load("shark1.RData")}

argu<-as.environment(list(nprocess=4,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Explore/shark.cfg",
                          regpath="/Volumes/bek/explore/shark/regs",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          proc_id_subs="",regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/explore/shark/ssanalysis",whichttest=c("onesample"),
                          glvl_outputroot="/Volumes/bek/explore/shark/grpanal",centerscaleall=F,
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                          ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_ssfeat_general_adaptive_template_R.fsf",
                          glvl_output="/Volumes/bek/explore/shark/grpanal",ifoverwrite_secondlvl=FALSE,hig_lvl_path_filter=NULL,
                          graphic.threshold=0.90,nuisa_motion="nuisance",motion_type="fd", motion_threshold="default",convlv_nuisa=F))
#DO NOT PROVIDE THOSE TWO AND IT WILL BE FINE;
argu$randomize_p_threshold<-0.001
argu$randomize_thresholdingways<-c("tfce","voxel-based","cluster-based-extent","cluster-based-mass")
argu$ss_zthreshold<-3.2  #This controls the single subject z threshold (if enabled in template)
argu$ss_pthreshold<-0.05 #This controls the single subject p threshold (if enabled in template)
getRLpara=F




#Model Selection
base_model=F
pe_daw=T
#Model Basic Event Mapping;
if(base_model){
  argu$model.name<-"BasicModel"
  argu$gridpath<-"./grids/basic.csv"
  #argu$model.varinames<-c("LeftRight1","Decision1_evt","Decision2_evt","Feedback_evt")
} 

if(pe_daw){
  argu$model.name<-"PE_MB"
  argu$gridpath<-"./grids/PE_MB.csv"
  argu$centerscaleall<-T
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
IDEatt<-T
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



fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="shark_fsl", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=shark_fsl_data #List of ID list of arguments for prep.call.
)




