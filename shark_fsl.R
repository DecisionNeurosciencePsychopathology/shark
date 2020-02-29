#Shark FSL Script
#Model Selection
QC     = TRUE
basic  = FALSE
pe_daw = FALSE

#Script Wide Arguments:
runHConly<-F
getRLpara<-F
run_step=c(1,2,6)
lvl3_covariate_names = c("Intercept")
excludeID<-c("221680","221036","221036_WRONG")

require("devtools")
devtools::install_github("PennStateDEPENdLab/dependlab",force=F)
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F)
library("fslpipe")
require("dplyr")

argu<-as.environment(list(nprocess=12,run_steps=run_step,forcereg=F,
                          cfgpath="/gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/cfg_files/exolore/Pitt_explore_shark_aroma_mni7mm.cfg",
                          rootpath_output = "/gpfs/group/LiberalArts/default/mnh5174_collab/explore/shark",
                          name_func_nii="nfaswuktm_shark[0-9]*.nii.gz",
                          regtype=".1D", adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,centerscaleall=FALSE,
                          templatedir="/gpfs/group/mnh5174/default/lab_resources/standard/mni_icbm152_nlin_asym_09c/mni_icbm152_t1_tal_nlin_asym_09c_brain_3mm.nii",
                          graphic.threshold=0.95,nuisa_motion=c("nuisance","motion_par"),motion_type="fd", motion_threshold="default",convlv_nuisa=F
))
#DO NOT PROVIDE THOSE TWO AND IT WILL BE FINE;
#We are refiting lvl2
argu$lvl2_overwrite<-F
argu$lvl3_overwrite<-F
argu$lvl1_retry<-T
argu$lvl3_afnify <- T
argu$lvl1_afnify <- T
argu$lvl3_type<-"flame"
argu$ss_zthreshold<-1.96  #This controls the single subject z threshold (if enabled in template)
argu$ss_pthreshold<-0.05 #This controls the single subject p threshold (if enabled in template)

#PBS argu
argu$run_on_pbs<-T

#Model Basic Event Mapping;
if (QC) {
  argu$model_name <- "QC"
  argu$gridpath <- "./grids/QC.csv"
}

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

laod("shark_data.RData")
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


fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="shark_fsl", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=shark_fsl_data #List of ID list of arguments for prep.call.
)




