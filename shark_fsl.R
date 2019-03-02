#Shark fsl
rootdir = '~/Box/skinner/data/eprime/shark' 

require("devtools")
devtools::install_github("PennStateDEPENdLab/dependlab")
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R")
library("fslpipe")
require("dplyr")
#Script Wide Arguments:
reloaddata<-F
runHConly<-T


parallenum=parallel::detectCores()/2
source('shark_utility.R')

if(reloaddata){
  source('behaviroal/shark_beh_analyses_import_process.R')
} else {load("shark1.RData")}

if(runHConly){bdf<-bdf[which(bdf$GROUP1245==1),]}

shark_fsl_data<-lapply(as.character(unique(bdf$ID)),function(ID){
 return(list(dfx=bdf[which(bdf$ID==ID),]))
})
names(shark_fsl_data)<-as.character(unique(bdf$ID))

argu<-as.environment(list(nprocess=10,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Explore/shark.cfg",
                          regpath="/Volumes/bek/explore/shark/regs",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          proc_id_subs="",regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/explore/shark/ssanalysis",
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

#Model Basic Event Mapping;
argu$model.name<-"BasicModel"
argu$gridpath<-"/Volumes/bek/explore/scripts/shark/grids/basic.csv"
#argu$model.varinames<-c("LeftRight1","Decision1_evt","Decision2_evt","Feedback_evt")

fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="shark_fsl", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=shark_fsl_data #List of ID list of arguments for prep.call.
)




