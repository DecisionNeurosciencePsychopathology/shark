#Shark FSL Script
#Model Selection
QC     = FALSE
Basic_Model  = FALSE
PE_daw = FALSE
run_HC = FALSE
stage2_pe = TRUE
#Script Wide Arguments:
lvl3_covariate_names = c("Intercept")

#Set up 
#Source the package from github 
require("devtools")
#Michael's pipeline for regressor genration
devtools::install_github("PennStateDEPENdLab/dependlab",force=F)
#Jiazhou's pipeline for analysis
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F)
library("fslpipe")


require("dplyr")
source("shark_utility.R")

#Set up the arguments for analysis pipeline;
argu<-as.environment(list(nprocess=2,    #number of parallel processes
                          run_steps=NULL, #select steps to run; put NULL for running everything
                          forcereg=F,     #Re-run regressor generation?
                          cfgpath="/Volumes/vol1/dnpl/cfg_files/Pitt_explore_shark_aroma_mni7mm.cfg", #Point to config files that are used in pre-proc pipeline
                          rootpath_output = "/Volumes/vol1/dnpl/explore/shark", #root directory for output
                          name_func_nii="nfaswuktm_shark[0-9]*.nii.gz", #regular expression of the final nii file from pre-proc
                          regtype=".1D", #Specify the regressor type in FSL; use .fsl to use the three columns format and .1D to use pre-convolved format
                          adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE, #Do you want to use adaptive feats instead of custom templates
                          #ssfeat_template = "where your template is",
                          centerscaleall=FALSE,tr=0.6,
                          templatedir="/Volumes/vol1/Newtemplate_may18/mni_icbm152_nlin_asym_09c/mni_icbm152_t1_tal_nlin_asym_09c_brain_3mm.nii", #Template brain location; must be the same as preproc
                          motion_type="fd",  #either fd or dvar for motion parameter 
                          motion_threshold="default", #threshold for said parameter 
                          nuisa_motion=c("nuisance","motion_par"), #which nuisance regressors to include
                          convlv_nuisa=F #if to include nuisance regressor
))

#DO NOT PROVIDE THOSE TWO AND IT WILL BE FINE;
#We are refiting lvl2

#overwrite, retry, afnify and type; 
#FALSE, FALSE, TRUE, and "flame" / "randomise"

#prefixes:
#lvl1_; lvl2_; lvl3_
#run_lv; subj_avg; group_lvl;
argu$lvl1_retry<-F
argu$lvl3_type<-"flame"
#Default z value for single subject;
argu$ss_zthreshold<-1.96  #This controls the single subject z threshold (if enabled in template)
argu$ss_pthreshold<-0.05 #This controls the single subject p threshold (if enabled in template)
argu$lvl3_afnify <- TRUE
argu$lvl1_afnify <- TRUE

#PBS argu
argu$run_on_pbs<-F
argu$lvl2_force_prep <- TRUE
##Set up model:
#For each model grid:
#ename: event name; 
#norm: normalization argument: check michael's depend lab package for detail
#normargu: useless 
#valuefrom: value to align with the event
#modifier: used in junction with "style"; multiply, add...
#name: any name you want to use for this regressor;
#style: see modifier;
#comment: useless, note
#AddNeg: TRUE to add another regressor for (-1*regressor);

#Model Basic Event Mapping;
if (QC) {
  argu$model_name <- "QC"
  argu$gridpath <- "./grids/QC.csv"
}

if(Basic_Model){
  argu$model_name<-"Basic_Model_updated"
  argu$gridpath<-"./grids/basic2.csv"
} 

if(PE_daw){
  argu$model.name<-"PE_MB"
  argu$gridpath<-"./grids/PE_MB.csv"
  argu$centerscaleall<-T
  getRLpara<-T
}

if(stage2_pe) {
  argu$model.name<-"stage2_pe"
  argu$gridpath<-"./grids/stage2_pe.csv"
  argu$centerscaleall<-F
  getRLpara<-T
}

design_grid <- read.csv(file = argu$gridpath,stringsAsFactors = F)
#Supply a proc function:
#See "shark_fsl" in "shark_utility.R"

#Input: single subject data (required); and other custom arguments
#Output needs to be a list of the followings:
  #1) event.list: is a list of data.frames that contains timing information;
  #2) output.df: is a data.frame that contains tthe "modifier" variables in the grid
  #3) value: is a data.frame that contains the "valuefrom" variables in the grid


#For trust:
#Grid: 
#Decision1_evt algined to Decision 1
#PE_1 aligned to outcome

#Supply behavioral data:
load(file = "shark_data.RData")
load("explore_subj_df.rdata")


if (run_HC) {
  argu$lvl3_ref_df <- subject_df[which(subject_df$GroupNEW=="HC"),]
} else {
  argu$lvl3_ref_df <- subject_df
}
argu$lvl3_ref_df$ID <- argu$lvl3_ref_df$id
argu$lvl1_proc_nuisance = FALSE
message("Number of subject before clean up: ",length(shark_data_proc))
#First we should remove subject who have 
o_miss_df <- do.call(rbind,lapply(shark_data_proc,shark_summarize,useable_vars=c("Missed","Outlier")))
ex_miss_ID<-o_miss_df$ID[o_miss_df$Type=="Overall" & o_miss_df$not_useable > 0.3]

block_miss_df <- o_miss_df[grep("Block",o_miss_df$Type),]
block_miss_df$Outliers <- block_miss_df$not_useable > 0.2

ID_blocks_ex <- do.call(paste,block_miss_df[block_miss_df$Outliers,c("ID","Type")])
subj_block_df <- aggregate(Outliers~ID,data = block_miss_df,FUN = function(x){length(which(x))/4})
block_max_ex<-subj_block_df$ID[which(subj_block_df$Outliers > 0.5)]


excludeIDs <- block_max_ex
shark_data_proc_exclude<-shark_data_proc[which(!names(shark_data_proc) %in% excludeIDs)]
shark_data_proc_exclude <- shark_data_proc_exclude
message("Number of subject AFTER clean up: ",length(shark_data_proc_exclude))
bdf <- merge(do.call(rbind,shark_data_proc_exclude),subject_df,by.x = "ID",by.y = "id",all.x = T)
bdf <- bdf[which(!paste(bdf$ID,"Block",bdf$Block) %in% ID_blocks_ex),]


# tx<-unlist(lapply(bdf_sp,function(dfx) {
#   dfx$type <- paste(dfx$Transition,dfx$RewardType,sep = " - ")
#   gx<-as.data.frame(table(dfx$type[!dfx$Missed]))
#   
# }))


if(FALSE){
  if(file.exists("shark_rl_df.rdata")){
    load("shark_rl_df.rdata")
  }else {
    stop("Please make sure RL output df is saved in the directory.")
  }
}


getRLpara = FALSE

shark_fsl_data<-lapply(as.character(unique(bdf$ID)),function(ID){
  if(getRLpara) {
    if(!is.null(shark_rl_df[[ID]])){
      return(list(design_grid = design_grid,dfx=bdf[which(bdf$ID==ID),],comboRLpara=getRLpara,RLparadf=shark_rl_df[[ID]]))
    } else {return(NULL)}
  } else {
    return(list(design_grid = design_grid,dfx=bdf[which(bdf$ID==ID),],comboRLpara=getRLpara,RLparadf=NULL))
  }
})
names(shark_fsl_data)<-as.character(unique(bdf$ID))
shark_fsl_data<-cleanuplist(shark_fsl_data)
#shark_fsl_data<-shark_fsl_data[-which(names(shark_fsl_data) %in% excludeID)]

if(FALSE) {
shark_fsl_data<-shark_fsl_data[sapply(shark_fsl_data,function(kr){unique(kr$dfx$Group)==1})]
}


fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="shark_fsl", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=shark_fsl_data #List of ID'ed data frames 
)




