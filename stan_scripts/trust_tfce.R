#Convert Lina's trust frirst level into fsl files...
rootdir<-"/Volumes/bek/trust_analyses/ssanal/clinical/"
Groups<-c("HC","ATT","DEP","IDEA")
folderstodo<-list.dirs(rootdir,full.names = F,recursive = F)

pathx<-file.path(rootdir,folderstodo[1])

for (groupx in Groups) {
  eachgrouppath<-file.path(pathx,groupx)
  
  #trust_task_dmUBLOCK_202200+tlrc.BRIK.gz
  
  ls.files<-list.files(path = eachgrouppath,pattern = ".*_[0-9]*.tlrc.BRIK.gz",full.names = T)
  for (filexe in ls.files) {
    infox <-  strsplit(system(paste("3dinfo -label",filexe),intern = T),"|",fixed = T)[[1]]
    for(infoa in 0:(length(infox)-1)){
      dir.create(file.path(dirname(filexe),infox[infoa+1]),showWarnings = F,recursive = T)
      
      system(paste("cd",file.path(dirname(filexe),infox[infoa+1]),"\n","3dAFNItoNIFTI",paste0(filexe,"[",infoa,"]")),intern = F)
    }
    return(infox)
  }
  
  
  outroot<-file.path(gsub("ssanal","grpanal",pathx),groupx)
  
  allcmds<-lapply(infox,function(infoexe){
    sourcedir=file.path(dirname(filexe),infoexe)
    outdir=file.path(outroot,infoexe); dir.create(outdir,showWarnings = F,recursive = T)
    if(!file.exists(file.path(outdir,"onesampleT4D.nii.gz"))){
      system(paste("fslmerge -t",file.path(outdir,"onesampleT4D"),paste(list.files(sourcedir,full.names = T),collapse = " ")))
    }
    if(!file.exists(file.path(outdir,"onesampleT_tfce_corrp_tstat1.nii.gz"))){
      paste("randomise","-i",file.path(outdir,"onesampleT4D.nii.gz"),"-o",file.path(outdir,"onesampleT"),"-1","-T")
    } else {NULL}
  })
  allcmds<-allcmds[!sapply(allcmds,is.null)]
  if(length(allcmds)>0){
    xc<-parallel::makeForkCluster(nnodes = 4)
    parallel::parLapply(cl = xc,X = allcmds,fun = system)
  }
}
# Group
# Age @ time of trust
# Age of first attempt
# Lethality of worst attempt 
# Suicide intent of worst attempt (SIS)
# Sex
# Education
# Race
# HAMBeck
# Lifetime substance use
# Lifetime anxiety disorders
# Andidepressant Exposure
# WTAR scaled
# EXIT
# MDRS
# CIR
# SPSI-ICS
# SIS-planning (of the highest lethality attempt)
# Brain Damage
# Opiod,antipsychotic, sedative/hypnotic exposure.
