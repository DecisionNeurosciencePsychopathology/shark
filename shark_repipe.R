###Cleaning pipeline:

rootdir="/Volumes/bek/explore/MR_Proc"
paradname="shark_proc"
paradname2="shark"
alldirs<-list.dirs(rootdir,recursive = F)
alldirsdf<-do.call(rbind,lapply(alldirs,function(d){
  dx<-list.files(path = file.path(d,paradname),pattern = paradname2,full.names = T)
  if(length(dx)>0){
  rx<-data.frame(paths=dx,stringsAsFactors = F)
  rx$ID<-basename(d)
  return(rx)
  } else {return(NULL)}
})
)


alldirsdf$preprocver<-sapply(alldirsdf$paths,function(gra){
  if(file.exists(file.path(gra,".preprocessfunctional_complete")) ){
    proclog<-readLines( file.path(gra,".preprocessfunctional_complete") )
    gsub(" ","",(gsub("# preprocessFunctional v","", proclog[2])))
  } else {
    NA
  }

  })

torundf<-alldirsdf[which(alldirsdf$preprocver=="2018/7/30"),]
torundf$donzo<-FALSE

#We will let throndike do the first 60 and mine do the rest
fslpipe::fsl_2_sys_env()
cl<-parallel::makeCluster(2,type = "FORK")
NX<-parallel::parSapply(cl,1:nrow(torundf),function(i){
  torundf$paths[i]->ipath
  message(ipath)
  cmda<-gsub("REPLACETHISPATH",ipath,
            "cd REPLACETHISPATH
            echo REPLACETHISPATH
fslmaths swudktm_REPLACETHISNUM_7 -mul wudktm_REPLACETHISNUM_extents_mask swudktm_REPLACETHISNUM_7 -odt float
fslmaths 'swudktm_REPLACETHISNUM_7' -Tmean tempMean
fslmaths 'swudktm_REPLACETHISNUM_7' -bptf 70.7714 -1 -add tempMean 'fswudktm_REPLACETHISNUM_7'
date > .temporal_filtering_complete
fslmaths 'fswudktm_REPLACETHISNUM_7' -Tmean 'fswudktm_mean_float' -odt float
fslmaths 'fswudktm_REPLACETHISNUM_7' -mul 100 -div fswudktm_mean_float 'nfswudktm_REPLACETHISNUM_7' -odt float
date > .rescaling_complete"
  )
  cmd<-gsub("REPLACETHISNUM",basename(ipath),cmda)
  if(!file.exists(file.path(ipath,"tempMean.nii.gz"))){
  system(cmd,intern = F)
  }
  torundf$donzo[i]<-TRUE
})
parallel::stopCluster(cl)

unique(torundf$ID)

lapply(list.dirs("/Volumes/bek/explore/shark/ssanalysis",recursive = F,full.names = T),function(yu){
  alldl<-list.dirs(yu,full.names = T,recursive = F)[which(list.dirs(yu,full.names = F,recursive = F) %in% unique(torundf$ID))]
  if(length(alldl)>0){
  unlink(alldl,recursive = T,force = T)
  }
})










