###


library(dependlab)

ID = "211101"
img_dir = "/Users/jiazhouchen/MRI_example/MR_Proc/mni_7mm_aroma"
outdir = "/Users/jiazhouchen/MRI_example/"
load("shark_data.RData")
dfx <- bdf[which(bdf$ID == ID),]
dfx$Trial<-as.numeric(unlist(lapply(split(dfx$trial,dfx$Run),seq_along)))
dfx<-dfx[order(dfx$Trial),]
finalist<-list(Decision1=data.frame(event="Decision1",
                                    onset=dfx$stim1.ons.ms/1000,
                                    duration=dfx$rts1,
                                    run=dfx$Run,
                                    trial=dfx$Trial),
               Decision2=data.frame(event="Decision2",
                                    onset=dfx$stim2.ons.ms/1000,
                                    duration=dfx$rts2,
                                    run=dfx$Run,
                                    trial=dfx$Trial),
               Outcome=data.frame(event="Outcome",
                                  onset=dfx$rew.ons.ms/1000,
                                  duration=1.5,
                                  run=dfx$Run,
                                  trial=dfx$Trial)
)
finalist[["allconcat"]]<-do.call(rbind,finalist)

winlose_lg_df <- data.frame(trial=dfx$Trial,run=dfx$Run,value=c(NA,dfx$won[2:nrow(dfx)]),stringsAsFactors = F)

signals <- list(
  decision1_evt=list(event="Decision1", value=1, normalization="none"),
  decision1_winlose_lag=list(event="Decision1", 
                             value=winlose_lg_df,
                             normalization="none")
)

# read.nifti.hdr
# 
# nvol <- paste0("fslinfo ",)

?dependlab::build_design_matrix
output$design<-dependlab::build_design_matrix(
  center_values=TRUE,
  events = finalist$allconcat,
  signals = signals,
  write_timing_files = c("convolved", "FSL","AFNI"),
  tr=as.numeric(argu$cfg$preproc_call$tr),
  plot = F,
  run_volumes = run_volum,
  output_directory = file.path(outdir,"reg")
  )

