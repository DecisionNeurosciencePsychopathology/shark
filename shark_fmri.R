#shark fMRI outside of fsl analysis 
library(fslpipe)
#Use mask gained from HC to see if we get group results
Basic_ROI<-roi_getvalue(rootdir="/Volumes/bek/explore/shark/ssanalysis",
                        grproot="/Volumes/bek/explore/shark/grpanal",
                        modelname="BasicModel",forceconcat=T,
                        basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                        roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)

