#Shark ML

#To do list: 
#Try to parallize the tuning so it doens't take forever;
#Another way to reduce this is to do PCA on each cat of the data, clinical, demo, neuropsy and task

#Use this as to learn ML I guess. Don't expect to publish this
mldf.l<-shark.ml.proc(bdf = bdf,include.clinical = T,include.lag = F,include.neruopsy = F,include.demo = F,removevar = c("Run","ETHNICITY TEXT","MAX LETHALITY"))
mldf<-mldf.l$data
#mldf<-mldf[mldf$GROUP1245 %in% c("1","4","5"),]
mldf$GROUP1245<-factor(mldf$GROUP1245)
#mldf<-mldf[mldf$state %in% c("2","3"),]
mldf$`MAX LETHALITY`<-as.numeric(mldf$`MAX LETHALITY`)

tofs<-names(mldf)[!names(mldf) %in% c("rts1","rts2","rts1_lag","rts2_lag","EXPLORE AGE","EDUCATION","MAX LETHALITY")]
for (tof in tofs) {
  mldf[[tof]]<-as.factor(mldf[[tof]])
}

toscales<-c("rts1","rts2","rts1_lag","rts2_lag","EXPLORE AGE","EDUCATION")
for (tos in toscales) {
  mldf[[tos]]<-scale(mldf[[tos]],center = T,scale = T)
}


#subset to have only ATT and HC:

mldf<-mldf[order(mldf$ID),]
mllist<-split(mldf,mldf$ID)
sapply(mllist, function(x){length(x[[1]])})

mldf<-reshape(data = na.omit(mldf), timevar = c("trial"),idvar = c("ID","contingency","GROUP1245"),direction = "wide")
#idindx<-data.frame(rn=rownames(mldf),id=mldf$ID)
#mldf$ID<-NULL



set.seed(10000)


dsx<-lapply(c(1,2,4,5),function(gx) {
  glx<-mldf$ID[!duplicated(mldf$ID) & mldf$GROUP1245==gx]
  train <- sample(glx,length(glx)*0.7,replace = FALSE)
  return(list(train=mldf[which(mldf$ID %in% train),],
              valid=mldf[-which(mldf$ID %in% train),]))
})
TrainSet<-do.call(rbind,lapply(dsx,function(dx){dx$train}))
ValidSet<-do.call(rbind,lapply(dsx,function(dx){dx$valid}))

# VTsample <- sample(1:dim(ValidSet)[1],dim(ValidSet)[1]*0.5,replace = FALSE)
# VSet <-ValidSet[VTsample,]
# TestSet <-ValidSet[-VTsample,]
# 
# 
# TrainSet <- mldf[which(rownames(mldf) %in% as.numeric(as.character(idindx$rn[which(idindx$id %in% train)]))),]
# ValidSet <- mldf[-which(rownames(mldf) %in% as.numeric(as.character(idindx$rn[which(idindx$id %in% train)]))),]

#Let's try to use very basic task data to predict group;
#Doesn't consider time vector because we are not there yet, but could include lag variables as one or do like a wide version of the data; 

#Try to use SVM with Multiple Comparison, one vs one approach; 
#Linear SVM:
library(e1071)
#Tunning the cost, cuz for no reasons to believe any out of all
# tune.out=tune(svm,GROUP1245~.,data=TrainSet,kernel="polynomial",
#               ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100,1000,10000),
#                           degree=c(2,3,4,5,6)))
# #summary(tune.out)
# bestmodel<-tune.out$best.model
#rainSet$ID<-NULL
bestmodel<-svm(GROUP1245~., data=TrainSet, kernel="linear", cost=100, scale=FALSE,gamma=0.004694836,type="C-classification",degree=2,cross=10)
#bestmodel<-svm(GROUP1245~., data=TrainSet)

trai<-data.frame(valid=bestmodel$fitted, data=TrainSet$GROUP1245[match(names(bestmodel$fitted),rownames(TrainSet))])
trai$same<-trai$valid==trai$data
length(which(trai$same))/length(trai$valid)

valid.out=predict(bestmodel ,ValidSet)
acu<-data.frame(valid=valid.out, data=ValidSet$GROUP1245[match(names(valid.out),rownames(ValidSet))])
acu$same<-acu$valid==acu$data
# acu$ID<-idindx$id[match(names(valid.out),idindx$rn)]
# acu<-acu[!duplicated(acu$ID),]
length(which(acu$same))/length(acu$valid)

prop.table(
  table(valid=valid.out, data=ValidSet$GROUP1245[match(names(valid.out),rownames(ValidSet))])
  ,1)

w <- t(bestmodel$coefs) %*% bestmodel$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
as.data.frame(w)
#Result in 0.3100741 accuracy; slightly above chance;
#Linear SVM failed to identify ANY IDE or DEP tho
#Adding time lagged variables adds 1% imporvement! 
#Adding trial variable adds another 1%


#Non-linear SVM:
#Polynomial:

#Radial:

ggplot2::ggplot(data = acu,aes(x=data,y=valid))+geom_point()+facet_wrap(~ID)+geom_abline(slope = 1)





