#Shark ML

#To do list: 
#Try to parallize the tuning so it doens't take forever;
#Another way to reduce this is to do PCA on each cat of the data, clinical, demo, neuropsy and task

#Use this as to learn ML I guess. Don't expect to publish this
mldf.l<-shark.ml.proc(bdf = bdf,include.clinical = T,include.lag = T,include.neruopsy = F,include.demo = T,removevar = c("ETHNICITY TEXT","MAX LETHALITY"))
mldf<-mldf.l$data
#mldf<-mldf[mldf$GROUP1245 %in% c("1","4","5"),]
mldf$GROUP1245<-factor(mldf$GROUP1245)
mldf<-mldf[mldf$state %in% c("2","3"),]
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

#mldf<-reshape(data = na.omit(mldf), timevar = c("trial"),idvar = c("ID","contingency","GROUP1245",mldf.l$demo),direction = "wide")
idindx<-data.frame(rn=rownames(mldf),id=mldf$ID)
mldf$ID<-NULL



set.seed(100)
train <- sample(nrow(mldf), 0.7*nrow(mldf), replace = FALSE)
TrainSet <- mldf[train,]
ValidSet <- mldf[-train,]

#Let's try to use very basic task data to predict group;
#Doesn't consider time vector because we are not there yet, but could include lag variables as one or do like a wide version of the data; 

#Try to use SVM with Multiple Comparison, one vs one approach; 
#Linear SVM:
library(e1071)
#Tunning the cost, cuz for no reasons to believe any out of all
#tune.out=tune(svm,GROUP1245~.,data=TrainSet,kernel="linear",
#              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
#summary(tune.out)
#bestmodel<-tune.out$best.model

bestmodel<-svm(GROUP1245~., data=TrainSet, kernel="polynomial", cost=0.01, scale=FALSE,gamma=1,degree=3)

trai<-data.frame(valid=bestmodel$fitted, data=TrainSet$GROUP1245[match(names(bestmodel$fitted),rownames(TrainSet))])
trai$same<-trai$valid==trai$data
length(which(trai$same))/length(trai$valid)

valid.out=predict(bestmodel ,ValidSet)
acu<-data.frame(valid=valid.out, data=ValidSet$GROUP1245[match(names(valid.out),rownames(ValidSet))])
acu$same<-acu$valid==acu$data
length(which(acu$same))/length(acu$valid)

prop.table(
  table(valid=valid.out, data=ValidSet$GROUP1245[match(names(valid.out),rownames(ValidSet))])
  ,1)



#Result in 0.3100741 accuracy; slightly above chance;
#Linear SVM failed to identify ANY IDE or DEP tho
#Adding time lagged variables adds 1% imporvement! 
#Adding trial variable adds another 1%


#Non-linear SVM:
#Polynomial:

#Radial:







