library(dplyr)
library(randomForest)
library(rpart)
library("mice")
## set URL
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"

## set column names according to
## https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
brCnames <- c("ID",
              "ClumpThickness",
              "UniformityOfCellSize",
              "UniformityOfCellShape",
              "MarginalAdhesion",
              "SingleEpithelialCellSize",
              "BareNuclei",
              "BlandChromatin",
              "NormalNucleoli",
              "Mitoses",
              "Class")

## read data
brCdata <- read.table(URL, header=FALSE, sep=",", col.names=brCnames,
                      na.strings="?")

## remove ID column
brCdata$ID <- NULL

## transform target column for the sake of better interpretability
brCdata$Class <- factor(ifelse(brCdata$Class == 4, "malignant", "benign"))

#----------------------factorizing data----------------
brCdata$Class<-as.factor(brCdata$Class)
#---------------------Completing data----------------
#brCdata_2 <- mice(brCdata,m=5,maxit=100,meth='pmm',seed=500)
#brCdata = complete(brCdata_2,1)
#-----------------------Or omit-------------------------
brCdata <- na.omit(brCdata)
#----------------------------------------------------

fit_Breast_RF <- randomForest(as.factor(brCdata$Class)~.,
                            data=brCdata,
                            importance=TRUE,
                            ntree=10)
fit_Breast_RF
#varImpPlot(fit_Breast_RF)
randomfor_CV<-function(train,labels){
  CV<-10
  #s <- split(sample(nrow(train)),rep(1:CV,nrow(train)))
  s <- split(sample(680),rep(1:CV,680))
  #adding the 8 samples left
  s[[1]][11]<-681;s[[5]][11]<-682;s[[9]][11]<-683
  #print(s)
  print((labels[s[[1]]]))
  scores<-matrix(0,1,4)
  tree_val <-c(10,100,1000,10000)
  for(j in 1:4){
    for (i in 1:CV){
      fit <- randomForest(as.factor(labels[-s[[i]]]) ~ .,
                          data=train[-s[[i]],], 
                          importance=TRUE, 
                          ntree=tree_val[j])
      
      Prediction <- predict(fit, train[s[[i]],])
      scores[j]<-scores[j]+sum(Prediction==(labels[s[[i]]]))
    }
    scores[j]=scores[j]/CV
  }
  #varImpPlot(fit)
  #plot(fit)
  return (scores)
}
sc<-randomfor_CV(brCdata[,1:9],brCdata[,10])

compute_svm_CV<-function(data,classes){
  
  tc <- tune.control(cross = 10)
  tuned_svm<-tune(svm, train.x=data, train.y = classes,kernel="linear", range=list(cost=10^(-2:2)),tunecontrol = tc )
  print(summary(tuned_svm))
  
  svm_good_model<-svm(classes~., data=data, kernel="linear",cost=tuned_svm$best.parameters$cost)
  
  
  return(svm_good_model)
}

compute_svm_CV(brCdata[,1:9],brCdata[,10])




