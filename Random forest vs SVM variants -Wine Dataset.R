library(dplyr)
library(randomForest)
library(rpart)
library("mice")

#--------------------------------------GET DATA----------------------------------
## https://archive.ics.uci.edu/ml/datasets/Wine
URL_2 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
Winenames<-c(
  "Class",
  "Alcohol",
  "Malic acid",
  "Ash",
  "Alcalinity of ash",
  "Magnesium",
  "Total phenols",
  "Flavanoids",
  "Nonflavanoid phenols",
  "Proanthocyanins",
  "Color intensity", 
  "Hue",
  "DW", 
  "Proline" 
)
Winedata <- read.table(URL_2, header=FALSE, sep=",", col.names=Winenames)
#----------------------------Some Functions------------------------------
compute_svm_CV<-function(data,classes){

    tc <- tune.control(cross = 10)
    tuned_svm<-tune(svm, train.x=data, train.y = classes,kernel="linear", range=list(cost=10^(-2:2)),tunecontrol = tc )
    print(summary(tuned_svm))
    
    svm_good_model<-svm(classes~., data=data, kernel="linear",cost=tuned_svm$best.parameters$cost)

    
  return(svm_good_model)
}
randomfor_CV<-function(train,labels){
  CV<-10
  #s <- split(sample(nrow(train)),rep(1:CV,nrow(train)))
  s <- split(sample(170),rep(1:CV,170))
  #adding the 8 samples left
  s[[1]][11]<-171;s[[2]][11]<-172;s[[3]][11]<-173;s[[4]][11]<-174;s[[5]][11]<-175;s[[6]][11]<-176;s[[7]][11]<-177;s[[8]][11]<-178
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

#----------------------factorizing data--------------------------------------

Winedata$Class<-as.factor(Winedata$Class)
#--------------------------------shuffle data-------------------------------
Winedata <- Winedata[sample(nrow(Winedata)),]
#----------------------------------------------------------------------------
fit_Wine_RF <- randomForest(as.factor(Winedata$Class) ~ .,
                            data=Winedata, 
                            importance=TRUE, 
                            ntree=10)
fit_Wine_RF
varImpPlot(fit_Wine_RF)
t<-randomfor_CV(Winedata[,2:14],Winedata[,1])
g<-compute_svm_CV(Winedata[,2:14],Winedata[,1])
