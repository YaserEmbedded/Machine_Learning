setwd("/home/yaser/Downloads/assignments_ML/")
library(kernlab)
library(dplyr)
library(randomForest)
library(e1071)


#set.seed(420)

datasets <- read.csv('Sequences_train.csv',header = FALSE)
datasets <- datasets[sample(nrow(datasets)),]
sequences<-datasets$V1
classes<-datasets$V2
sequences<-as.character(sequences)  

Dict = list(A=  524288,  # 10000000000000000000
            C=  262144,  # 01000000000000000000
            D=  131072,  # 00100000000000000000
            E=   65536,  # 00010000000000000000
            F=   32768,  # 00001000000000000000
            G=   16384,  # 00000100000000000000
            H=    8192,  # 00000010000000000000
            I=    4096,  # 00000001000000000000
            K=    2048,  # 00000000100000000000
            L=    1024,  # 00000000010000000000
            M=     512,  # 00000000001000000000
            N=     256,  # 00000000000100000000
            P=     128,  # 00000000000010000000
            Q=      64,  # 00000000000001000000
            R=      32,  # 00000000000000100000
            S=      16,  # 00000000000000010000
            T=       8,  # 00000000000000001000
            V=       4,  # 00000000000000000100
            W=       2,  # 00000000000000000010
            Y=       1   # 00000000000000000001
)

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
} 
get_hot_mat<-function(sequnc){
  hot_enc = matrix(0,length(sequnc),300)
  #hot_enc = matrix(0,length(sequnc),15)
  print(dim(hot_enc))
  sample_no<-1
  for (i in sequnc){
    sample<-matrix(0,0,0)
    for (j in 1:nchar(i)){
      charac<-substr(i,j,j)
      #sample<-c(sample,as.numeric(paste(number2binary(Dict[[get('charac')]],20),collapse = '')))
      sample<-c(sample,number2binary(Dict[[get('charac')]],20))
    }
    #print(sample)
    hot_enc[sample_no,]<-sample 
    sample_no<-sample_no+1
  }
  return(hot_enc)
}

compute_svm_CV<-function(svm_type){
  if (svm_type == "RBF")
  {
    tuned_svm<-tune(svm, train.x=hot_data, train.y = classes,type = ,kernel="radial basis", range=list(cost=10^(-2:2), gamma=c(0.1, 0.25,0.5,0.75,1,2)) )
    print(tuned_svm)
    
    svm_good_model<-svm(classes~., data=hot_data, kernel="radial basis",cost=tuned_svm$best.parameters$cost, gamma=tuned_svm$best.parameters$gamma)
  }
  else{
    tc <- tune.control(cross = 10)
    tuned_svm<-tune(svm, train.x=hot_data, train.y = classes,kernel="linear", range=list(cost=10^(-2:2)),tunecontrol = tc )
    print(summary(tuned_svm))
    
    svm_good_model<-svm(classes~., data=hot_data, kernel="linear",cost=tuned_svm$best.parameters$cost)
    print("Guuuut")
    print(cross(svm_good_model))
  }
  return(svm_good_model)
}
randomfor_CV<-function(train,labels){
  CV<-10
  s <- split(sample(nrow(train)),rep(1:CV,nrow(train)))
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
    scores[j]<-scores[j]+sum(Prediction==sign(labels[s[[i]]]))
  }
  scores[j]=scores[j]/CV
  }
    #varImpPlot(fit)
    #plot(fit)
return (scores)
}

hot_data<-get_hot_mat(sequences)

#random forrest using oob
fit_oob <- randomForest(as.factor(classes) ~ .,
                    data=hot_data, 
                    importance=TRUE, 
                    ntree=10)
varImpPlot(fit_oob)

scores_cv<-randomfor_CV(hot_data,classes)


#---------------------------------------------------------SVM-HOT-CV
best_model<-compute_svm_CV("Linear")


