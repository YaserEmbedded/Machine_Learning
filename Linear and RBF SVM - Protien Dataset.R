#Name: Yasser Taha
#Matrkl. no.: k11777228
setwd("/home/yaser/Downloads/assignments_ML/")
library(kernlab)
library(dplyr)
datasets <- read.csv('Sequences_train.csv',header = FALSE)
datasets <- datasets[sample(nrow(datasets)),]
sequences<-datasets$V1
classes<-datasets$V2

#-------------------------------TEST--------------------------------
dataset_test <- read.csv('Sequences_test_unlabeled.csv',header = FALSE)
sequences_test<-dataset_test$V1
classes_test<-dataset_test$V2
#-------------------------------------------------------------------
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


hot_data<-get_hot_mat(sequences)
library(e1071)
compute_svm<-function(svm_type,data,classes){
  if (svm_type == "RBF")
    {
      tuned_svm<-tune(svm, train.x=data, train.y = factor(classes),kernel="radial", range=list(cost=10^(-3:3), gamma=c(0.001,0.01,0.1, 0.25,0.5,0.75,1,2)) )
      print(summary(tuned_svm))

      svm_good_model<-svm(factor(classes)~., data=data, kernel="radial",cost=tuned_svm$best.parameters$cost, gamma=tuned_svm$best.parameters$gamma)
  }
  else{
    
      tuned_svm<-tune(svm, train.x=data, train.y = factor(classes),kernel="linear", range=list(cost=10^(-3:5)) )
      print(summary(tuned_svm))
    
      svm_good_model<-svm(factor(classes)~., data=data, kernel="linear",cost=tuned_svm$best.parameters$cost)
    print("Guuuut")
    print(svm_good_model)
  }
  return(svm_good_model)
}
#type="C-classification",
best_model<-compute_svm("linear",hot_data,classes)
test_pred<-predict(best_model,hot_data[1901:2000,])
f<-sum((test_pred)==(classes[1901:2000]))
#how to know the cross over error?

#hot_data_test<-get_hot_mat(sequences_test)
#results<-predict(best_model,hot_data_test)

#write.table(results, file="/home/yaser/Downloads/assignments_ML/Results_of_RBF",sep = "\n" ,row.names=FALSE, col.names=FALSE)
