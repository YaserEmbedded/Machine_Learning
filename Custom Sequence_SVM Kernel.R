#Name: Yasser Taha
#Matrkl. no.: k11777228
setwd("/home/yaser/Downloads/")
library(kernlab)
datasets <- read.csv('Sequences_train.csv',header = FALSE)
sequences<-datasets$V1
classes<-datasets$V2
sequences<-as.character(sequences)

dataset_test <- read.csv('Sequences_test_unlabeled.csv',header = FALSE)
sequences_test<-dataset_test$V1
sequences_test<-as.character(sequences_test)


#-----------------------------------------FUNCTIONS--------------------------------------------------
divide_seq<- function(sequnc,k){
  f = list()
  for(j in 0:(nchar(sequnc)-k)+1){
    
    for(i in substr(sequnc,j,j+k-1)){
      if(is.null(f[[get('i')]])){f[[get('i')]]<-1}
      else{f[[get('i')]] <- f[[get('i')]]+1}
      
    }
  }
  return(f)
}
compute_SP <- function(L,l_no){
  vec<- matrix(0,1,length(L)-l_no)
  for(i in (l_no+1):length(L)){
    
    current_list<-L[[i]]
    for(j in 1:length(L[[l_no]])){
      
      value<-names(L[[l_no]][j])
      corresp_val<-current_list[[get('value')]]
      
      if(!is.null(corresp_val)){
        vec[i-l_no] = vec[i-l_no] + corresp_val*L[[l_no]][[j]]
      }
    }
  }
  return(vec)
}
get_SP_dd<-function(L){
  vec<- matrix(0,1,length(L))
  for (i in 1:length(L)){
    for (j in 1:length(L[[i]])){vec[i] = vec[i] + L[[i]][j][[1]]*L[[i]][j][[1]]}
  }
  return(vec)
}
compute_Matrix<- function(k,dataset){
  
  KernalMat <- diag(length(dataset))
  All_Mat<- lapply(as.list(dataset),divide_seq,k)
  diag_dd <- get_SP_dd(All_Mat)
  for (i in 1:(length(All_Mat)-1)){
    KernalMat[i,(i+1):length(All_Mat)]<-compute_SP(All_Mat,i)
    sample_no<-i+1
    for(j in sample_no:length(All_Mat)){KernalMat[i,j] = KernalMat[i,j]/sqrt(diag_dd[j]*diag_dd[i]) }
  }
  KernalMat[lower.tri(KernalMat)] <- t(KernalMat)[lower.tri(KernalMat)]
  
  return(KernalMat)
}
compute_test_Mat<-function(List_test,List_train){
  TEST_MAT<-matrix(0,length(List_test),length(List_train))
  
  for(i in 1:length(List_test)){
    current_list<-List_test[[i]]
    for(k in 1:length(List_train)){
      for(j in 1:length(List_train[[k]])){
        
        value<-names(List_train[[k]][j])
        corresp_val<-current_list[[get('value')]]
        
        if(!is.null(corresp_val)){
          TEST_MAT[i,k] = TEST_MAT[i,k] + corresp_val*List_train[[k]][[j]]
        }
      }
    }
  }
  return(TEST_MAT)
}
Compute_score<- function(Cost,dataset,y,All_Mat){
  
  
  score<-0
  
  holdouts = sample(1:length(dataset),100)
  KernalMatrix_train<-All_Mat[-holdouts,-holdouts]
  Test_Matrix<- All_Mat[holdouts,-holdouts]
  m <- ksvm(KernalMatrix_train,y=y[-holdouts],type="C-svc",kernel='matrix',C=Cost,cross=10,prob.model(TRUE) )
  
  print(cross(m))
#either return the score of the cross over error
  return(cross(m))
#or return the  scores on the predicted 100 samples 
  #p<-predict(m,as.kernelMatrix(Test_Matrix))
  #return(sum(sign(p)==sign(y[holdouts])))
  
  
}
Compute_score_best<- function(Cost,y,All_Mat){
  
  
  holdouts = c(2001:4000)
  KernalMatrix_train<-All_Mat[-holdouts,-holdouts]
  Test_Matrix<- All_Mat[holdouts,-holdouts]
  m <- ksvm(KernalMatrix_train,y=y,type="C-svc",kernel='matrix',C=Cost,prob.model(TRUE) )
  p<-predict(m,as.kernelMatrix(Test_Matrix))

  return(p)
  
  
}
#SOURCE: https://stackoverflow.com/questions/16172731/how-to-compute-the-power-of-a-matrix-in-r
library(expm) 
exp.mat<-function(MAT, EXP, tol=NULL){
  MAT <- as.matrix(MAT)
  matdim <- dim(MAT)
  if(is.null(tol)){
    tol=min(1e-7, .Machine$double.eps*max(matdim)*max(MAT))
  }
  if(matdim[1]>=matdim[2]){ 
    svd1 <- svd(MAT)
    keep <- which(svd1$d > tol)
    res <- t(svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep]))
  }
  if(matdim[1]<matdim[2]){ 
    svd1 <- svd(t(MAT))
    keep <- which(svd1$d > tol)
    res <- svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep])
  }
  return(res)
}

#--------------------------choosing the best c and k----------------------------
k <- c(1:5)
#k<-2
costs <- c(0.001,0.01,0.1,0.05,1,10,100,1000,10000,100000)
#costs<-c(0.5,0.6,0.7,0.8,0.9,1.1,1.2,1.3,1.4,1.5,1.9,2)
scores<-matrix(0,length(k),length(costs))
#loop to get the best k and c
for(i in 1:length(k)){
  All_Matrix<-compute_Matrix(k[i],sequences)
  #print(i)
  for(j in 1:length(costs)){
  print(j)
  scores[i,j]<-Compute_score(costs[j],sequences,classes,All_Matrix)
  }
}

#after best k and best c and best power_mat were found
k <- 2
costs <- 0.8
power_mat<- -0.01

All_seq<-c(sequences,sequences_test)
All_Matrix<-compute_Matrix(k,All_seq)

All_Matrix<-exp.mat(All_Matrix, power_mat)
All_Matrix <- All_Matrix + diag(length(All_seq))

output_predict<-Compute_score_best(costs,classes,All_Matrix)

#write.table(output_predict, file="/home/yaser/Results_of_sequence",sep = "\n" ,row.names=FALSE, col.names=FALSE)
