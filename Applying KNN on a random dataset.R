#--------------------------LIBRARY-------------------
library(class)
library(gmodels)
library(caret)
library(package = "lattice")
#--------------GETTING DATA------------------------

setwd("/home/yaser/Downloads/")
dataset <- read.csv('DataSet1.csv')
dataset$Y <- factor(dataset$Y)
str(dataset)
#--------------------FLIPPING DATA----------------------
Flipped_data <- dataset
#no_of_changed_nos <- 1
num <- runif(200, 0, 1)
for (index in 1:200)
{
  if(num[index] <= 0.2)
  {#no_of_changed_nos <- no_of_changed_nos+1
    
    if(Flipped_data$Y[index] == '1')
    {
      Flipped_data$Y[index] <- '-1'
    }
    else
    {
      Flipped_data$Y[index] <-  '1' 
    }
  #{dataset$Y[index] <- dataset$Y[index]*-1}
    }
}
#print(no_of_changed_nos)
print(summary(Flipped_data$Y))

#---------------------------------------FOUR NOISES -------------------------------------------
Data_fourNoises <- dataset
col_3 <- runif(200, 0, 1)
col_4 <- runif(200, 0, 1)
Data_fourNoises[,3] <- col_3
Data_fourNoises[,4] <- col_4
Data_fourNoises[,5] <- dataset[,3]

#---------------------------------------KNN FUNCTION--------------------------------------------
knn.cv <- function(klist,x.train,y.train,nfolds) {
  # Cross-validation for kNN
  #
  # Perform nfolds-cross validation of kNN, for the values of k in klist
  
  # Number of instances
  n.train <- nrow(x.train)
  n.train
  # Matrix to store predictions
  p.cv <- matrix(NA, n.train, length(klist))
  #print(summary(p.cv))
  # Prepare the folds
                    #200             #10        #200
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))
  

  
  
  for (i in seq(nfolds)) 
  {k_value <- 1
    for(k_index in klist)
      {
      

                                     #training              #test                    #labels of training
    p.cv[s[[i]],k_value] <- knn(    x.train[-s[[i]],],    x.train[s[[i]],],          cl=y.train[-s[[i]]],          k=k_index)

    
    k_value <- k_value + 1
     
     }
  }
  
  # Return matrix of CV predictions
  invisible(p.cv)
}
#K values
k = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51)
#calling knn function on three datasets
y.pred.cv <- knn.cv(k,dataset[,1:2],dataset[,3],10)
y.pred_flipped.cv <- knn.cv(k,Flipped_data[,1:2],Flipped_data[,3],10)
y.pred_Noise.cv <- knn.cv(k,Data_fourNoises[,1:4],Data_fourNoises[,5],10)


#------getting errors
GenError<-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51)
GenError_flipped <- GenError
GenError_Noise <- GenError

for (i  in 1:length(k)){
  
store_table <- CrossTable(x = dataset[,3],y.pred.cv[,i],prop.chisq = TRUE)
store_table_flipped <- CrossTable(x = Flipped_data[,3],y.pred_flipped.cv[,i],prop.chisq = TRUE)
store_table_Noise <- CrossTable(x = Data_fourNoises[,5],y.pred_Noise.cv[,i],prop.chisq = TRUE)

GenError[i]  <- store_table$t[1,2] + store_table$t[2,1]
GenError_flipped[i]  <- store_table_flipped$t[1,2] + store_table_flipped$t[2,1]
GenError_Noise[i]    <- store_table_Noise$t[1,2] + store_table_Noise$t[2,1]
}
#normalizing errors
GenError <- GenError/200
GenError_flipped <- GenError_flipped/200
GenError_Noise <- GenError_Noise/200

barplot(GenError, main="Genaralization error", xlab="K values", ylab="Error", names.arg=k,
        border="red", density=c(90, 70, 50, 40, 30, 20, 10))

barplot(GenError_flipped, main="Genaralization error Flipped", xlab="K values", ylab="Error", names.arg=k,
        border="red", density=c(90, 70, 50, 40, 30, 20, 10))

barplot(GenError_Noise, main="Genaralization error Noise", xlab="K values", ylab="Error", names.arg=k,
        border="red", density=c(90, 70, 50, 40, 30, 20, 10))











#--------------------ONLY DATA PLOTTING-------------------------------------------------------------------------------------
#decisionplot <- function(model, data, class = NULL, predict_type = "class",
  #                       resolution = 100, showgrid = TRUE, ...) {
  
#  if(!is.null(class)) cl <- data[,class] else cl <- 1
 # data <- data[,1:2]
 # k <- length(unique(cl))
  
 # plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
 # r <- sapply(data, range, na.rm = TRUE)
 # xs <- seq(r[1,1], r[2,1], length.out = resolution)
 # ys <- seq(r[1,2], r[2,2], length.out = resolution)
  #g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
 # colnames(g) <- colnames(r)
 # g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
#  p <- predict(model, g, type = predict_type)
#  if(is.list(p)) p <- p$class
#  p <- as.factor(p)
  
#  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
#  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
#  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
#          lwd = 2, levels = (1:(k-1))+.5)
  
#  invisible(z)
#}
#plot the whole data
#plot(dataset$X1,dataset$X2 ,pch=unclass(dataset$Y))


#plot the boundary according to k
#model <- knn3(dataset$Y ~ ., dataset, k = 9)
#model
#decisionplot(model, dataset, class = "Y", main = "kNN (1)")



#---------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------








