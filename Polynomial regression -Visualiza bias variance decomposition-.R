one_training_shot<- function(){
l = 20
x <- seq(-1, 3, length= l)


noise <-  ((1/(sqrt(2*pi*0.09)))*exp(-(x^2)/(2*0.09)))

#plot(x,noise,col='deepskyblue4',xlab='q',main='Observed data')
draws = sample(x, size = l, replace = TRUE, prob = noise)

function_my <-0.6*(x^(4))+2*(x^(3)) - 8*(x^(2))
y <- function_my + draws

predicted_intervals <- matrix(, nrow = 7, ncol = 20)
predicted_value <- matrix(, nrow = 1, ncol = 7)


    for(power in 1:7)
    {
      model <- lm(y ~ poly(x,power))
      predicted_intervals[power,]<- predict(model,data.frame(x=x))
      predicted_value[power] <- predict(model,data.frame(x = 1.8))
      
    }
invisible(predicted_value)
}  

#plot(x,y,col='deepskyblue4',xlab='q',main='Observed data')
#lines(x,function_my,col='firebrick1',lwd=3)
#lines(x,predicted_intervals[3,],col='green',lwd=3)
augmented <- matrix(, nrow = 200, ncol = 7)
for (training_set_number in 1:200){
  augmented[training_set_number,] <- one_training_shot()
}
#compute bias
bias <- c(0,0,0,0,0,0,0)
varianc<- bias
#compute expected at x = 1.8
x_0 <- 1.8
expected_y_at_x0 <- 0.6*(x_0^(4))+2*(x_0^(3)) - 8*(x_0^(2))
#loop to get bias and variance
for(complexity in 1:7)
{
  bias[complexity] <- (expected_y_at_x0 - (sum(augmented[,complexity])/200))^2
  varianc[complexity] <- var(augmented[,complexity])
}

unav_error <- c(0.09 ,0.09, 0.09, 0.09, 0.09,0.09, 0.09)
EPE = unav_error + bias + varianc
comp <- c(1,2,3,4,5,6,7)
plot(comp,bias,col='deepskyblue4',xlab='q',main='EPE vs. Complexity')



















