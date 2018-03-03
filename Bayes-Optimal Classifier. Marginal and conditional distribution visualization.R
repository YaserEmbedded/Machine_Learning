p_y_plus <- 0.25
p_y_neg  <- 0.75

x = seq(-5, 10, length= 1000)

p_x_given_y_neg <- (1/(2*sqrt(pi)))*exp(-((x+1)^2)/4)
p_x_given_y_plus  <- (sqrt(2)/(sqrt(pi)))*exp(-2*((x-3)^2))

p_x <- (p_x_given_y_plus*p_y_plus)+(p_x_given_y_neg*p_y_neg)  
g_x <- (p_x_given_y_plus*p_y_plus)-(p_x_given_y_neg*p_y_neg)
g_bar <- g_x/p_x
plot(x, p_x, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

p_y_neg_given_x <- p_x_given_y_neg*p_y_neg/p_x
p_y_pos_given_x <- p_x_given_y_plus*p_y_plus/p_x


plot(x,p_y_neg_given_x,type="l",col="red")
lines(x,p_y_pos_given_x,col="green")


