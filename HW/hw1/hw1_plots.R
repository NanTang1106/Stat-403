## question 2
x_value <- seq(-10, 10, 0.05)
y_value <- exp(-sin(x_value)) * cos(pi * x_value)

plot(x_value, y_value, type = 'l', lwd = 2, col = 'goldenrod',
     xlab = 'x', ylab = 'y')

## question 3
normal_dt <- rnorm(5000, 2, 2)
hist(normal_dt, breaks=50, main='Histogram of N(2, 2)', probability=TRUE, 
     col='powderblue')
x_nor_value <- seq(-5, 10, 0.01)
y_nor_value <- dnorm(x_nor_value, 2, 2)
lines(x_nor_value, y_nor_value, lwd=3, col='blueviolet')

## question 4
iterations <- 100000
unisample <- rep(NA, iterations)

for(i in 1:iterations) {
  uni_avg <- mean(runif(2, 2, 4))
  unisample[i] <- uni_avg
}

hist(unisample, breaks=50, main='Histogram of Sample Mean 
     of X1, X2 from Uniform(2, 4)', probability=TRUE, col='darkseagreen1')
lines(c(2:3), c(0:1), lwd=3, col='deepskyblue')
lines(c(3:4), c(1:0), lwd=3, col='deepskyblue')



hist(rnorm(5000, 2, 2), breaks=50, probability=T, col='brown', main='Histogram of Normal(2, 2)')
x_base = seq(-5, 10, 0.01)
lines(x_base, dnorm(x_base, 2, 2), lwd=3, col='yellow')
