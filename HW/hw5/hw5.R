# questoin 1-a
bern_p <- function(x) {
  return(exp(1 + 2 * x) / (1 + exp(1 + 2 * x)))
}
n <- 500
x_value <- rnorm(500)
y_value <- rbinom(n, size=1, p=bern_p(x_value))
xy_logic = glm(y_value~x_value, family = "binomial")

beta0 <- summary(xy_logic)$coefficient[1,1]
beta1 <- summary(xy_logic)$coefficient[2,1]
beta0
beta1

# question 1-b
N = 2000
beta0_sim <- rep(NA, N)
beta1_sim <- rep(NA, N)

for (ii in 1:N) {
  x_value <- rnorm(n)
  y_value <- rbinom(n, size=1, p=bern_p(x_value))
  xy_logic = glm(y_value~x_value, family = "binomial")
  beta0_sim[ii] <- summary(xy_logic)$coefficient[1,1]
  beta1_sim[ii] <- summary(xy_logic)$coefficient[2,1]
}

hist(beta0_sim, probability=T, main='Histogram of Fitted Beta0', breaks=20,
     xlab='Fitted Beta0', col='coral1')
abline(v=1, lwd=3, col='cornflowerblue' )
legend('topright', 'True Beta0', col='cornflowerblue', lwd=3, cex=0.75)

hist(beta1_sim, probability=T, main='Histogram of Fitted Beta1', breaks=20,
     xlab='Fitted Beta1', col='cornflowerblue')
abline(v=2, lwd=3, col='coral1' )
legend('topright', 'True Beta1', col='coral1', lwd=3, cex=0.75)

mean(beta0_sim)
mean(beta1_sim)

# question 1-d
n = 2000
N = 2000
beta0_sim <- rep(NA, N)
beta1_sim <- rep(NA, N)

for (ii in 1:N) {
  x_value <- rnorm(n)
  y_value <- rbinom(n, size=1, p=bern_p(x_value))
  xy_logic = glm(y_value~x_value, family = "binomial")
  beta0_sim[ii] <- summary(xy_logic)$coefficient[1,1]
  beta1_sim[ii] <- summary(xy_logic)$coefficient[2,1]
}

hist(beta0_sim, probability=T, main='Histogram of Fitted Beta0', breaks=20,
     xlab='Fitted Beta0', col='coral1', xlim=c(0.5, 1.5))
abline(v=1, lwd=3, col='cornflowerblue' )
legend('topright', 'True Beta0', col='cornflowerblue', lwd=3, cex=0.75)

hist(beta1_sim, probability=T, main='Histogram of Fitted Beta1', breaks=20,
     xlab='Fitted Beta1', col='cornflowerblue', xlim=c(1, 3))
abline(v=2, lwd=3, col='coral1' )
legend('topright', 'True Beta1', col='coral1', lwd=3, cex=0.75)

# question 1-e
library(Metrics)
sample_sizes <- seq(from=100, to=2000, by=200)
beta1_mse <- rep(NA, length(sample_sizes))
N = 2000

for (ii in 1:length(sample_sizes)) {
  n <- sample_sizes[ii]
  beta1 <- rep(NA, N)
  for (jj in 1:N) {
    x_value <- rnorm(n)
    y_value <- rbinom(n, size=1, p=bern_p(x_value))
    xy_logic = glm(y_value~x_value, family = "binomial")
    beta1[jj] <- summary(xy_logic)$coefficient[2,1]
  }
  beta1_mse[ii] <- mse(beta1, 2)
}

plot(x=sample_sizes, y=beta1_mse, type='ln', lwd=3, col='coral1', xlim=c(100, 2000),
     xlab='Sample Size', ylab='MSE of Estimated Beta1', 
     main='MSE of Estimated Beta1 VS Sample Size')
points(x=sample_sizes, y=beta1_mse, pch=20, cex=2, col="skyblue")
grid(nx=NA,ny=NULL,lty=1,lwd=0.5,col="gray")


# question 2-c
density_func <- function(x) {
  return(exp(x)/(1 + exp(x))^2)
}

M = 1.7
sim_size <- 20000
sim_U <- runif(sim_size)
sim_Y <- rcauchy(sim_size, 0, 1)
sim_X <- sim_Y[which(sim_U < density_func(sim_Y) / (M * dcauchy(sim_Y)))]

x_base <- seq(-10, 10, 0.01)
density_value <- density_func(x_base)

hist(sim_X, breaks=30, probability=T, xlim=c(-10,10), ylim=c(0, 0.25),
     xlab='X', main='Histogram of X', col='coral1')
lines(x_base, density_value, lwd=3, col='skyblue')
legend('topright', col='skyblue', legend='True Density', lwd=3, cex=0.75)

