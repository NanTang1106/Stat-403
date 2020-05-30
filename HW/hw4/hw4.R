# question 1-c

po4_dt <- rpois(100, 4)

hist(po4_dt, main='Histogram of Poisson(4)', probability = T, xlab='X Value', 
     ylab='Density', col='orange')

lambda_est <- mean(po4_dt)

# question 1-d

sim_size <- 10000
sim_result <- rep(NA, sim_size)

for (i in 1:sim_size) {
  po_dt <- rpois(100, 4)
  sim_result[i] <- mean(po_dt)
}

norm_base <- seq(3.0, 5.0, 0.01)
norm_dt <- dnorm(norm_base, 4, 0.2)

hist(sim_result, probability = T, breaks=30, col='darkolivegreen1', 
     main='Histogram of Monte Carlo Poisson(4)', xlab='MLE value', ylab='Density')
lines(norm_base, norm_dt, lwd=2, col='deepskyblue')

# question 1-e

sum(sim_result >= 3.5 & sim_result <= 4.5) / sim_size

# question 2-a

quakes <- read.table('fijiquakes.dat', sep='', header=T)

hist(quakes$stations, probability=T, main='Histogram of EarthQuake Stations',
     xlab='Stations', col='skyblue')

# question 2-b

lambda_est <- 1/mean(quakes$stations)

exp_base <- seq(10, 140, 0.01)
exp_dt <- dexp(exp_base, lambda_est)

hist(quakes$stations, probability=T, main='Histogram of EarthQuake Stations',
     xlab='Stations', col='cadetblue1')
lines(exp_base, exp_dt, lwd=2, col='chocolate1')
legend('topright', legend=('Exp(0.02992)'), col='chocolate', lwd=3, cex=0.75)

# question 2-c
linear_reg = lm(quakes$mag~quakes$stations)
summary(linear_reg)$coeff[2,1]

plot(x=quakes$stations, y=quakes$mag, pch=19, cex=0.8, col='gray50', xlab='Station',
     ylab='Magnitude', main='Scatter Plot of Mag VS Station')
abline(linear_reg, lwd=3, col='skyblue')
legend('bottomright', legend=('Fitted Linear Curve'), col='skyblue', lwd=3, cex=0.75)

# question 2-d
beta1 <- summary(linear_reg)$coeff[2,1]
sd_beta1 <- summary(linear_reg)$coeff[2,2]
lowerbd <- beta1 - qnorm(0.975) * sd_beta1
upperbd <- beta1 + qnorm(0.975) * sd_beta1

