# question 1-a
petal_wdt_ln <- lm(Petal.Width~Sepal.Length + Sepal.Width + Petal.Length, data=iris)
petal_wdt_ln$coefficients

# quearion 1-b
# empirical bootstrap
cal_var <- function(dt, col_num) {
  result <- rep(NA, col_num)
  for (ii in 1:col_num) {
    result[ii] <- var(dt[, ii])
  }
  return(result)
}

n <- nrow(iris)
B <- 10000
reg_ep_bt <- matrix(NA, nrow=B, ncol=4)
for (ii in 1:B) {
  sp_index <- sample(n, n, replace=T)
  sp_dt <- iris[sp_index,]
  sp_ln <- lm(Petal.Width~Sepal.Length + Sepal.Width + Petal.Length, data=sp_dt)
  reg_ep_bt[ii,] <- sp_ln$coefficients 
}
var_ep_bt <- cal_var(reg_ep_bt, 4)

# residual bootstrap
y_predict <- predict(petal_wdt_ln)
reg_resid_bt <- matrix(NA, nrow=B, ncol=4)
for (ii in 1:B) {
  sp_index <- sample(n, n, replace=T)
  sp_y <- petal_wdt_ln$residuals[sp_index] + y_predict
  sp_dt <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Width=iris$Sepal.Width, 
                      Petal.Length=iris$Petal.Length, Petal.Width=sp_y)
  sp_ln <- lm(Petal.Width~Sepal.Length + Sepal.Width + Petal.Length, data=sp_dt)
  reg_resid_bt[ii,] <- sp_ln$coefficients
}
var_resid_bt <- cal_var(reg_resid_bt, 4)

# wild bootstrap
reg_wild_bt <- matrix(NA, nrow=B, ncol=4)
for (ii in 1:B) {
  sp_y <- rnorm(n, sd=abs(petal_wdt_ln$residuals)) + y_predict
  sp_dt <- data.frame(Sepal.Length=iris$Sepal.Length, Sepal.Width=iris$Sepal.Width, 
                      Petal.Length=iris$Petal.Length, Petal.Width=sp_y)
  sp_ln <- lm(Petal.Width~Sepal.Length + Sepal.Width + Petal.Length, data=sp_dt)
  reg_wild_bt[ii,] <- sp_ln$coefficients
}
var_wild_bt <- cal_var(reg_wild_bt, 4)

var_result <- rbind(var_ep_bt, var_resid_bt, var_wild_bt)
rownames(var_result) <- c('Empirical bt', 'Residual bt', 'Wild bt')
colnames(var_result) <- c('Intercept', 'Sepal.Length', 'Sepal.Width', 'Petal.Length')

# question 1-c
hist(reg_ep_bt[,1], probability = T, breaks=30, col=rgb(1,0,0,0.5), xlim=c(-1, 0.5),
     ylim=c(0, 2.5), xlab='Bootstrap Intercept',
     main='Distributions of Bootstrap Intercept')
hist(reg_resid_bt[,1], probability = T, breaks=30, add=T, col=rgb(0,0,1,0.5))
hist(reg_wild_bt[,1], probability = T, breaks=30, add=T, col=rgb(0,1,0,0.4))
legend('topright', c('Empirical', 'Residual', 'Wild'), 
       col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5), rgb(0,1,0,0.5)),lwd=5, cex = 0.7)

library(plotrix)
p <- multhist(list(reg_ep_bt[,1], reg_resid_bt[,1], reg_wild_bt[,1]), probability=T,
              main='Histograms of Bootstrap Intercept', xlab='Bootstrap Intercept')
legend('topright', c('Empirical', 'Residual', 'Wild'), 
       col=c('gray30', 'gray55', 'gray80'),lwd=5, cex = 0.75)

# question 2-a
setwd('/Users/nantang/Google Drive/STAT 403/HW/hw7')
admission_dt <- read.csv('binary.csv', header =T)
binom_logistic <- glm(admit~gpa + gre, data=admission_dt, family='binomial')
B <- 10000
n <- nrow(admission_dt)

par_bt_slope <- rep(NA, B)
for (ii in 1:B) {
  sp_admit <- rbinom(n, size=1, prob=predict(binom_logistic, type='response'))
  sp_dt <- data.frame(admit=sp_admit, gpa=admission_dt$gpa, gre=admission_dt$gre)
  sp_logistic <- glm(admit~gpa + gre, data=sp_dt, family='binomial')
  par_bt_slope[ii] <- sp_logistic$coefficients[2]
}
quantile(par_bt_slope, 0.05)
quantile(par_bt_slope, 0.95)

# question 2-b
par_bt_three <- matrix(NA, nrow=B, ncol=3)
for (ii in 1:B) {
  sp_admit <- rbinom(n, size=1, prob=predict(binom_logistic, type='response'))
  sp_dt <- data.frame(admit=sp_admit, gpa=admission_dt$gpa, gre=admission_dt$gre)
  sp_logistic <- glm(admit~gpa + gre, data=sp_dt, family='binomial')
  par_bt_three[ii,] <- sp_logistic$coefficients
}
par_bt_sd <- c(sd(par_bt_three[,1]), sd(par_bt_three[,2]), sd(par_bt_three[,3]))

ep_bt_three <- matrix(NA, nrow=B, ncol=3)
for (ii in 1:B) {
  sp_index <- sample(n, n, replace=T)
  sp_dt <- admission_dt[sp_index, ]
  sp_logistic <- glm(admit~gpa + gre, data=sp_dt, family='binomial')
  ep_bt_three[ii,] <- sp_logistic$coefficients
}
head(ep_bt_three)
ep_bt_sd <- c(sd(ep_bt_three[,1]), sd(ep_bt_three[,2]), sd(ep_bt_three[,3]))

result_sd <- rbind(summary(binom_logistic)$coefficients[,2], par_bt_sd, ep_bt_sd)
colnames(result_sd) <- c('Intercept', 'gpa', 'gre')
rownames(result_sd) <- c('original', 'parametric bt', 'empirical bt')

# question 2-3
est_prob <- predict(binom_logistic, type='response', 
                    data.frame(gpa=3.7, gre=500))
lower_bd <- est_prob$fit - est_prob$se.fit * qnorm(0.95)
upper_bd <- est_prob$fit + est_prob$se.fit * qnorm(0.95)

prob_bt <- rep(NA, B)
for (ii in 1:B) {
  sp_index <- sample(n, n, replace=T)
  sp_dt <- admission_dt[sp_index,]
  sp_logis <- glm(admit~gpa + gre, data=sp_dt, family='binomial')
  prob_bt[ii] <- predict(sp_logis, type='response', data.frame(gpa=3.7, gre=500))
}
lower_bd <- quantile(prob_bt, 0.05)
upper_bd <- quantile(prob_bt, 0.95)

# question 2-4
probs_bt <- matrix(NA, nrow=B, ncol=2)
for (ii in 1:B) {
  sp_index <- sample(n, n, replace=T)
  sp_dt <- admission_dt[sp_index,]
  sp_logis <- glm(admit~gpa + gre, data=sp_dt, family='binomial')
  probs_bt[ii, 1] <- predict(sp_logis, type='response', data.frame(gpa=2.3, gre=700))
  probs_bt[ii, 2] <- predict(sp_logis, type='response', data.frame(gpa=3.9, gre=670))
}

delta <- probs_bt[,1] - probs_bt[,2]
hist(delta)

delta_sd <- sd(delta)

observed_prob1 <- predict(binom_logistic, type='response', data.frame(gpa=2.3, gre=700))
observed_prob2 <- predict(binom_logistic, type='response', data.frame(gpa=3.9, gre=670))
observed_delta <- observed_prob1 - observed_prob2

p_value <- 2 * pnorm(observed_delta, sd=delta_sd)





