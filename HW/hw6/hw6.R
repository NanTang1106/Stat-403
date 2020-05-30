# question 2-a
origin_sd <- sd(faithful$waiting)
bt_size <- 10000
sample_size <- length(faithful$waiting)
bt_result <- rep(NA, bt_size)

for (ii in 1:bt_size) {
  index <- sample(sample_size, sample_size, replace=T)
  bt_dt <- faithful$waiting[index]
  bt_result[ii] <- sd(bt_dt)
}

hist(bt_result, probability=T, col='cornflowerblue', xlab='SD',
     ylab='Density', main='Histogram of Bootstrapped sample SD')
abline(v=origin_sd, lwd=3, lty=1, col='coral1')

# question 2-b
bt_var <- sd(bt_result)^2
bt_mse <- mean((bt_result - origin_sd)^2)

# question 2-c
bt_sd <- sd(bt_result)
lower_bd <- origin_sd + qnorm(0.975) * bt_sd
upper_bd <- origin_sd - qnorm(0.975) * bt_sd

quantile(bt_result, c(0.025, 0.975))

# question 2-d
bt_sd <- sd(bt_result)

p_value <- 2 * pnorm(origin_sd, 15, bt_sd)

#question 3-a
admission_dt <- UCBAdmissions[,,1]

origin_or <- (admission_dt[1,1] * admission_dt[2,2]) / 
  (admission_dt[2,1] * admission_dt[1,2])

n_male <- admission_dt[1,1] + admission_dt[2,1]
n_female <- admission_dt[1,2] + admission_dt[2,2]
n <- n_male + n_female

data_pull <- c(rep(1, admission_dt[1,1] + admission_dt[1,2]), 
               rep(0, admission_dt[2,1] + admission_dt[2,2]))

N <- 10000
bt_per_or <- rep(NA, N)
for (ii in 1:N) {
  sp_index <- sample(n, n, replace=T)
  sp_dt <- data_pull[sp_index]
  sp_male <- sp_dt[1:n_male]
  sp_female <- sp_dt[(n_male+1):n]
  male_admit <- sum(sp_male)
  male_reject <- length(sp_male) - male_admit
  female_admit <- sum(sp_female)
  female_reject <- length(sp_female) - female_admit
  sp_or <- (male_admit * female_reject) / (male_reject * female_admit)
  bt_per_or[ii] <- sp_or
}

bt_per_mse <- mean((bt_per_or - origin_or)^2)

# question 3-b
hist(bt_per_or, probability=T, xlab='Permutation OR', ylab='Density', col='cornflowerblue',
     main='Histogram of Permutation OR')
abline(v=mean(mean(bt_per_or)), lwd=3, lty=1, col='coral1')

bt_per_sd <- sd(bt_per_or)
p_value <- 2 * pnorm(origin_or, 1, bt_per_sd)

# question 3-c
male_dt <- c(rep(1, admission_dt[1,1]), rep(0, admission_dt[2,1]))
female_dt <- c(rep(1, admission_dt[1,2]), rep(0, admission_dt[2,2]))

bt_ep_or <- rep(NA, N)
for (ii in 1:N) {
  male_index <- sample(n_male, n_male, replace=T)
  female_index <- sample(n_female, n_female, replace=T)
  sp_male <- male_dt[male_index]
  sp_female <- female_dt[female_index]
  male_adm <- sum(sp_male)
  male_reject <- n_male - male_adm
  female_adm <- sum(sp_female)
  female_reject <- n_female - female_adm
  sp_or <- (male_adm * female_reject) / (male_reject * female_adm)
  bt_ep_or[ii] <- sp_or
}

bt_ep_mse <- mean((bt_ep_or - origin_or)^2)

hist(bt_ep_or, probability=T, xlab='Bootstrap OR', ylab='Density', col='cornflowerblue',
     main='Histogram of Bootstrap OR')
abline(v=mean(mean(bt_ep_or)), lwd=3, lty=1, col='coral1')


bt_ep_sd <- sd(bt_ep_or)
p_value <- 2 * pnorm(origin_or, 1, bt_ep_sd)
