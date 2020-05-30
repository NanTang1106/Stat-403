# question 1 a
data("chickwts")

feed_linseed_dt <- subset(chickwts, feed == 'linseed')

t.test(x=sample_wt, mu=240, alternative='two.sided')

# question 1 b
mean(feed_linseed_dt$weight)
sd(feed_linseed_dt$weight)

# question 1 c
sim_size <- 10000
alpha <- 0.1
sim_result <- rep(NA, sim_size)
for (i in 1:sim_size) {
  sample <- rnorm(n=12, mean=220, sd=52)
  t_result <- t.test(x=sample, mu=240, alternative='two.sided')
  sim_result[i] <- t_result$p.value < alpha
}
t_power <- sum(sim_result) / sim_size


# question 1 d
sim_size <- 10000
sample_sz_seq <- c(12,24,36,48,60,72,84,96)
power_seq <- rep(NA, length(sample_sz_seq))

for (i in 1:length(sample_sz_seq)) {
  sim_result <- rep(NA, sim_size)
  for (j in 1:sim_size) {
    sample <- rnorm(n=sample_sz_seq[i], mean=220, sd=52)
    t_result <- t.test(x=sample, mu=240, alternative='two.sided')
    sim_result[j] <- t_result$p.value < alpha
  }
  t_power <- sum(sim_result) / sim_size
  power_seq[i] <- t_power
}

size_power_comp <- data.frame(cbind(sample_sz_seq, power_seq))
colnames(size_power_comp)[1] <- 'size'
colnames(size_power_comp)[2] <- 'power'

col_ramp = colorRampPalette(c("skyblue","limegreen"))

plot(x=size_power_comp$size, y=size_power_comp$power, col=col_ramp(nrow(size_power_comp)),
     pch=20, cex=2, xlim=c(10, 100), main='Size-Power Plot (test size 0.1)', 
     ylab='Power of t-test', xlab='Sample Size')
lines(x=size_power_comp$size, y=size_power_comp$power, lwd=3, col="skyblue")
grid(nx=NA,ny=NULL,lty=1,lwd=0.5,col="gray")

# question 2 a

sim_size <- 10000
sample_size <- 50
minunif_eva <- rep(NA, sim_size)

for (i in 1:sim_size) {
  sample <- runif(sample_size)
  minunif_eva[i] <- min(sample)
}

mean(minunif_eva)
sd(minunif_eva)

# question 2 b

x_value <- seq(from=0, to=max(minunif_eva), by=0.001)
exp50 <- dexp(x_value, 50)

hist(minunif_eva, breaks=50, col='skyblue', probability=T, 
     main='Histogram of M50 with Exponential(50)', xlab='M50')
lines(x=x_value, y=exp50, lwd=3, col='purple')
grid(nx=NA,ny=NULL,lty=1,lwd=0.5,col="gray")
legend('right', legend='Exp(50)',
       col='purple', lty=1:2, cex=0.8)

# question 2 c
sample_size <- 100
minunif_eva <- rep(NA, sim_size)

for (i in 1:sim_size) {
  sample <- runif(sample_size)
  minunif_eva[i] <- min(sample)
}

x_value <- seq(from=0, to=max(minunif_eva), by=0.001)
exp50 <- dexp(x_value, 100)

hist(minunif_eva, breaks=50, col='#b2df8a', probability=T, 
     main='Histogram of M100 with Exponential(100)', xlab='M100')
lines(x=x_value, y=exp50, lwd=3, col='#1f78b4')
grid(nx=NA,ny=NULL,lty=1,lwd=0.5,col="gray")
legend('right', legend='Exp(100)',
       col='#1f78b4', lty=1:2, cex=0.8)







