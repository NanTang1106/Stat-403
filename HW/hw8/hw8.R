# question 2-a
eruption_dt <- faithful$eruptions

bwds <- c(0.1, 0.3, 0.9)
colors <- c('red', 'blue', 'green')
plot(1, type="n", xlab="Eruption", ylab="Density", xlim=c(min(eruption_dt), max(eruption_dt)),
     ylim=c(0, 1), main='KDE Function of Eruption')
for (ii in 1:length(bwds)) {
  erupt_kde <- density(eruption_dt, bw=bwds[ii])
  lines(erupt_kde, lwd=3, col=colors[ii])
}
legend('topright', legend=c('bw=0.1', 'bw=0.3', 'bw=0.9'), col=colors, lwd=3, cex=0.8)

# question 2-b
erupt_kde <- density(eruption_dt, bw=0.3)
hist(eruption_dt, breaks=20, probability=T)
lines(erupt_kde, lwd=3)

# question 2-c
erupt_kde <- density(eruption_dt, from=1, to=6, bw=0.3)
n <- length(eruption_dt)
B <- 10000
kde_bt <- matrix(NA, B, length(erupt_kde$x))
for (ii in 1:B) {
  sp_index <- sample(n,n,replace=T)
  sp_bt <- eruption_dt[sp_index]
  sp_kde <- density(sp_bt, from=1, to=6, bw=0.3)
  kde_bt[ii,] <- sp_kde$y
}

bt_sd <- sqrt(diag(var(kde_bt)))

plot(erupt_kde, lwd=3, col="blue", ylim=c(0,0.6),main="95% CI of KDE Function")
lines(x=erupt_kde$x,y=erupt_kde$y+qnorm(0.975)*bt_sd, lwd=3, col="dodgerblue",
      lty=2)
lines(x=erupt_kde$x,y=erupt_kde$y-qnorm(0.975)*bt_sd, lwd=3, col="dodgerblue",
      lty=2)

# question 3-a
library(smoothmest)
n=1000
dexp_dt <- rdoublex(n, mu=0, lambda=1)
dexp_den <- density(dexp_dt, bw=0.2, from=-6, n=n, to=6)
x_base <- dexp_den$x
dexp_true <- ddoublex(x_base, mu=0, lambda=1)

plot(x=x_base, y=dexp_den$y, type='l', lwd=3, col='skyblue', xlim=c(-6,6), 
     ylim=c(0, 0.5), xlab='X', ylab='Density', main='KDE of 1000 R.N from Double Exponential')
lines(x=x_base, y=dexp_true, lwd=3, lty=3, col='coral1')
legend('topright', legend=c('KDE h=0.2', 'dexp lambda=1'), col=c('skyblue', 'coral1'), 
       lty=c(1,2), lwd=2, cex=0.8)

# question 3-c
N = 10000
bwds <- seq(0.05, 0.5, 0.05)

mise_result <- rep(NA, length(bwds))
for (ii in 1:length(bwds)) {
  bwd <- bwds[ii]
  kde_result <- matrix(NA, nrow=N, ncol=n)
  for (jj in 1:N) {
    temp_den <- density(dexp_dt, from=-6, to=6, n=n, bw=bwd)
    kde_result[jj,] <- temp_den$y
  }
  mse_result <- colSums((t(t(kde_result) - dexp_true))^2) /N
  mise_result[ii] <- sum(mse_result)
}

plot(x=bwds, y=mise_result, pch=16, xlab='Bandwidth', ylab='MISE', 
     main='Bandwidth vs MISE', col='coral1')
lines(x=bwds, y=mise_result, lwd=2, col='skyblue')

opt_bwd <- bwds[which.min(mise_result)]

