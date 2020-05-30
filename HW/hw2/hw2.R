# question 1-3
x_base <- seq(0, 1, 0.01)
beta_cdf <- function(x) {return(3 * x^2 - 2 * x^3)}
plot(x_base, beta_cdf(x_base), type='l', col='skyblue', lwd=3,
     xlim=c(0, 1), main='CDF of Beta(2, 2)', xlab='x value', ylab='F(x)')

# question 2-2
unif_value <- runif(1000, 0, 1)
w_value <- -2*log(unif_value)
exp_value <- rexp(1000, 0.5)
w_edf <- ecdf(w_value)
exp_edf <- ecdf(exp_value)
plot(w_edf, col='royalblue', lwd=1, 
     main='EDF of W=-2log(Unif(0, 1)) and Exp(0.5)')
lines(exp_edf, col='orange', lwd=1)
legend('bottomright', legend=c('W', 'Exp(0.5)'), lty=1:1, 
       col=c('royalblue', 'orange'), title='Distributions', cex=0.8)

# question 3-1
norm_value <- rnorm(5000, 2, 2)
norm_edf <- ecdf(norm_value)
plot(norm_edf, xlim=c(-1, 5), col='limegreen', lwd=1,
     main='EDF of N(2, 2)')

# question 3-2
x_base <- seq(-2, 6, 0.01)
norm_cdf <- pnorm(x_base, 2, 2)
plot(x_base, norm_cdf, xlim=c(-1, 5), type='l', lwd=1, col='red', xlab='x value',
     ylab='Fn(x)', main='CDF and EDF of N(2, 2)')
for(i in 1:10) {
  edf_value <- ecdf(rnorm(5000, 2, 2))
  lines(edf_value, col=paste('gray', i+45), lwd=1)
}
lines(x_base, norm_cdf, xlim=c(-1, 5), col='red', lwd=1)
legend('bottomright', legend=c('cdf of N(2, 2)', '10 edfs from N(2, 2)'), lty=1:1, 
       col=c('red', 'gray50'), title='Line Types', cex=0.8)






