## -------------------------------------------------------------
## Stat/Q Sci 403 Lab 1 | Spring 2019 | University of Washington
## -------------------------------------------------------------
###                     ###
### Part 1: First steps ###
###                     ###

## R as a caculator
1+4
2*5
18-3
5/4


## Naming variables
x <- 2*5
y = 2*5


## Other math operators also work
log(10)
exp(-4)
2^4

## Creating vectors is usually done by combine function c()
x = c(3,5)
c(18,2000,981)

c(18,2000,981, "a")


## effect of colon
1:10

x = 5:10
x

x[1]

x[5]

x[5] = 11
x


## creating a matrix:
matrix(c(1,2,3,4), nrow=2, ncol=2, byrow = T)
matrix(1:10, nrow=2)
matrix(1:10, ncol=2)

## naming a matrix 
A = matrix(1:10, ncol=2)
colnames(A) = c("XXX", "YYY")
A

rownames(A) = c("AA", "BB", "CC", "DD", "EE")
A

A[,2]

rbind(c(1,2,3), 5:7)

rbind(c(1,"a","3"), 5:7)

c(T,F,T, 5)


## creating a list
B = list(c(1:4), A, exp)
B

B[[1]]

B[[2]]

B[[2]][,1]


B[[3]]

B[[3]](5)
exp(5)

## creating a dataframe
C = data.frame(A)
C

C$XXX
  # you can use "$" in a dataframe to extra component

A$XXX
  # this does not work for a matrix

## using a built-in data
faithful

head(faithful)

faithful$eruptions
  # this works--the built-in data are dataframe


###                  ###
### Part 2: Graphics ###
###                  ###

## histogram
hist(faithful$eruptions)

hist(faithful$eruptions, breaks=50, col="green", probability = T)


## boxplot
boxplot(faithful$eruptions)

boxplot(faithful$eruptions, col="skyblue")


## scatter plot
plot(x= faithful[,2], y=faithful[,1])

plot(faithful, pch=16, col="limegreen", main="Scatter plot!")

plot(faithful, pch=16, col="limegreen", cex.axis=1.5, cex.lab=1.2,
     main="Scatter plot!", cex.main=2)


## creating a curve of a function
x_base = seq(from=-5, to =5, by=0.1)
x_base

y_value = exp(x_base)+exp(-x_base)
y_value

plot(x=x_base, y=y_value)

plot(x=x_base, y=y_value, type="l")

plot(x=x_base, y=y_value, type="l", lwd=5, col="brown")


#####
##### Exercise: 
##### 2-1. Plot the function f(x) = sin(x) 
#####      for x within [-10, 10].

x_value <- seq(-10, 10, 0.1)
y_value <- sin(x_value)
plot(x_value, y_value, type='l', col='darkorchid', lwd=3)

#####
##### 2-2. Plot the function f(x) = exp(-0.1*x)*cos(5*x) 
#####      for x within [0,10].
#####

x_value <- seq(0, 10, 0.05)
y_value <- exp(-0.1 * x_value) * cos(5 * x_value)
plot(x_value, y_value, type='l', lwd=3)

###                          ###
### Part 3: Random Variables ###
###                          ###
## normal random variable
rnorm(10, mean = 5, sd=10)
  # equals to rnorm(n=10), n is the number of random numbers

qnorm(0.50)

pnorm(1.64)


pnorm(qnorm(0.3))

qnorm(pnorm(3))


dnorm(0)

1/sqrt(2*pi)*exp(-0^2/2)


## exponential random variable
rexp(100,rate = 5)

hist(rexp(1000), breaks=100)

hist(rexp(1000), breaks=100, probability = T)
  # y-axis is now "density"

dexp(1)

exp(-1)

dexp(1, rate = 2)

2*exp(-2)
  # p(x;\lambda) =  \lambda * exp(-\lambda*x)


## uniform random variable
runif(50)
hist(runif(10000), col="orange")

runif(50, min=2, max=5)


## Bernoulli and Bionomial distribution
rbinom(10, size=1, p=0.7)
  # size=1: Bernoulli

rbinom(10, size=2, p=0.7)


#####
##### Exercise: 
##### 3-1. Plot the density curve of N(1,2^2) 
#####      in the interval [-3, 3]
#####

x_value <- seq(-3, 3, 0.1)
y_value <- dnorm(x_value)
plot(x_value, y_value, col='limegreen', type='l', lwd=3)

##### 3-2. Generate 1000 data points from Exp(3), 
#####      plot the histogram,
#####      and compare to its density curve
#####

x_value = rexp(1000, 3)
hist(x_value, breaks = 50, col='limegreen', probability = T)

###               ###
### Part 4: Loops ###
###               ###
x0 = NULL
for(i in 1:10){
  x0 = c(x0, 2*i)
  print(i)
}
x0


## resampling uniform distributions
for(i in 1:100){
  hist(runif(1000), breaks=seq(from=0, to=1, by=0.05),
       probability =T, ylim=c(0,1.6), col="palegreen")
  abline(h=1, lwd=3, col="purple")
  Sys.sleep(0.1)
}


## distribution of many medians
n_rep = 10000
n = 100
df0 = 4

sample_median = rep(NA,n_rep)
for(i in 1:n_rep){
  data = rchisq(n, df = df0)
    # generate n data points from Chi-sq(df0)
  sample_median[i] = median(data)
}

hist(sample_median, probability = T, col="skyblue", breaks=50)
  # It also converges to a normal distribution! 
  # We will talk about why this happens at the end of this course.
  

## central limit theorem
n_rep = 10000
n = 10
rate0 = 3

sample_mean = rep(NA, n_rep)

for(i in 1:n_rep){
  data = rexp(n,rate = rate0)
    # generate n data points from Exp(rate0)
  sample_mean[i] = mean(data)
    # i-th element of object "sample_mean" is a sample average
}

hist(sample_mean, probability = T, col="orchid", breaks=100)
x_base = seq(from=0, to=1, by=0.001)
lines(x=x_base, y=dnorm(x_base, mean=1/rate0, sd=1/(sqrt(n)*rate0)),
      lwd=3, col="dodgerblue")
  

## failure of central limit theorem
n_rep = 100
n = 1000

sample_mean = rep(NA, n_rep)

for(i in 1:n_rep){
  data = rcauchy(n)
  # generate n data points from Cauchy(0,1) distirbution 
  # the "mean" of Cauchy distribution does not exist!
  sample_mean[i] = mean(data)
}

hist(sample_mean, probability = T, col="pink", breaks=100)
x_base = seq(from=0, to=1, by=0.001)
  # with some probability, the sample mean could be very large/small


#####
##### Exercise: 
##### 4. Study the distribution of sample standard deviation.
#####    Try to generate n=100 data points from Exp(5), compute the sample SD.
#####    Repeat the procedure n_rep=10000 times and plot the histogram.
#####    You can use the function sd() to compute sample SD.
#####    Vary the value of n to see if the distribution converges.
#####
n_rep = 10000
result <- rep(NA, n_rep)
for (i in 1:n_rep) {
  result[i] <- sd(rexp(100, 5))
}
hist(result, probability = T, breaks = 100)

## changing sample size (optional)
n_rep = 10000
rate0 = 3
n_seq = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)

for(n in n_seq){
  sample_mean = rep(NA, n_rep)
  for(i in 1:n_rep){
    data = rexp(n,rate = rate0)
    sample_mean[i] = mean(data)
  }
  hist(sample_mean, probability = T, col="palevioletred", breaks=50,
       xlim=c(0,1), main=n)
  x_base = seq(from=0, to=1, by=0.001)
  lines(x=x_base, y=dnorm(x_base, mean=1/rate0, sd=1/(sqrt(n)*rate0)),
        lwd=3, col="dodgerblue")
  Sys.sleep(2)
}


