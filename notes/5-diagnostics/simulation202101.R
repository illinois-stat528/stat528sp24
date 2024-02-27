#######################################
### Code for "Assessment of Regression Models with Discrete Outcomes Using
### Quasi-Empirical Residual Distribution Functions"
#######################################
#######################################
#### Preamble
rm(list = ls())
library(np)
### A function needed for bandwidth selection
listvec <- function(x) {
  x[1]:x[2]
}
########################################
#######################################





########################################
########################################
## Poisson example
########################################
########################################


### \hat{U}. y is the vector of outcomes,and lambdaf is the vector of fitted Poisson mean
marginesti <- function(u, y, lambdaf) {
  n <- length(y)
  # F^{-1}(u)
  inv1 <- qpois(u, lambdaf)
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  # H^+ and H^_
  p1 <- ppois(n1, lambdaf)
  # find out which one is closer to u
  ind1 <- apply(abs(p1 - u), 1, which.min)
  # weight function
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

marginesti <- Vectorize(marginesti, "u")


####### bandwidth selector
bandwidthp <- function(y, lambdaf) {
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qpois(0.1, lambdaf), qpois(0.9, lambdaf)), 1:n), listvec))
  newlambda <- rep(lambdaf, times = qpois(0.9, lambdaf) - qpois(0.1, lambdaf) + 1)
  newx <- ppois(newout, newlambda)
  newy <- 1 * (rep(y, times = qpois(0.9, lambdaf) - qpois(0.1, lambdaf) + 1) <= newout)

  bws <- npregbw(ydat = newy[which(newx <= 0.9)], xdat = newx[which(newx <= 0.9)], ckertype = "epanechnikov")
  return(bws$bw)
}


####### A simulated example
# generate Poisson data
n <- 500

beta1 <- 2
beta2 <- 1
beta0 <- -2


set.seed(1234)
# covariates
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.7)
lambda1 <- exp(beta0 + beta1 * x1 + beta2 * x2)
set.seed(12345)
# generate outcomes
y <- rpois(n, lambda1)


# fit regression model
poismodel <- glm(y ~ x1 + x2, family = poisson(link = "log"))
lambda1f <- glm(y ~ x1 + x2, family = poisson(link = "log"))$fitted.values



h <- bandwidthp(y = y, lambdaf = lambda1f)


curve(marginesti(u, y = y, lambdaf = lambda1f), xname = "u", main = "Quasi, Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


### play-around
library(tidyverse)
library(faraway)
gala <- gala %>% 
  mutate(Size = as.factor(1 + ifelse(Area > 1,1,0) + ifelse(Area > 25,1,0)))
dat <- gala %>% 
  mutate(Size = as.factor(1 + ifelse(Area > 1,1,0) + ifelse(Area > 25,1,0))) %>% 
  filter(Species > 5)

poismodel <- glm(Species ~ sqrt(Elevation) + Nearest + Scruz + Adjacent + Area + 
                   sqrt(Area), 
          family = "poisson", data = dat, x = TRUE)
#poismodel <- glm(Species ~ Elevation + Nearest + Scruz + Adjacent + Size, 
#          family = "quasipoisson", data = dat, x = TRUE)
y = dat$Species
lambda1f = poismodel$fitted.values
cbind(lambda1f, dat$Species)
h = bandwidthp(y = y, lambdaf = lambda1f)

curve(marginesti(u, y = y, lambdaf = lambda1f), xname = "u", main = "Quasi, Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)





### Deviance residuals
resd <- residuals(poismodel, type = "deviance")
probDistd <- pnorm(resd)
plot(ppoints(n), sort(probDistd),
  main = "Deviance, Poisson", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


## Pearson residuals
resp <- residuals(poismodel, type = "pearson")
probDistp <- pnorm(resp)
plot(ppoints(n), sort(probDistp),
  main = "Pearson, Poisson", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



########################################
########################################
## binary example
########################################
########################################

## \hat{U}. y is the outcome, and q0 is the fitted probability of zero
margin01 <- function(u, y, q0) {
  wei <- 1 * ((q0 - u)^2 < 5 * h^2) * (1 - ((q0 - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y == 0)) / sum(wei)
  l
}

margin01 <- Vectorize(margin01, "u")

## bandwidth selector
bandwidth01 <- function(y, q0) {
  bw <- npregbw(ydat = 1 * (y == 0), xdat = q0, ckertype = "epanechnikov")
  return(bw$bw)
}


####### A simulated example
beta1 <- 2
beta2 <- 1
beta0 <- -2

n <- 2000
set.seed(1234)
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.7)

q1 <- 1 / (1 + exp(beta0 + beta1 * x1 + beta2 * x2))
set.seed(1234)
y <- rbinom(n, size = 1, prob = 1 - q1)

### fit the regression model
model0 <- glm(y ~ x1 + x2, family = binomial(link = "logit"))
q10 <- 1 - model0$fitted.values


h <- bandwidth01(y = y, q0 = q10)

curve(margin01(u, y = y, q0 = q10), xname = "u", main = "Quasi, Binary", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)

### deviance residuals
resd <- residuals(model0, type = "deviance")
probDistd <- pnorm(resd)
plot(ppoints(n), sort(probDistd),
  main = "Deviance, Binary", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)

#### randomized quantile residuals
library(statmod)
set.seed(11)
r <- qresiduals(model0)
plot(ppoints(n), sort(pnorm(r)),
  main = "Randomized, Binary", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


########################################
########################################
## negative binomial example
########################################
########################################
library(MASS)

### hat{U}
## y is the outcome, lambdaf is the mean, and sizef is the size for the negative binomial distribution
marginnb <- function(u, y, lambdaf, sizef) {
  n <- length(y)
  inv1 <- qnbinom(u, mu = lambdaf, size = sizef)
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  p1 <- pnbinom(n1, mu = lambdaf, size = sizef)
  ind1 <- apply(abs(p1 - u), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

marginnb <- Vectorize(marginnb, "u")

## bandwidth selector
bandwidthnb <- function(y, lambdaf, sizef) {
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qnbinom(0.1, mu = lambdaf, size = sizef), qnbinom(0.9, mu = lambdaf, size = sizef)), 1:n), listvec))
  newlambda <- rep(lambdaf, times = qnbinom(0.9, mu = lambdaf, size = sizef) - qnbinom(0.1, mu = lambdaf, size = sizef) + 1)
  newx <- pnbinom(newout, mu = newlambda, size = sizef)
  newy <- 1 * (rep(y, times = qnbinom(0.9, mu = lambdaf, size = sizef) - qnbinom(0.1, mu = lambdaf, size = sizef) + 1) <= newout)

  newy1 <- newy[which(newx <= 0.9)]
  newx1 <- newx[which(newx <= 0.9)]
  bws <- npregbw(ydat = newy1, xdat = newx1, ckertype = "epanechnikov")
  return(bws$bw)
}

####### A simulated example
n <- 500
set.seed(1234)
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.7)


beta1 <- 2
beta2 <- 1
beta0 <- -2

size1 <- 2

lambda1 <- exp(beta0 + beta1 * x1 + beta2 * x2)

set.seed(1234)

y <- rnbinom(n, mu = lambda1, size = size1)


##### Fit NB regression model
model1 <- glm.nb(y ~ x1 + x2)
lambda1f <- model1$fitted.values
size1f <- summary(model1)$theta

h <- bandwidthnb(y = y, lambdaf = lambda1f, sizef = size1f)


curve(marginnb(u, y = y, lambdaf = lambda1f, sizef = size1f), xname = "u", main = "Quasi, NB", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


#### Deviance residuals
resd <- residuals(model1, type = "deviance")
probDistd <- pnorm(resd)
plot(ppoints(n), sort(probDistd),
  main = "Deviance, NB", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


######## Randomized quantile residuals
resr <- qres.nbinom(model1)

probDistr <- pnorm(resr)
plot(ppoints(n), sort(probDistr),
  main = "Randomized, NB", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)




########################################
########################################
## Zero-inflated Poisson
########################################
########################################

library(pscl)

##### \hat{U}. y is the outcome, pzero is the fitted excess zero probability, and meanpoisson is the Poisson mean
marginzerop <- function(u, y, pzero, meanpoisson) {
  n <- length(y)
  inv1 <- ifelse(u < (pzero + (1 - pzero) * (ppois(0, lambda = meanpoisson))), 0,
    qpois(pmax((u - pzero) / (1 - pzero), 0), lambda = meanpoisson)
  )
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  p1 <- cbind(
    (pzero + (1 - pzero) * (ppois(inv1, meanpoisson))),
    (pzero + (1 - pzero) * (ppois(inv1m, meanpoisson)))
  )
  ind1 <- apply(abs(p1 - u), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

marginzerop <- Vectorize(marginzerop, "u")

### Bandwidth selection
bandwidth0p <- function(y, pzero, meanpoisson) {
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qpois(0.1, meanpoisson), qpois(0.9, meanpoisson)), 1:n), listvec))
  newlambda <- rep(meanpoisson, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newzero <- rep(pzero, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newx <- newzero + (1 - newzero) * ppois(newout, newlambda)
  newy <- 1 * (rep(y, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1) <= newout)

  return(npregbw(ydat = newy[which(newx <= 0.9 & newx >= 0.1)], xdat = newx[which(newx <= 0.9 & newx > 0.1)], ckertype = "epanechnikov")$bw)
}


####### A simulated example
n <- 500
set.seed(1234)
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.7)

beta1 <- 2
beta2 <- 1

beta0 <- 0
beta00 <- -2
beta10 <- 2

##### Mean of Poisson part
lambda1 <- exp(beta0 + beta1 * x1 + beta2 * x2)
#### Excess zero probability
p0 <- 1 / (1 + exp(-(beta00 + beta10 * x1)))

#### simulate outcomes
set.seed(1234)
y0 <- rbinom(n, size = 1, prob = 1 - p0)
y1 <- rpois(n, lambda1)

y <- ifelse(y0 == 0, 0, y1)


##### fit zero-inflated Poisson
model1 <- zeroinfl(y ~ x1 + x2 | x1, dist = "poisson", link = "logit")
meanpoisson <- exp(model1$coefficients$count[1] + model1$coefficients$count[2] * x1 + model1$coefficients$count[3] * x2)
pzero <- 1 / (1 + exp(-(model1$coefficients$zero[1] + model1$coefficients$zero[2] * x1)))

#### histogram of F(0)
hist((pzero + (1 - pzero) * (ppois(0, lambda = meanpoisson))), breaks = 20, xlim = c(0, 1), main = "Histogram of F(0|X)", xlab = "", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)

h <- bandwidth0p(y = y, pzero = pzero, meanpoisson = meanpoisson)

curve(marginzerop(u, y = y, pzero = pzero, meanpoisson = meanpoisson), xname = "u", main = "Quasi, 0-Inflated Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)

resp <- residuals(model1, type = "pearson")
probDistp <- pnorm(resp)
plot(ppoints(n), sort(probDistp),
  main = "Pearson, 0-Inflated Poisson", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)


#### Randomized quantile residuals
fitted <- pzero + (1 - pzero) * (ppois(y, lambda = meanpoisson))
fitted1 <- ifelse(y == 0, 0, pzero + (1 - pzero) * (ppois(y - 1, lambda = meanpoisson)))

set.seed(1234)
rand <- qnorm(runif(n, min = fitted1, max = fitted))
plot(ppoints(n), sort(pnorm(rand)),
  main = "Randomized, 0-Inflated Poisson", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



########################################
########################################
## Underdispersion, COM-Poisson
########################################
########################################
library(COMPoissonReg)
library(MASS)


### \hat{U} y is the outcome, lambdaf is the location parameter, sizef is the dispersion parameter
margincmp <- function(u, y, lambdaf, sizef) {
  n <- length(y)
  inv1 <- qcmp(rep(u, n), lambda = lambdaf, nu = sizef)
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  p1 <- cbind(pcmp(inv1, lambda = lambdaf, nu = sizef), pcmp(inv1m, lambda = lambdaf, nu = sizef))
  ind1 <- apply(abs(p1 - u), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

margincmp <- Vectorize(margincmp, "u")

### Bandwidth selection
bandwidthcmp <- function(y, lambdaf, sizef) {
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qcmp(rep(0.1, n), lambda = lambdaf, nu = sizef), qcmp(rep(0.9, n), lambda = lambdaf, nu = sizef)), 1:n), listvec))
  newlambda <- rep(lambdaf, times = qcmp(rep(0.9, n), lambda = lambdaf, nu = sizef) - qcmp(rep(0.1, n), lambda = lambdaf, nu = sizef) + 1)
  newx <- pcmp(newout, lambda = newlambda, nu = sizef)
  newy <- 1 * (rep(y, times = qcmp(rep(0.9, n), lambda = lambdaf, nu = sizef) - qcmp(rep(0.1, n), lambda = lambdaf, nu = sizef) + 1) <= newout)

  newy1 <- newy[which(newx <= 0.9)]
  newx1 <- newx[which(newx <= 0.9)]
  bws <- npregbw(ydat = newy1, xdat = newx1, ckertype = "epanechnikov")

  return(bws$bw)
}


####### A simulated example
n <- 500
set.seed(1234)
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.7)


beta1 <- 2
beta2 <- 1
beta0 <- -2

size1 <- 4

lambda1 <- exp(beta0 + beta1 * x1 + beta2 * x2)

set.seed(1234)

y <- rcmp(n, lambda = lambda1, nu = size1)

model1 <- glm.cmp(y ~ x1 + x2)
lambda1f <- exp(model1$X %*% model1$beta)
size1f <- summary(model1)$DF.nu[1]$Estimate

h <- bandwidthcmp(y = y, lambdaf = lambda1f, sizef = size1f)

curve(margincmp(u, y = y, lambdaf = lambda1f, sizef = size1f), xname = "u", main = "Quasi, COM-Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



########################################
########################################
## Ordinal regression with three levels 1, 2, and 3
########################################
########################################

library(MASS)

### \hat{U} y is the outcome, q0=P(y<=1), q1=P(y<=2)
marginm <- function(x, y, q0, q1) {
  n <- length(y)
  p1 <- cbind(q0, q1)
  ind1 <- apply(abs(p1 - x), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - x)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - x)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= (ind1 - 1))) / sum(wei)
  l
}

marginm <- Vectorize(marginm, "x")

### Bandwidth selection
bandwidthord <- function(y, q0, q1) {
  bw <- npregbw(ydat = c(1 * (y == 0), 1 * (y <= 1)), xdat = c(q0, q1), ckertype = "epanechnikov")
  return(bw$bw)
}


####### A simulated example
n <- 500
set.seed(1234)
beta1 <- 2


set.seed(1234)
x1 <- rnorm(n, mean = 2)
x2 <- rbinom(n, 1, 0.7)

## P(y==1)
p0 <- plogis(2, location = beta1 * x1)
## P(y==2)
p1 <- plogis(4, location = beta1 * x1) - p0
## P(y==3)
p2 <- 1 - p0 - p1

set.seed(1234)
genemult <- function(p) {
  rmultinom(1, size = 1, prob = c(p[1], p[2], p[3]))
}

##### generate outcomes
test <- apply(cbind(p0, p1, p2), 1, genemult)

y <- rep(0, n)
y[which(test[1, ] == 1)] <- 0
y[which(test[2, ] == 1)] <- 1
y[which(test[3, ] == 1)] <- 2




#### Fit ordered logistic regression
multimodel <- polr(as.factor(y) ~ x1, method = "logistic")

q0 <- multimodel$fitted.values[, 1]
q1 <- multimodel$fitted.values[, 1] + multimodel$fitted.values[, 2]



h <- bandwidthord(y = y, q0 = q0, q1 = q1)
curve(marginm(x, y = y, q0 = q0, q1 = q1), main = "Quasi, Ordinal", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))

abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



### Liu&Zhang's method



mu <- multimodel$coefficients[1] * x1

z <- rep(0, n)
set.seed(1234)
u <- runif(n)

a1 <- multimodel$zeta[1]
a2 <- multimodel$zeta[2]

z[y == 0] <- -log((1 + exp(mu[y == 0] - a1)) / u[y == 0] - 1) + mu[y == 0]


z[y == 1] <- -log(1 / (u[y == 1] * (1 / (1 + exp(-(a2 - mu[y == 1]))) - 1 / (1 + exp(mu[y == 1] - a1))) + 1 / (1 + exp(mu[y == 1] - a1))) - 1) + mu[y == 1]
z[y == 2] <- -log(1 / (u[y == 2] * (1 - 1 / (1 + exp(-a2 + mu[y == 2]))) + 1 / (1 + exp(-a2 + mu[y == 2]))) - 1) + mu[y == 2]

r <- z - mu

plot(ppoints(n), sort(plogis(r)),
  main = "Liu&Zhang, Ordinal", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)




#### Li&Shepherd

residli <- rep(0, n)
residli[which(y == 0)] <- plogis(multimodel$zeta[1], location = mu[y == 0]) - 1

residli[which(y == 1)] <- plogis(multimodel$zeta[1], location = mu[y == 1]) - 1 + plogis(multimodel$zeta[2], location = mu[y == 1])

residli[which(y == 2)] <- plogis(multimodel$zeta[2], location = mu[y == 2])
plot(ppoints(n), sort(punif(residli, min = -1, max = 1)),
  main = "Li&Shepherd, Ordinal", xlab = "Theoretical Cumulative", ylab = "Empirical Cumulative",
  cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1)
)
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)
