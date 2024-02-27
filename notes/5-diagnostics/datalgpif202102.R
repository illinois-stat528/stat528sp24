library(MASS)
library(pscl)
library(np)

rm(list = ls())

### A function needed for bandwidth selection
listvec <- function(x) {
  x[1]:x[2]
}

urllgpif <- "https://sites.google.com/a/wisc.edu/jed-frees/home/documents/data.RData"
load(url(urllgpif))

## Choose the subset of customers who had coverage in BC
BCy <- which(data$CoverageBC > 0)
freqinBC <- data[BCy, ]
# log coverage is used in the model
freqinBC$lnCoverageBC <- log(freqinBC$CoverageBC)
## Outcomes
freqBC <- freqinBC$FreqBC





# Possion GLM
freqmodelBC <- glm(FreqBC ~ lnCoverageBC + lnDeductBC + NoClaimCreditBC +
  TypeCity + TypeCounty + TypeMisc + TypeSchool + TypeTown,
data = freqinBC, family = poisson(link = "log")
)
meanpoissonBC <- freqmodelBC$fitted.values


### \hat{U}. y is the vector of outcomes,and lambdaf is the vector of fitted Poisson mean
marginesti <- function(u, y, lambdaf) {
  n <- length(y)
  # F^-1(u)
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


hpois <- bandwidthp(y = freqBC, lambdaf = meanpoissonBC)
h <- hpois

curve(marginesti(u, y = freqBC, lambdaf = meanpoissonBC), xname = "u", main = "Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)




# covariates
indexfreqBC <- c(
  which(names(freqinBC) == "lnCoverageBC"),
  which(names(freqinBC) == "lnDeductBC"),
  which(names(freqinBC) == "NoClaimCreditBC"),
  which(names(freqinBC) == "TypeCity"),
  which(names(freqinBC) == "TypeCounty"),
  which(names(freqinBC) == "TypeMisc"),
  which(names(freqinBC) == "TypeSchool"),
  which(names(freqinBC) == "TypeTown")
)
indexzeroBC <- c(
  which(names(freqinBC) == "lnCoverageBC"),
  which(names(freqinBC) == "lnDeductBC"),
  which(names(freqinBC) == "NoClaimCreditBC")
)
k <- length(indexfreqBC) + 1
m <- length(indexzeroBC) + 1
x2 <- cbind(1, freqinBC[, indexfreqBC])
z <- cbind(1, freqinBC[, indexzeroBC])



# Zero-inflated Poisson
zeroBC <- zeroinfl(FreqBC ~ lnCoverageBC + lnDeductBC + NoClaimCreditBC +
  TypeCity + TypeCounty + TypeMisc + TypeSchool +
  TypeTown | 1 + lnCoverageBC + lnDeductBC + NoClaimCreditBC,
data = freqinBC, dist = c("poisson"), link = c("logit")
)
gammazeroP <- zeroBC$coefficients$zero
betazeroP <- zeroBC$coefficients$count
pzeroBCP <- exp(as.matrix(z) %*% gammazeroP) / (1 + exp(as.matrix(z) %*% gammazeroP))
meanpoisson0pBC <- exp(as.matrix(x2) %*% betazeroP)

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

bandwidth0p <- function(y, pzero, meanpoisson) {
  ### Bandwidth selection
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qpois(0.1, meanpoisson), qpois(0.9, meanpoisson)), 1:n), listvec))
  newlambda <- rep(meanpoisson, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newzero <- rep(pzero, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newx <- newzero + (1 - newzero) * ppois(newout, newlambda)
  newy <- 1 * (rep(y, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1) <= newout)

  return(npregbw(ydat = newy[which(newx <= 0.9 & newx >= 0.1)], xdat = newx[which(newx <= 0.9 & newx > 0.1)], ckertype = "epanechnikov")$bw)
}



h0p <- bandwidth0p(y = freqBC, pzero = pzeroBCP, meanpoisson = meanpoisson0pBC)
h <- h0p
curve(marginzerop(u, y = freqBC, pzero = pzeroBCP, meanpoisson = meanpoisson0pBC), xname = "u", main = "Zero Inflated Poisson", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)




# 01 inflated Poisson
## likelihood
likelihood01 <- function(a) {
  beta <- a[1:k]
  gamma0 <- a[(k + 1):(k + m)]
  gamma1 <- a[(k + m + 1):(k + m + m)]
  zgamma0 <- as.matrix(z) %*% gamma0
  zgamma1 <- as.matrix(z) %*% gamma1
  pzeroBC <- 1 / (exp(-zgamma0) + exp(zgamma1 - zgamma0) + 1)
  poneBC <- 1 / (exp(-zgamma1) + exp(zgamma0 - zgamma1) + 1)
  x2beta <- as.matrix(x2) %*% beta
  meanpoissonBC <- exp(x2beta)
  l <- -sum(log(pmax((pzeroBC) * (freqBC == 0) +
    poneBC * (freqBC == 1) + (1 - pzeroBC - poneBC) *
      dpois(freqBC, meanpoissonBC), 10^-10)))
  return(l)
}
opp01BCbfgs <- optim(c(
  zeroBC$coefficients$count,
  zeroBC$coefficients$zero, rep(0, m)
),
likelihood01,
method = ("BFGS"), hessian = T
)

## Estimated parameters and standard errors
cbind(opp01BCbfgs$par, sqrt(diag(solve(opp01BCbfgs$hessian))))
beta01 <- opp01BCbfgs$par[1:k]
gamma0 <- opp01BCbfgs$par[(k + 1):(k + m)]
gamma1 <- opp01BCbfgs$par[(k + m + 1):(k + m + m)]
pzeroBC01 <- exp(as.matrix(z) %*% gamma0) /
  (1 + exp(as.matrix(z) %*% gamma0) + exp(as.matrix(z) %*% gamma1))
poneBC01 <- exp(as.matrix(z) %*% gamma1) /
  (1 + exp(as.matrix(z) %*% gamma1) + exp(as.matrix(z) %*% gamma0))
meanpoissonBC01 <- exp(as.matrix(x2) %*% beta01)

margin01pois <- function(u, y, pzero, pone, meanpoisson) {
  n <- length(y)
  inv1 <- ifelse(u < (pzero + (1 - pzero - pone) * (ppois(0, lambda = meanpoisson))), 0,
    ifelse(u < (pzero + pone + (1 - pzero - pone) * (ppois(1, meanpoisson))), 1,
      qpois(pmax((u - pzero - pone) / (1 - pzero - pone), 0), lambda = meanpoisson)
    )
  )
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  p1 <- cbind(
    1 * (inv1 == 0) *
      (pzero + (1 - pzero - pone) *
        (ppois(0, meanpoisson))) +
      1 * (inv1 >= 1) * (pzero + pone + (1 - pzero - pone) *
        (ppois(inv1, lambda = meanpoisson))),
    1 * (inv1m == 0) *
      (pzero + (1 - pzero - pone) *
        (ppois(0, meanpoisson))) +
      1 * (inv1m >= 1) * (pzero + pone + (1 - pzero - pone) *
        (ppois(inv1m, lambda = meanpoisson)))
  )

  ind1 <- apply(abs(p1 - u), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

margin01pois <- Vectorize(margin01pois, "u")

bandwidth01pois <- function(y, pzero, pone, meanpoisson) {
  n <- length(y)
  newout <- unlist(sapply(split(cbind(qpois(0.1, meanpoisson), qpois(0.9, meanpoisson)), 1:n), listvec))
  newlambda <- rep(meanpoisson, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newzero <- rep(pzero, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newone <- rep(pone, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1)
  newx <- ifelse(newout == 0, newzero + (1 - newzero - newone) * ppois(newout, newlambda), newzero + newone + (1 - newzero - newone) * ppois(newout, newlambda))
  newy <- 1 * (rep(y, times = qpois(0.9, meanpoisson) - qpois(0.1, meanpoisson) + 1) <= newout)
  return(npregbw(ydat = newy[which(newx <= 0.9 & newx >= 0.1)], xdat = newx[which(newx <= 0.9 & newx > 0.1)], ckertype = "epanechnikov")$bw)
}

h01p <- bandwidth01pois(y = freqBC, pzero = pzeroBC01, pone = poneBC01, meanpoisson = meanpoissonBC01)
h <- h01p
curve(margin01pois(u, y = freqBC, pzero = pzeroBC01, pone = poneBC01, meanpoisson = meanpoissonBC01), xname = "u", main = "Zero One Inflated", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



# nb GLM
freqmodelBCnb <- glm.nb(FreqBC ~ lnCoverageBC + lnDeductBC + NoClaimCreditBC +
  TypeCity + TypeCounty + TypeMisc + TypeSchool + TypeTown,
data = freqinBC
)
meannbBC <- freqmodelBCnb$fitted.values
sizenbf <- summary(freqmodelBCnb)$theta


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

hnb <- bandwidthnb(y = freqBC, lambdaf = meannbBC, sizef = sizenbf)
h <- hnb
curve(marginnb(u, y = freqBC, lambdaf = meannbBC, sizef = sizenbf), xname = "u", main = "NB", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)



# zero inflated negative binomial
zeronbBC <- zeroinfl(FreqBC ~ lnCoverageBC + lnDeductBC + NoClaimCreditBC +
  TypeCity + TypeCounty + TypeMisc + TypeSchool +
  TypeTown | 1 + lnCoverageBC + lnDeductBC + NoClaimCreditBC,
data = freqinBC, dist = c("negbin"), link = c("logit")
)
gammazeronb <- zeronbBC$coefficients$zero
betazeronb <- zeronbBC$coefficients$count
pzeroBCnb <- exp(as.matrix(z) %*% gammazeronb) / (1 + exp(as.matrix(z) %*% gammazeronb))
mean0nbBC <- exp(as.matrix(x2) %*% betazeronb)
sizenb0f <- zeronbBC$theta

##### \hat{U}. y is the outcome, pzero is the fitted excess zero probability, and meanpoisson is the Poisson mean
marginzeronb <- function(u, y, pzero, meannb, sizenb) {
  n <- length(y)
  inv1 <- ifelse(u < (pzero + (1 - pzero) * (pnbinom(0, mu = meannb, size = sizenb))), 0,
    qnbinom(pmax((u - pzero) / (1 - pzero), 0), mu = meannb, size = sizenb)
  )
  inv1m <- ifelse(inv1 > 0, inv1 - 1, 0)
  n1 <- cbind(inv1, inv1m)
  p1 <- cbind(
    (pzero + (1 - pzero) * (pnbinom(inv1, mu = meannb, size = sizenb))),
    (pzero + (1 - pzero) * (pnbinom(inv1m, mu = meannb, size = sizenb)))
  )
  ind1 <- apply(abs(p1 - u), 1, which.min)
  wei <- 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l <- sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

marginzeronb <- Vectorize(marginzeronb, "u")

bandwidth0nb <- function(y, pzero, meannb, sizenb) {
  ### Bandwidth selection

  n <- length(y)
  newout <- unlist(sapply(split(cbind(qnbinom(0.1, mu = meannb, size = sizenb), qnbinom(0.9, mu = meannb, size = sizenb)), 1:n), listvec))
  newlambda <- rep(meannb, times = qnbinom(0.9, mu = meannb, size = sizenb) - qnbinom(0.1, mu = meannb, size = sizenb) + 1)
  newzero <- rep(pzero, times = qnbinom(0.9, mu = meannb, size = sizenb) - qnbinom(0.1, mu = meannb, size = sizenb) + 1)
  newx <- newzero + (1 - newzero) * pnbinom(newout, mu = newlambda, size = sizenb)
  newy <- 1 * (rep(y, times = qnbinom(0.9, mu = meannb, size = sizenb) - qnbinom(0.1, mu = meannb, size = sizenb) + 1) <= newout)

  return(npregbw(ydat = newy[which(newx <= 0.9 & newx >= 0.1)], xdat = newx[which(newx <= 0.9 & newx > 0.1)], ckertype = "epanechnikov")$bw)
}



h0nb <- bandwidth0nb(y = freqBC, pzero = pzeroBCnb, meannb = mean0nbBC, sizenb = sizenb0f)
h <- h0nb
curve(marginzeronb(u, y = freqBC, pzero = pzeroBCnb, meannb = mean0nbBC, sizenb = sizenb0f), xname = "u", main = "Zero Inflated NB", ylab = expression(hat(U) * "(s)"), xlab = "s", cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2, xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1, col = "red", lty = 5, cex.lab = 2, cex.axis = 2, cex.main = 2, lwd = 2)
