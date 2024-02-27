
# load in software
library(tidyverse)
library(faraway)

head(gala)
?gala
gala = gala %>% 
  mutate(Size = as.factor(1 + ifelse(Area > 1,1,0) + ifelse(Area > 25,1,0)))

# fit model (main effects only; no Area; no Endemics)
m1 = glm(Species ~ Elevation + Nearest + Scruz + Adjacent + Size, 
         family = "poisson", data = gala, x = TRUE)

## model matrix
M = m1$x
head(M)

# summary table
summary(m1)

# Fisher scoring is actually IRLS
glm.fit

# compare summary table with vcov
all.equal(summary(m1)$coef[, 2], sqrt(diag(vcov(m1))))

# parameterizations

## beta -> theta
beta = coef(m1)
theta = as.numeric(M %*% beta)

## theta -> mu
mu = exp(theta)
all.equal(mu, as.numeric(predict(m1, type = "response")))

# test against intercept only (null) model
pchisq(m1$null.deviance - m1$deviance, df = m1$df.null - m1$df.residual, 
       lower = FALSE)

m_null = glm(Species ~ 1, family = "poisson", data = gala, x = TRUE)
anova(m_null, m1, test = "LRT")
all.equal(m_null$deviance, m1$null.deviance)

## null model is iid Poisson model
exp(coef(m_null))
mean(gala$Species)

## AIC/BIC
AIC(m_null, m1)
BIC(m_null, m1)

# test against a smaller model with Elevation removed
m_small = glm(Species ~ Nearest + Scruz + Adjacent + Size, 
              family = "poisson", data = gala)
anova(m_small, m1, test = "LRT")
pchisq(m_small$deviance - m1$deviance, df = 1, lower = FALSE)

## AIC/BIC
AIC(m_small, m1)
BIC(m_small, m1)

# test levels of the Size variable
summary(m1)
comp = c(0,0,0,0,0,-1,1)
betahat = m1$coefficients
grad = exp(betahat) * comp
est = sum(grad)
est

InvFish = vcov(m1)
asympVar = as.numeric(t(grad) %*% InvFish %*% grad)
asympVar
SE = sqrt(asympVar)
SE

est/SE
est + qnorm(c(0.025,0.975)) * SE

## Bonferroni correction
est + qnorm(c(0.025/3, 1-0.025/3)) * SE


# Diagnostics 

## Pearson residuals
residuals(m1, "pearson")

## Deviance residuals
residuals(m1)

## make dataframe and ggplot to display residuals
dat = data.frame(theta = as.numeric(M %*% betahat), 
                 mu = exp(as.numeric(M %*% betahat)), 
                 deviance_residuals = residuals(m1), 
                 pearson_residuals = residuals(m1, "pearson")) %>%
  pivot_longer(., cols = deviance_residuals:pearson_residuals, 
               names_to = "residuals")
dat$residuals = 
  factor(dat$residuals, 
         levels = c("deviance_residuals", "pearson_residuals"), 
         labels = c("Deviance", "Pearson"))

ggplot(dat) + 
  aes(x = theta, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

ggplot(dat) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

## half normal plot
halfnorm(residuals(m1), pch = 19)

## variance vs mean plot
plot(log(fitted(m1)),log((gala$Species-fitted(m1))^2), 
     xlab= expression(hat(mu)),ylab=expression((y-hat(mu))^2), 
     pch = 19, main = "variance vs mean (on log scale)")
abline(0,1)

# test against saturated model
summary(m1)
m1$deviance

## saturated model log likelihood
y = gala$Species
sum(log(dpois(y, lambda = y)))

## submodel log likelihood
logLik(m1)
sum(log(dpois(y, lambda = mu)))

## deviance 
-2 * (logLik(m1) - sum(log(dpois(y, lambda = y))))
m1$deviance

## saturated model preferable
pchisq(m1$deviance, df = m1$df.residual, lower = FALSE)


# overdispersion
n = nrow(gala)
p = length(betahat)
y = gala$Species

## estimate dispersion directly
fits = predict(m1, type = "response")
dp = sum((y - fits)^2/fits) / (n - p)
summary(m1, dispersion = dp)

## fit the model with dispersion
m2 = glm(Species ~ Elevation + Nearest + Scruz + Adjacent + Size, 
         family = "quasipoisson", data = gala, x = TRUE)
summary(m2)

## dispersion and sqrt(dispersion)
c(dp, sqrt(dp))

# basic model outputs
# coefficient estimates are the same; standard errors are different
se = function(model) sqrt(diag(vcov(model)))
round(data.frame(coef.m1=coef(m1), 
                 coef.m2=coef(m2), 
                 se.m1=se(m1), 
                 se.m2=se(m2), 
                 ratio=se(m2)/se(m1)), 4)

# START HERE 

## revisit our test between levels (with overdispersion)
summary(m2)
comp = c(0,0,0,0,0,-1,1)
betahat = m2$coefficients
grad = exp(betahat) * comp
est = sum(grad)
est

InvFish = vcov(m2)
asympVar = as.numeric(t(grad) %*% InvFish %*% grad)
asympVar
SE = sqrt(asympVar)
SE


## is overdispersion present?
library(AER)
dispersiontest(m1, trafo = 1)


# negative binomial regression
library(faraway)
?solder
head(solder)
solder %>% arrange(desc(skips))

## fit with Poisson
modp = glm(skips ~., data = solder, family = "poisson")
summary(modp)

## check saturated model
pchisq(deviance(modp), df.residual(modp), lower = FALSE)
nrow(solder)

## fit with Poisson quadratic
modp2 = glm(skips ~.^2, data = solder, family = "poisson")
summary(modp2)
pchisq(deviance(modp2), df.residual(modp2), lower = FALSE)

## check saturated model

## check for outliers and overdispersion

### half normal plot
halfnorm(modp2)
n = nrow(solder)
dat = data.frame(
  resid = sort(residuals(modp2)), 
  q = qnorm( (n + 1:n) / (2*n + 1))
)

ggplot(dat) + 
  aes(x = q, y = resid) + 
  geom_point()

### overdispersion?
plot(predict(modp2, type = "response"), (solder$skips - fitted(modp2))^2, 
     xlab= expression(hat(theta)),ylab=expression((y-hat(mu))^2), 
     pch = 19)
lines(sort(predict(modp2, type = "response")), 
      sort(fitted(modp2)), lty="dashed", 
      col = "red")

dispersiontest(modp, trafo=1)
dispersiontest(modp2, trafo=1)

## try negative binomial regression
library(MASS)
modn = glm(skips ~ ., negative.binomial(1), solder)
summary(modn)

## get optimal k
?glm.nb
modnk = glm.nb(skips ~ ., solder)
summary(modnk)

## look at coefficients from Poisson and neg Binom fits
sort(coef(modp2), decreasing = TRUE)[1:5]
sort(coef(modn), decreasing = TRUE)[1:5]
sort(coef(modnk), decreasing = TRUE)[1:5]

dat = solder %>% mutate(preds = predict(modn, type = "response"))
dat %>% arrange(desc(preds))

# zero-inflated regression
library(pscl)
?bioChemists

## Poisson fit
modp = glm(art ~ ., data=bioChemists, family=poisson)

## test against saturated
pchisq(deviance(modp), df.residual(modp), lower = FALSE)

## look at data
ocount = table(bioChemists$art)[1:8]
pcount = colSums(predprob(modp)[,1:8])
?predprob

plot(pcount, ocount, type="n", xlab="Predicted", ylab="Observed", 
     ylim = c(0, 300), axes = FALSE)
axis(side = 1)
axis(side = 2)
text(pcount,ocount, 0:7)


## on probability scale
probs = dpois(0:7, mean(bioChemists$art))
obs_freq = table(bioChemists$art)[1:8]/sum(table(bioChemists$art)[1:8])
plot(probs, obs_freq, 
     type="n", xlab="Predicted", ylab="Observed")
axis(side = 1)
axis(side = 2)
text(probs, obs_freq, 0:7)

## zero-inflated model
modz = zeroinfl(art ~ ., data=bioChemists)
summary(modz)

## smaller model
modz2 = zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists)

## summary table for smaller model
summary(modz2)

## test of nested models
anova(modz2, modz, test = "LRT")


## do by hand
pchisq(-2*(modz2$loglik - modz$loglik), 6, lower = FALSE)

## get estimates of mean-value parameters by hand
modz2 = zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists, 
                 x = TRUE)
M = modz2$x
betahat = coef(modz2)
betahatp = betahat[1:4]
betahatb = betahat[5:6]
Mp = M[[1]]
Mb = M[[2]]

thetap = as.numeric(Mp %*% betahatp)
thetab = as.numeric(Mb %*% betahatb)

mup = exp(thetap)
mub = 1/(1 + exp(-thetab))

(1 - mub) * mup

preds = as.numeric(predict(modz2))

all.equal((1 - mub) * mup, preds)


# zero-truncated regression 
require(foreign)
require(ggplot2)
require(VGAM)
require(boot)

dat = read.dta("https://stats.idre.ucla.edu/stat/data/ztp.dta")
head(dat)
