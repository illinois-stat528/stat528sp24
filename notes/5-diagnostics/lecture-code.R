
set.seed(13)
p = 3
n = 5e3
beta = rep(1,p+1)
X = cbind(1, matrix(rnorm(n*p), nrow = n, ncol = p))
Y = rbinom(n, size = 1, prob = 1/(1 + exp(-X %*% beta)))
p1 = predict(glm(Y ~ -1 + X[, 2], family = "binomial"), type = "response")


K = function(x) (1 - abs(x)^3)^3

wls = function(m){
  x = m-p1
  w = sapply(x, K)
  as.numeric(coef(lm(Y ~ x, weights = w))[1])
}


x = seq(from = 0, to = 1, by = 1/1000)
y = sapply(x, wls)  


foo = data.frame(x = x, y = y)
ggplot(foo) + 
  aes(x = x, y = y) +
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) + 
  theme_minimal()


library(tidyverse)
source("stat528sp24/notes/5-diagnostics/yang2021supp.R")


## true model
set.seed(13)
n = 500
p = 2
beta = rep(1,p+1)
M = cbind(1, matrix(rnorm(n*p), nrow = n, ncol = p))
Y = rpois(n = n, lambda = exp(M %*% beta))
m1 = glm(Y ~ -1 + M, family = "poisson")
lambdahat = m1$fitted.values

Delta = function(k, x){
  cumsum(dpois(0:k, lambda = exp(coef(m1) %*% x)) )
}


y = sort(unlist(do.call(rbind, lapply(1:1e2, function(j) Delta(k = 10, x = c(1, rnorm(p)))))))
hist(y)
y


## calculate bandwidth parameter
h = bandwidthp(y = Y, lambdaf = lambdahat)

## diagnostic plot using the marginesti function
data.frame(s = seq(0,1,length = 1001)) %>% 
  mutate(y = sapply(s, function(u) {
    marginesti(u, y = Y, lambdaf = lambdahat)
  })) %>% 
  ggplot() + 
  aes(x = s, y = y) + 
  geom_line() + 
  geom_abline(col = "red", lty = 2) + 
  labs(title = "QERDF for Poisson (true model)", 
       x = "s", y = "U(s)") + 
  theme_minimal()


m2 = glm(Y ~ -1 + M[, -p], family = "poisson")
lambdahat = m2$fitted.values

## calculate bandwidth parameter
h = bandwidthp(y = Y, lambdaf = lambdahat)

## diagnostic plot using the marginesti function
data.frame(s = seq(0,1,length = 1001)) %>% 
  mutate(y = sapply(s, function(u) {
    marginesti(u, y = Y, lambdaf = lambdahat)
  })) %>% 
  ggplot() + 
  aes(x = s, y = y) + 
  geom_line() + 
  geom_abline(col = "red", lty = 2) + 
  labs(title = "QERDF for Poisson (missing covariate)", 
       x = "s", y = "U(s)") + 
  theme_minimal()


## classification example
library(lubridate)
library(caret)
library(pROC)
library(PresenceAbsence)

## balls put into play 2022 season
dat = read_csv("stat528sp24/notes/5-diagnostics/sc-hr-2022.csv")
head(dat)

m1 = glm(HR ~ launch_speed + launch_angle, data = dat, 
         family = "binomial")
summary(m1)
pchisq(deviance(m1), df.residual(m1), lower = FALSE)

preds_m1 = predict(m1, type = "response")
mean(preds_m1 >= 0.999)

y = dat$HR
confusionMatrix(
  data = as.factor(as.numeric(preds_m1 >= 0.5)), 
  reference = as.factor(y))

## add a quadratic term for launch angle
m2 = glm(HR ~ launch_speed + poly(launch_angle, 2), 
         data = dat, family = "binomial")
anova(m1, m2, test = "LRT")

preds_m2 = predict(m2, type = "response")
confusionMatrix(
  data = as.factor(as.numeric(preds_m2 >= 0.5)), 
  reference = as.factor(y))



roc_m1 = roc(y, preds_m1)
roc_m1
plot(roc_m1, print.auc = TRUE)

roc_m2 = roc(y, preds_m2) 
roc_m2
plot(roc_m2, print.auc = TRUE)

guesses = rbinom(nrow(dat), size = 1, prob = mean(y))
confusionMatrix(
  data = as.factor(as.numeric(guesses >= 0.5)), 
  reference = as.factor(y))

confusionMatrix(
  data = as.factor(c(rep(0,nrow(dat)-1),1)), 
  reference = as.factor(y))

roc_guess = roc(y, guesses) 
plot(roc_guess, print.auc = TRUE)

library(heatmapFit)
heatmap.fit(y = y, preds_m1)
heatmap.fit(y = y, preds_m2)


dat_aug = cbind(dat, preds_m2)
dat_aug %>% arrange(desc(preds_m2))
#https://www.reddit.com/r/baseball/comments/xll84y/would_it_dong_aaron_judge_vs_matt_barnes_repbx/
#https://youtu.be/0XUgX8Rvz-E?start=466&end=479


library(PresenceAbsence)
optimal.thresholds(DATA = data.frame(ID = seq_along(nrow(dat)), 
                                     obs = y, 
                                     pred = preds_m2)) %>% 
  as.data.frame() 
