
library(tidyverse)

# Agresti example setup and summary
x = (1:9 * 10)[-5]
y = c(0,0,0,0,1,1,1,1)
plot(x, y, pch = 19)

## separation vector
b = c(-50, 1) 

## model matrix
M = cbind(1, x)

## check condition
cbind(M %*% b, y)

## model fit
m1 = glm(y ~ x, family = "binomial")

## summary table
summary(m1)

## submodel canonical parameter estimates
betahat = m1$coefficients
betahat

## saturated model canonical parameter estimates
thetahat = as.numeric(M %*% betahat)
thetahat

## saturated model mean value parameter estimates
phat = predict(m1, type = "response")
phat
1/(1 + exp(-thetahat))

## variance matrices
vcov(m1)
eigen(vcov(m1))$val
eigen(solve(vcov(m1)))$val

## LRT
anova(m1, test = "LRT")

m2 = glm(y ~ x, family = "binomial", 
         control = list(maxit = 1e4, epsilon = 1e-100))
summary(m2)

## submodel canonical parameter estimates
betahat2 = m2$coefficients
betahat2

## saturated model canonical parameter estimates
thetahat2 = as.numeric(M %*% betahat2)
thetahat2

## saturated model mean value parameter estimates
phat2 = predict(m2, type = "response")
phat2
1/(1 + exp(-thetahat2))

vcov(m2)
eigen(vcov(m2))$val
eigen(solve(vcov(m2)))$val

cbind(betahat, betahat2)
cbind(thetahat, thetahat2)
cbind(1/(1 + exp(-thetahat)), 1/(1 + exp(-thetahat2)))


## asymptote of log likelihood
asymptote = t(sapply(1:30, function(iter){
  m1 = glm(y ~ x, family = "binomial", 
           control = list(maxit = iter, epsilon = 1e-20))
  c(sqrt(log(crossprod(coef(m1)))), logLik(m1))
}))
asymptote = as.data.frame(asymptote)

library(ggplot2)
ggplot(asymptote) + 
  ggtitle('asymptote of the log likelihood') + 
  labs(x= expression(log(~"||"~beta~"||")), y= "log likelihood") + 
  geom_line(aes(x = V1, y = V2), color = "black") + 
  geom_abline(intercept = 0, slope = 0, lty = 2, color = "red") + 
  theme(legend.position="bottom", panel.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line("lightgrey", size = 0.15), 
        panel.grid.major.y = element_line("lightgrey", size = 0.15), 
        panel.grid.minor.x = element_line("lightgrey", size = 0.07), 
        panel.grid.minor.y = element_line("lightgrey", size = 0.07))

ggplot(asymptote[25:30, ]) + 
  ggtitle('zoomed in') + 
  labs(x= expression(log(~"||"~beta~"||")), y = "") + 
  geom_line(aes(x = V1, y = V2), color = "black") + 
  geom_abline(intercept = 0, slope = 0, lty = 2, color = "red") + 
  theme(legend.position="bottom", panel.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line("lightgrey", size = 0.15), 
        panel.grid.major.y = element_line("lightgrey", size = 0.15), 
        panel.grid.minor.x = element_line("lightgrey", size = 0.07), 
        panel.grid.minor.y = element_line("lightgrey", size = 0.07))


invFI = vcov(m1)
FI = solve(invFI)
eigen(FI)
M %*% eigen(FI)$vec


library(glmdr)
eigen(FI)

m_glmdr = glmdr(y ~ x, family = "binomial")

## summary information
summary(m_glmdr)

## one-sided CIs
CIs = inference(m_glmdr)
cbind(CIs, y, x)

bounds.lower.p = CIs$lower
bounds.upper.p = CIs$upper
par(mar = c(4, 4, 0, 0) + 0.1)
plot(x, y, axes = FALSE, type = "n",
     xlab = expression(x), ylab = expression(mu(x)))
segments(x, bounds.lower.p, x, bounds.upper.p, lwd = 2)
box()
axis(side = 1)
axis(side = 2)
points(x, y, pch = 21, bg = "white")
