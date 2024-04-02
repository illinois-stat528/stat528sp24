

library(tidyverse)
library(lme4)
library(ggplot2)
library(faraway)

################
# ANOVA and REML
################

data(pulp)
?pulp
## note that we are changing the contrasts to sum contrasts
op = options(contrasts = c("contr.sum", "contr.poly"))
## aov is another wrapper for lm; results more appropriate for ANOVA models
lmod = aov(bright ~ operator, pulp)
summary(lmod)
coef(lmod)

## (MSA - MSE) / n 
(0.4467 - 0.1062)/5

mmod = lmer(bright ~ 1 + (1|operator), pulp)
summary(mmod)

######################
# Parametric bootstrap
######################

## null model fit
nullmod = lm(bright ~ 1, pulp) 

# alternative model fit
smod = lmer(bright ~ 1 + (1|operator), pulp, REML = FALSE)

LRT = as.numeric(2*(logLik(smod) - logLik(nullmod)))
LRT
pchisq(LRT, df = 1, lower = FALSE)

set.seed(13)
B = 1000
lrtstat = numeric(B)
system.time(for(b in 1:B){
  y = unlist(simulate(nullmod))
  bnull = lm(y ~ 1)
  balt = lmer(y ~ 1 + (1|operator), pulp, REML = FALSE)
  lrtstat[b] = as.numeric(2*(logLik(balt) - logLik(bnull)))
})

mean(lrtstat < 1e-5)

## p-value
pval = mean(lrtstat > 2.568371)
pval

## simple standard error of the above
sqrt(pval*(1-pval)/B)



###########################
# Predicting random effects
###########################

## operator random effects
ranef(mmod)$operator

## operator fixed effects from linear model
(cc = model.tables(lmod))

## estimated fixed effects divided by predicted random effects
## shrinkage
cc[[1]]$operator / ranef(mmod)$operator

## 95% CI for predicted random effects
library(lattice)
dotplot(ranef(mmod, condVar=TRUE))

## total operator effect
fixef(mmod)
fixef(mmod) + ranef(mmod)$operator

## get a 95% CI for a new observation for operator a
group.sd = as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd = as.data.frame(VarCorr(mmod))$sdcor[2]
B = 1000
pva = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u=TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pva[i] = predict(bmod, 
                  newdata=data.frame(operator="a")) + 
    rnorm(n=1,sd=resid.sd)
})

quantile(pva, c(0.025, 0.975))
fixef(mmod)


## get a 95% CI for a new observation for operator b
group.sd = as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd = as.data.frame(VarCorr(mmod))$sdcor[2]
B = 1000
pvb = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u=TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pvb[i] = predict(bmod, 
                  newdata=data.frame(operator="b")) + 
    rnorm(n=1,sd=resid.sd)
})

quantile(pvb, c(0.025, 0.975))
fixef(mmod)



dat = read_csv("stat528sp24/notes/8-linear-mixed-models/soybean.csv")
head(dat)

foo = dat %>% select(Date, year, ID, plot_number, 
  plot_number_year, mAqE, Precip_cum, Precip_7day, 
  Ta_7day, Fsd_7day, Date_num)

m1_mAqE_lm = lm(mAqE ~ ID + Date_num + I(Date_num^2),
               data = foo) 

m1_mAqE = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
  (1|plot_number_year),
  data = foo, REML = FALSE, 
  control = lmerControl(optimizer ="Nelder_Mead")) 

m1_mAqE_full = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
  Precip_cum + Precip_7day + Ta_7day + Fsd_7day + (1|plot_number_year),
  data = foo, REML = FALSE, 
  control = lmerControl(optimizer ="Nelder_Mead")) 

AIC(m1_mAqE_lm)
AIC(m1_mAqE)
AIC(m1_mAqE_full)


par(mfrow = c(1,2))
plot(fitted(m1_mAqE_full), residuals(m1_mAqE_full), 
     xlab="Fitted", ylab="Residuals", pch = 19, 
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m1_mAqE_full), main="", pch = 19, 
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m1_mAqE_full))


## AIC for each ID variable from full AqE fixed-effects model
M = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2) + 
                   Precip_cum + Precip_7day + Ta_7day + Fsd_7day, 
                 data = dat)

# Note that likelihood ratios are asymptotic, i.e. don't account for 
# uncertainty in the estimate of the residual variance
library(parallel)
ncores = detectCores() - 2
system.time({AIC_IDs = matrix(unlist(lapply(
  grep("ID", colnames(M)), function(j){
    M1 = M[, -j]	
    foo = lmer(mAqE ~ -1 + M1 + (1|plot_number_year), 
               data = dat, REML = FALSE, 
               control = lmerControl(optimizer ="Nelder_Mead"))	
    AIC(m1_mAqE_full) - AIC(foo) 
  })), ncol = 1)})
rownames(AIC_IDs) = colnames(M)[grep("ID", colnames(M))]
colnames(AIC_IDs) = c("AqE")
cbind(round(AIC_IDs,2), ifelse(AIC_IDs < 0, 1, 0))

