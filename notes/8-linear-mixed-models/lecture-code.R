

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


# Parametric bootstrap

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



# Predicting random effects

## operator fixed effects from linear model
(cc = model.tables(lmod))
pulp %>% group_by(operator) %>% 
  summarise(bright = mean(bright)) %>% 
  select(bright) - mean(pulp$bright)

## operator random effects
ranef(mmod)$operator

## estimated fixed effects divided by predicted random effects
## shrinkage
cc[[1]]$operator / ranef(mmod)$operator

## 95% CI for predicted random effects
library(lattice)
dotplot(ranef(mmod, condVar=TRUE))

## total operator effect
fixef(mmod)
fixef(mmod) + ranef(mmod)$operator


## get a 95% CI for a new observation (operator unspecified)
group.sd = as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd = as.data.frame(VarCorr(mmod))$sdcor[2]
?simulate.merMod
B = 1000
pv = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u = TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pv[i] = predict(bmod, re.form=~0)[1] + 
    rnorm(n=1,sd=group.sd) +  
    rnorm(n=1,sd=resid.sd)
})
quantile(pv, c(0.025, 0.975))


## get a 95% CI for a new observation for operator a
?simulate.merMod
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

data.frame(
  operator = rep(c("u","a","b"), each=1e3),
  bright = c(pv,pva,pvb)
) %>% 
  ggplot() + 
  aes(x = bright, color = operator) + 
  geom_density() + 
  theme_minimal() + 
  geom_vline(aes(xintercept = 60.4))
  


# Soybean analysis
dat = read_csv("stat528sp24/notes/8-linear-mixed-models/soybean.csv")
head(dat)

## subset of data
foo = dat %>% select(Date, year, ID, plot_number, 
  plot_number_year, mAqE, Precip_cum, Precip_7day, 
  Ta_7day, Fsd_7day, Date_num)
dim(foo)

## linear model
m1_mAqE_lm = lm(mAqE ~ ID + Date_num + I(Date_num^2),
               data = foo) 

## random effect for plot
m1_mAqE = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
  (1|plot_number_year),
  data = foo, REML = FALSE, 
  control = lmerControl(optimizer ="Nelder_Mead")) 

## random effect for plot plus weather variables
m1_mAqE_full = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
  Precip_cum + Precip_7day + Ta_7day + Fsd_7day + (1|plot_number_year),
  data = foo, REML = FALSE, 
  control = lmerControl(optimizer ="Nelder_Mead")) 

AIC(m1_mAqE_lm)
AIC(m1_mAqE)
AIC(m1_mAqE_full)

summary(m1_mAqE_full)

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

# Note that likelihood ratios are asymptotic, i.e. don't 
# account for uncertainty in the estimate of the residual 
# variance
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


bar = data.frame(
  AIC = as.numeric(round(AIC_IDs,2)), 
  selection = as.numeric(ifelse(AIC_IDs < 0, 1, 0)), 
  coefficient = summary(m1_mAqE_full)$coef[2:41, 1]
)
bar



# split plot analysis
?irrigation
data(irrigation)

## data summary
summary(irrigation)

## data visualization
ggplot(irrigation, aes(y=yield, x=field, shape=irrigation, color= variety)) +
  geom_point(size = 2) +
  theme_minimal()

lmod = lmer(yield ~ irrigation * variety + (1|field) + (1|field:variety),
            data = irrigation)

lmod = lmer(yield ~ irrigation * variety + (1|field),
            data = irrigation)
sumary(lmod)

library(pbkrtest)
lmoda = lmer(yield ~ irrigation + variety + (1|field), data=irrigation)
?KRmodcomp
KRmodcomp(lmod, lmoda)

lmodi = lmer(yield ~ irrigation + (1|field), irrigation)
KRmodcomp(lmoda, lmodi)

lmodv = lmer(yield ~ variety + (1|field), irrigation)
KRmodcomp(lmoda, lmodv)

par(mfrow = c(1,2))
plot(fitted(lmod), residuals(lmod), xlab="Fitted", ylab="Residuals", pch = 19)
qqnorm(residuals(lmod), main="", pch = 19)




# Nested random effects via an example
?eggs
data(eggs)
summary(eggs)

ggplot(eggs, aes(y=Fat, x=Lab, color=Technician, shape=Sample)) +
  geom_point(size = 2, 
             position = position_jitter(width=0.1, height=0.0)) +
  scale_color_grey() +
  theme_minimal()

cmod = lmer(Fat ~ 1 + (1|Lab) + 
              (1|Lab:Technician) + 
              (1|Lab:Technician:Sample), data=eggs)
sumary(cmod)
confint(cmod, method="boot")

cmod2 = lmer(Fat ~ 1 + 
               (1|Lab) + 
               (1|Lab:Technician), data=eggs)
as.matrix(model.matrix(cmod2, type = "random"))
confint(cmod2, method="boot")

cmod3 = lmer(Fat ~ 1 + (1|Lab), data=eggs)
confint(cmod3, method="boot")


cmod = lmer(Fat ~ 1 + 
              (1|Lab) + 
              (1|Lab:Technician) + 
              (1|Lab:Technician:Sample), data=eggs, 
            REML = FALSE)
cmod2 = lmer(Fat ~ 1 + 
               (1|Lab) + 
               (1|Lab:Technician), data=eggs, 
             REML = FALSE)
cmod3 = lmer(Fat ~ 1 + (1|Lab), data=eggs, REML = FALSE)

AIC(cmod)
AIC(cmod2)
AIC(cmod3)

confint(cmod, method="boot")
confint(cmod2, method="boot")
confint(cmod3, method="boot")



###########################################
## Example: Panel Study of Income Dynamics
###########################################

library(tidyverse)
data(psid)
?psid

psid20 = filter(psid, person <= 20)
ggplot(psid20, aes(x=year, y=income)) +
  geom_line() +
  facet_wrap(~ person) +
  theme_minimal()


ggplot(psid20, aes(x=year, y=income+100, group=person)) +
  geom_line() +
  facet_wrap(~ sex) +
  scale_y_log10() +
  theme_minimal()

lmod = lm(log(income) ~ I(year-78), subset=(person==1), psid)
coef(lmod)

ml = lmList(log(income) ~ I(year-78) | person, psid)
intercepts = sapply(ml,coef)[1,]
slopes = sapply(ml,coef)[2,]

plot(intercepts,slopes,xlab="Intercept",ylab="Slope")
psex = psid$sex[match(1:85,psid$person)]
boxplot(split(slopes,psex))

t.test(slopes[psex=="M"],slopes[psex=="F"])
t.test(intercepts[psex=="M"],intercepts[psex=="F"])


psid$cyear = psid$year - 78
mmod = lmer(log(income) ~ cyear*sex + age + educ + (cyear|person), psid)
sumary(mmod, digits=3)
