

## Get CCSO data
library(stat528materials)
library(tidyverse)
data(CCSO)

names(CCSO)
?CCSO

## investigate other traffic offenses
## investiage at least one day in jail 
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1, 1, 0)) 


## consider basic demographic variables
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1, 1, 0)) %>% 
  group_by(sex) %>% 
  summarise(n = n(), mean(atleastone))

CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  nrow()

## consider CCSO-specifc variables (releaseReason, employmentStatus)
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1, 1, 0)) %>% 
  group_by(releasedReason) %>% 
  summarise(n = n(), mean(atleastone)) %>% 
  as.data.frame()

## "Served Sentence of Incarceration                 Y"



## data wrangling (simple demographic variables)
CCSO_small = CCSO %>% 
  mutate(atleastone = ifelse(daysInJail > 0,1,0)) %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>%  
  filter(race %in% c("Asian/Pacific Islander","Black","White","Hispanic")) %>% 
  filter(sex %in% c("Female","Male")) %>% 
  dplyr::select(atleastone, arrestAge, sex, race, bookingDate) %>%  
  mutate(race = fct_drop(race), sex = fct_drop(sex))
CCSO_small = CCSO_small[complete.cases(CCSO_small), ]
head(CCSO_small)
dim(CCSO_small)


## fit a basic model (race, sex, arrestAge)
?glm
m1 = glm(atleastone ~ race + sex + arrestAge, data = CCSO_small, 
    family = "binomial")
#glm.fit

## x = TRUE returns a model matrix
m1 = glm(atleastone ~ -1 + race + sex + arrestAge, data = CCSO_small, 
         family = "binomial", x = TRUE)
M = m1$x
head(M)

## summary table for coefficient estimates and other quantities
summary(m1)

## check standard errors against vcov (all.equal)
?vcov
cbind(summary(m1)$coef[, 2], sqrt(diag(vcov(m1))))
all.equal(summary(m1)$coef[, 2], sqrt(diag(vcov(m1))))

?all.equal
.Machine$double.eps
sqrt(.Machine$double.eps)
?.Machine

## compare with saturated model
pchisq(m1$deviance, df = m1$df.residual, lower = FALSE)
p1 = predict(m1, type = "response")
y = CCSO_small$atleastone
-2 * sum(y*log(p1) + (1-y)*log(1 - p1))


## compare with null model
-2 * sum(y*log(1/2) + (1-y)*log(1 - 1/2))

## compare with intercept only model (what is this?)
phat = mean(CCSO_small$atleastone)
deviance_intercept = -2 * sum(y*log(phat) + (1-y)*log(1 - phat))
pchisq(deviance_intercept - m1$deviance, df = 5,lower = FALSE)
mnull = glm(atleastone ~ 1, data = CCSO_small, 
            family = "binomial")
anova(mnull, m1, test = "LRT")

unique(predict(mnull, type = "response"))
phat

## investigate sex
msmall = glm(atleastone ~ -1 + race +arrestAge, data = CCSO_small,
             family = "binomial")
summary(msmall)
anova(msmall, m1, test = "LRT")
AIC(msmall); AIC(m1)

## Esarey and Pierce (2012) diagnostic plot
library(heatmapFit)
y = CCSO_small$atleastone
p1 = predict(m1, type = "response")
heatmap.fit(y = y, pred = p1)

psmall = predict(msmall, type = "response")
heatmap.fit(y = y, pred = psmall)

## investigate age (higher order terms)
CCSO_small %>% 
  ggplot() +
  aes(x = arrestAge, y = atleastone) + 
  geom_smooth()

m_big = glm(atleastone ~ -1 + race + sex + poly(arrestAge, 3), 
            data = CCSO_small, family = "binomial")
summary(m_big)

BIC(m1, m_big)
anova(m1, m_big, test = "LRT")

heatmap.fit(y = y, pred = predict(m_big, type = "response"))


## careful with interpretation
m1 = glm(atleastone ~ -1 + race + sex + arrestAge, data = CCSO_small, 
         family = "binomial", x = "TRUE")
m2 = glm(atleastone ~ race + sex + arrestAge, data = CCSO_small, 
         family = "binomial", x = "TRUE")
cbind(coef(m1),coef(m2))
all.equal(p1, predict(m2, type = "response"))
AIC(m1)
AIC(m2)

M3 = M %*% eigen(vcov(m1))$vec
m3 = glm(CCSO_small$atleastone ~ -1 + M3, family = "binomial")
cbind(coef(m1),coef(m2),coef(m3))
AIC(m1)
AIC(m2)
AIC(m3)
all.equal(p1, predict(m3, type = "response"))


#### Pick up here
head(CCSO_small)


## get parameter estimates 

### beta -> theta
summary(m1)
beta = coef(m1)
theta = as.numeric(M %*% beta)
all.equal(theta, as.numeric(predict(m1, type = "link")))

### theta -> mu
mu = 1 / (1 + exp(-theta))
all.equal(mu, as.numeric(predict(m1, type = "response")))



## nonparametric bootstrap (be mindful of sparse categorical 
## level in this demonstration)

head(CCSO_small)



CCSO_small$race = 
  relevel(CCSO_small$race, ref = 2)



CCSO_small2 = CCSO_small %>% 
  filter(race != "Asian/Pacific Islander")


## slow
betastar = function(){
  mod = glm(atleastone ~ race + sex + arrestAge, 
            data = CCSO_small[sample(1:nrow(CCSO_small), replace = TRUE), ], 
           family = "binomial")
  coef(mod)
}
B = 2e3
system.time({
  bootsamp = do.call(rbind, lapply(1:B, function(j) betastar() ))  
})
bootse = sqrt(diag(var(bootsamp)))

m1 = glm(atleastone ~ race + sex + arrestAge, 
          data = CCSO_small, 
          family = "binomial")

cbind(bootse, summary(m1)$coef[, 2])


## faster (mclapply)
library(parallel)
ncores = detectCores() - 2

system.time({
  bootsamp2 = do.call(rbind, mclapply(1:B, function(j) betastar(), 
                                      mc.cores = ncores))  
})
bootse2 = sqrt(diag(var(bootsamp)))
cbind(bootse, bootse2, summary(m1)$coef[, 2])

## faster (foreach)
library(foreach)
library(doParallel)
system.time({
  myCluster = makeCluster(ncores) # number of cores to use
  registerDoParallel(myCluster)
  bootsamp = foreach(i=1:2e3, .combine=rbind) %dopar% betastar()  
  stopCluster(myCluster) # stop cluster when done
})
bootse3 = sqrt(diag(var(bootsamp)))



## compare variability
round(cbind(summary(m1)$coef[, 2], 
            bootse,
            bootse2,
            bootse3), 3)

