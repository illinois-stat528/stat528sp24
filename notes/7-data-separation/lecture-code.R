
library(tidyverse)

# Agresti example setup and summary

## create data

## separation vector 

## model matrix 

## check for separation

## model fit and summary

## submodel canonical parameter estimates

## saturated model canonical parameter estimates

## saturated model mean value parameter estimates

## LRT



## change optimization parameters

## control = list(maxit = 1e4, epsilon = 1e-100))

## compare parameter estimates
cbind(betahat, betahat2)
cbind(thetahat, thetahat2)
cbind(1/(1 + exp(-thetahat)), 1/(1 + exp(-thetahat2)))


## asymptote of log likelihood

## canonical statistic on the boundary of support

## Inverse Fisher Information degenerate
