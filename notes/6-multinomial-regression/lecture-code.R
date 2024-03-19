
## load software
library(VGAM)  # has model-fitting functions
library(tidyverse)

## load data
bball = read.csv("stat528sp24/notes/6-multinomial-regression/bball.csv")
bball$events = as.factor(bball$events)
head(bball)

## summaries of launch angle, launch speed, spray angle

## box plot of quantitative variables by events

## fit two models
mod1 = vglm(events ~ launch_speed + launch_angle + spray_angle, 
            family=multinomial, data=bball)

mod2 = vglm(events ~ launch_speed + launch_angle + spray_angle + 
              I(launch_angle^2), 
            family=multinomial, data=bball)

## LRT
llrts = deviance(mod1) - deviance(mod2)
llrts.df = df.residual(mod1) - df.residual(mod2)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## AIC
AIC(mod1); AIC(mod2)

## 6th order polynomial in spray angle
system.time({mod3 = vglm(events ~ launch_speed + 
                           launch_angle + spray_angle + I(launch_angle^2) + 
                           I(spray_angle^2) + I(spray_angle^3) + 
                           I(spray_angle^4) + I(spray_angle^5) + 
                           I(spray_angle^6), family=multinomial, data=bball)})


llrts = deviance(mod2) - deviance(mod3)
llrts.df = df.residual(mod2) - df.residual(mod3)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## AIC
AIC(mod1); AIC(mod2); AIC(mod3)


## add interaction terms
system.time({mod4 = vglm(events ~ launch_speed + 
                           launch_angle + spray_angle + I(launch_angle^2) + 
                           I(spray_angle^2) + I(spray_angle^3) + I(spray_angle^4) + 
                           I(spray_angle^5) + I(spray_angle^6) + 
                           I(spray_angle*launch_angle) + I(spray_angle*launch_speed) + 
                           I(launch_angle*launch_speed), 
                         family=multinomial, data=bball)})

llrts = deviance(mod3) - deviance(mod4)
llrts.df = df.residual(mod3) - df.residual(mod4)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

AIC(mod1); AIC(mod2); AIC(mod3); AIC(mod4)
## summary table takes a long time to load
#s4 = summary(mod4)
#s4

## predictions 

### median launch angle and launch speed
summary(bball$launch_angle)
summary(bball$launch_speed)

new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 90.30, 
                      launch_angle = 12)

# type = response for category probabilities
pred = predict(mod4, newdata = new_data, type = "response")

par(mfrow = c(2,2), oma = c(4,4,0,0), mar = c(1,2,1,1))
plot.new()
title("single")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 1]), max(pred[, 1])))
points(new_data$spray_angle, pred[, 1], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(2)

plot.new()
title("double")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 2]), max(pred[, 2])))
points(new_data$spray_angle, pred[, 2], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(2)

plot.new()
title("triple")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 3]), max(pred[, 3])))
points(new_data$spray_angle, pred[, 3], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(1)
axis(2)

plot.new()
title("home run")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 4]), max(pred[, 4])))
points(new_data$spray_angle, pred[, 4], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(1)
axis(2)


### well hit balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 100, 
                      launch_angle = 20)

### should-be home runs
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 110, 
                      launch_angle = 29)

### bad balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 70, 
                      launch_angle = 0)

### bad balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 80, 
                      launch_angle = 60)
