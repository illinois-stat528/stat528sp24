
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



