# causal inference methods
# 
# implementation details:
# if 'X -> Y' they output cd  = 1
# if 'Y -> X' they output cd = 0
# epsilon: confidence (or score)
library(rvinecopulib)
library(Hmisc)
library(statmod)
library(copula)
## Proper scoring rule for predicted quantiles
quantileScoring <- function(actual, pred, prob = 0.95) {
  mean((as.numeric(actual <= pred) - prob) * (pred - actual))
}



QCCDWrapper<- function(pair){
  # changing the seed might slightly affect the results due to the random jittering
  # in the rank function
  set.seed(0)
  n <- length(pair[,1])
  X = pair[,1]
  Y = pair[,2]
  x = X
  y = Y
  
  # Recover pseudo-observations and estimate the copula non-parametrically
  u1 <- rank(x, ties.method = "random")/(n + 1)
  u2 <- rank(y, ties.method = "random")/(n + 1)
  a <- acepack::ace(x, y)
  cop <- bicop(data = cbind(u1,u2),
               family_set = "tll",
               nonpar_method = "constant",
               mult = n^(1/6 - 1/5) * abs(cor(x, y)) / abs(cor(a$tx, a$ty)))
  
  uw <- gauss.quad.prob(3)
  h <- sapply(uw$nodes, function(uu) {
    
    u1p <- predict(object = cop, newdata = cbind(uu, u2), what = "hinv2")
    u2p <- predict(object = cop, newdata = cbind(u1, uu), what = "hinv1")
    xp <- quantile(x, u1p)
    yp <- quantile(y, u2p)
    
    h1 <- quantileScoring(x, xp, uu)
    h2 <- quantileScoring(y, yp, uu)
    
    rel_sc = h1/(h1 + h2)
    rel_sc
  })
  r1 <- sum(uw$weights[!is.na(h)]*h[!is.na(h)])/sum(uw$weights[!is.na(h)])   
  
  cd = ifelse(r1 > 0.5, 1, 0)
  return(list(cd = cd, epsilon = r1))
  
}
