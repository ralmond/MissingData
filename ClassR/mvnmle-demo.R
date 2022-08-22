## Testing out MVNMLE
library(mvnmle)

help(package="mvnmle")

## Their sample data set
data(apple)
help("apple")
summary(apple)
n <- nrow(apple)

apple.mle <- mlest(apple)
apple.mle <- mlest(apple,print.level=1)  ## Show details

apple.muhat <- apple.mle$muhat  # mean vector
apple.sigmahat <- apple.mle$sigmahat # Covariance matrix

source("matSweep.R")
### To apply sweep operator, first paste mean vector and covariance matrix together.

musighat <- rbind(c(1/n, apple.muhat),
                  cbind(apple.muhat,apple.sigmahat))

apple.reg <- matSweep(musighat,2)
apple.reg.param <- apple.reg[1:2,3]
apple.reg.sse <- apple.reg[3,3]

### Compare to simple regression
apple.lm0 <- lm(worms~size,data=as.data.frame(apple),na.action=na.omit)
apple.lm0

## Bootstrap estimates of parameters and s.e.s
Nboot <- 100
apple.boot.param <- matrix(NA,Nboot,length(apple.reg.param))
apple.boot.sse <- rep(NA,Nboot)
colnames(apple.boot.param) <- c("intercept","slope")
for ( iBoot in 1:Nboot) {
  ## bootstrap sample
  apple.boot <- apple[sample.int(n,n,replace=TRUE),]
  ## MLE
  apple.mleboot <- mlest(apple.boot)
  ## Mu Sigma to regression parameters
  apple.boot.musighat <- 
    rbind (c (1/n, apple.mleboot$muhat),
           cbind(apple.mleboot$muhat,apple.mleboot$sigmahat))
  apple.boot.reg <- matSweep(apple.boot.musighat,2)
  ## Stash result
  apple.boot.param[iBoot,] <- apple.boot.reg[1:2,3]
  apple.boot.sse[iBoot] <- apple.boot.reg[3,3]
}


colMeans(apple.boot.param)
apply(apple.boot.param,2,sd)



### Summary

## 1) Use mlest() to get muhat and Sigmahat
## 2) build the combined matrix and use matSweep() to sweep out rows corresponding
## to predictors
## 3) Use the bootstrap to calculate standard errors.
