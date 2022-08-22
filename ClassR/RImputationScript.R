### R and imputations

class(5)
class(df)
class(mydata)
class(c)

library(MASS)
data(Cars93) # pull in Cars93 data
class(Cars93)
summary(Cars93)
class(Cars93$Weight)
class(Cars93$Type)

summary
summary.default(Cars93$Weight)
summary.factor(Cars93$Type)
help("summary.lm")

## Methods command
methods("summary")
getS3method("summary","stl")
methods(class="lm")

## Plot and formulae
plot(Price ~ Weight, data=Cars93)
plot (Price~ Type, data=Cars93)
library(lattice) # For xyplot
xyplot(Price~Weight|Type,data=Cars93)

## A confusing error message
pilot <- rnorm(10)
mean(plot)

## Fitted and residuals
Cars1 <- Cars93
is.na(Cars1[1:3,"MPG.city"]) <- TRUE
fit1 <- lm(MPG.city ~ MPG.highway,data=Cars1,
           na.action=na.exclude)
summary(fit1)
summary(fit1)$r.squared


head(fitted(fit1))
head(residuals(fit1))

pred1 <- predict(fit1,Cars1)
head(pred1)

pred1i <- predict(fit1,Cars1,interval="prediction")
head(pred1i)

pred1s <- predict(fit1,Cars1,se.fit=TRUE)
pred1s

## Stochastic imputation
imp.model <- predict(fit1,Cars1,se.fit=TRUE)
## Calculate prediction se's
imp.model$se.pred <- sqrt(imp.model$se.fit^2 +
                          imp.model$residual.scale^2)
imps <- rnorm(nrow(Cars1),imp.model$fit,imp.model$se.pred)

## Updating
fit2 <- update(fit1,.~.+Weight)
fit3 <- update(fit2,.~.-MPG.highway)

anova(fit1,fit2,fit3)

logLik(fit2)
logLik(fit1)
deviance(fit2)

AIC(fit1)
AIC(fit3)


######
## New method, using sim()
library(arm)

### Draw random coefficients.
rsim <- sim(fit1,n.sims=1)

### Extract the X values.
c1.mm <- model.matrix(fit1)

### Do the matrix algebra for the prediction
pred.mean <- apply(sweep(c1.mm,2,coef(rsim),"*"),1,sum)

### Now do the imputations
imps <- rnorm(nrow(Cars1),pred.mean,sigma.hat(rsim))
