### Problem 2 -- Unconditional Mean Inputation

#create sample with missing data

df <- data.frame(x = 1:20, y = c(1:10,rep(NA,10)),
                 z = c(1:15,rep(NA,5)))

df

#impute unconditional mean for missing data
df.imp <- df # Copy to keep original
for (i in 1:ncol(df.imp)) {
  if (anyNA(df.imp[,i])) {
    df.imp[is.na(df.imp[,i]),i] <- mean(df.imp[,i], na.rm=TRUE)
  }
}


df.imp

##3
cat <- c(1,2,2,1,2,1,1,2,1,2)
missing <- c(6,12,NA,9,12,NA,NA,NA,12,12)
working <- cbind(cat, missing)
working
test <- split(missing,cat)
test
means <- sapply(test,mean,na.rm=TRUE)
#meanone <- mean(test$"1", na.rm = TRUE)
#meantwo <- mean(test$"2", na.rm = TRUE)
means[cat]
work.imp <- working
work.imp[is.na(work.imp[,"missing"]),"missing"] <- 
  means[cat][is.na(work.imp[,"missing"])]
# missingone<-test$"1"
# missingtwo<-test$"2"
# missingone[is.na(test$"1")] <- meanone
# missingtwo[is.na(test$"2")] <- meantwo
# c(missingone, missingtwo)

#4. Perform a regression imputation on a single variable.
library(MASS)
Carsdata<-data(Cars93) #bringing in Cars93 dataset from MASS
Carsdata<-Cars93
is.na(Carsdata[1:3, "MPG.city"])<-TRUE
fit1<-lm(MPG.city~MPG.highway,data=Carsdata, na.action=na.exclude) #runnning regression
summary(fit1) #viewing results
head(Carsdata$MPG.city) #checking NAs
pred1<-predict(fit1,Carsdata) #predicting NAs
head(pred1) #checking predictions for NAs
Carsdata[is.na(Carsdata[,"MPG.city"]),"MPG.city"] <- 
  pred1[is.na(Carsdata[,"MPG.city"])]
head(Carsdata$MPG.city)
### 5
setwd("C:/EDF6937MissingData")
install.packages("Hmisc")
install.packages("stargazer")
library(Hmisc)
library(stargazer)

set.seed(12345)
x1 <- rnorm(150, mean = 80, sd = 12)
y <- 2 + x1*3
for (i in 1:length(y)) {
  y[i] <- y[i] + rnorm(1, 0, 40)
}
x2 <- rnorm(150, mean = 50, sd = 15)
df <- as.data.frame(cbind(y, x1, x2))
head(df)

df1 <- df

#randomly replace x values with NAs
df1$y[sample(1:length(df1$y),40)] <- NA
summary(df1)
head(df1)
#linear model with excluded values
model.exclude <- lm(y ~ x1, data = df1, na.action = na.exclude)

## Stochastic imputation
imp.model <- predict(model.exclude, df1, se.fit = TRUE)

## Calculate prediction se's
imp.model$se.pred <- sqrt(imp.model$se.fit^2 + imp.model$residual.scale^2)
y.fitted <- rnorm(imp.model$fit, imp.model$fit, imp.model$se.pred)

y.fitted <- as.data.frame(y.fitted)
df.final <- cbind(df1, y.fitted)

#Replace NAs with fitted values
df.final$new.y <- df.final$y
my.na <- is.na(df.final$y)
df.final$new.y[my.na]<- df.final$y.fitted[my.na]
head(df.final)
df.final <- as.data.frame(df.final)

#Compare scatterplots
model.true <- lm(y ~ x1, data = df)
plot(x1,y)
abline(model.true, col="green")

model.exclude <- lm(y ~ x1, data = df1, na.action = na.exclude)
#points(x1, y)
abline(model.exclude, col = "blue")
model.imputed <- lm(new.y ~ x1, data = df.final)
points(x1,df.final$new.y,col=ifelse(df.final$y!=df.final$new.y, "black", "red") )
abline(model.imputed, col = "red")

stargazer(model.true, model.exclude, model.imputed, type="text",
          title="Regression Results",align=TRUE,
          dep.var.labels=c("y","new.y"),
          out="models.txt")



#### 6
install.packages("alr4")
library(alr4)
sniffer[1:5,]
# No any missing values
names(sniffer)
sniffer$TankTemp[116:125]<-NA   # for monotone missing pattern
sniffer$GasTemp[106:125]<-NA    # for monotone missing pattern
sniffer$TankPres[96:125]<-NA    # for monotone missing pattern
sniffer$GasPres[86:125]<-NA     # for monotone missing pattern
sniffer$Y[76:125]<-NA           # for monotone missing pattern
summary(sniffer)
sniffer
# sniffer<-sniffer[,sample(1:5)]  # if you want to shuffle NA's use this two command
# sniffer<-sniffer[sample(1:125),] # to be able to shuffle NA's 
#######
summary(sniffer)
names(sniffer)
plot(Y~TankTemp,data=sniffer)
plot(Y~GasTemp,data=sniffer)
plot(Y~TankPres,data=sniffer)
plot(Y~GasPres,data=sniffer)
library(lattice)

xyplot(Y~TankTemp,data=sniffer)
xyplot(Y~GasTemp,data=sniffer)
xyplot(Y~TankPres,data=sniffer)
xyplot(Y~GasPres,data=sniffer)

fit1=lm(Y~TankTemp+GasTemp+TankPres+GasPres, sniffer)
summary(fit1)
summary(fit1)$r.squared
f1=fit1$fitted
r1=fit1$resid

# head(fitted(fit1))
# head(residuals(fit1))

# pred1 <- predict(fit1,Cars1)
# head(pred1)
par(mfrow=c(1,1), oma=c(1,1,1,1),mar=c(4,6,3,4))

plot(f1,r1, xlab="Fitted Value", ylab="Residual",main="Unweighted Model",ylim=c(-8,8))
pred1<-predict(fit1,sniffer)
pred1
pred1i <- predict(fit1,sniffer,interval="prediction")
pred1i

fit2=lm(Y~TankTemp+GasTemp+TankPres+GasPres, data=sniffer, weights=1/GasPres)
summary(fit2)
f2=fit2$fitted
r2=fit2$resid
plot(f2,r2, xlab="Fitted Value", ylab="Residual",main="Weighted Model", ylim=c(-8,8))

fit3=lm(Y~GasTemp+GasPres, data=sniffer, weights=1/GasPres)
summary(fit3)
f3=fit3$fitted
r3=fit3$resid
plot(f3,r3, xlab="Fitted Value", ylab="Residual",main="Weighted Reduced Model", ylim=c(-8,8))

fit4<-lm(Y~GasTemp+GasPres+TankPres,data=sniffer)
summary(fit4)
f4<-fit4$fitted
r4<-fit4$resid
plot(f4,r4,xlab="Fitted Value",ylab="Residual")
anova(fit1,fit2,fit3,fit4)