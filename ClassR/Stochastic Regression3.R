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
