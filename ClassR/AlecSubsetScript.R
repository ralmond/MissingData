## Work with Alec subset
library(foreign)
library(effsize)
alecsmall <- as.data.frame(read.spss("Alec-5400Subset.sav"))
summary(alecsmall)

## -9 values are missing codes.  Fix that.
for (v in names(alecsmall)) {
  alecsmall[[v]] <- ifelse(alecsmall[[v]]==-9,NA,alecsmall[[v]])
}
summary(alecsmall)

## Lost labels, so put them back
alecsmall$group <- factor(alecsmall$group,labels=c("ADHD","control"))
alecsmall$YEAR <- ordered(alecsmall$YEAR,
                          labels=c("freshman","sophmore","junior","senior",
                                   "grad student"))
alecsmall$GENDER <- factor(alecsmall$GENDER,labels=c("female","male"))
alecsmall$ETHNICITY <- factor(alecsmall$ETHNICITY,
                              labels=c("White","African-American","Asian",
                                       "Hispanic","Other"))
alecsmall$SAT <- factor(alecsmall$SAT,labels=c("no","yes"))
summary(alecsmall)


#######
## Research Question 1:  Do ADHD and control students differ in academic anxiety
## and panic attacks

plot(genanxa ~ group, data=alecsmall)

lapply(split(alecsmall$genanxa,alecsmall$group),mean,na.rm=TRUE)
table(is.na(alecsmall$genanxa),alecsmall$group)

## Build an imputation model for genanxa
## Start by looking at correlations
print(cor(alecsmall[,c(3,7:15)],use="pairwise.complete.obs"),digits=2)
lapply(split(alecsmall$genanxa,alecsmall$ETHNICITY),mean,na.rm=TRUE)
lapply(split(alecsmall$genanxa,alecsmall$GENDER),mean,na.rm=TRUE)
lapply(split(alecsmall$genanxa,alecsmall$YEAR),mean,na.rm=TRUE)

## Now fit some models
imp1 <- lm (genanxa ~ ETHNICITY + inatt + hyper,data=alecsmall,
            na.action = na.exclude)
summary(imp1)
imp2 <- lm (genanxa ~ ETHNICITY,data=alecsmall,
            na.action = na.exclude)
summary(imp2)
imp3 <- lm (genanxa ~ ETHNICITY + YEAR+ AGE+ inatt + hyper,data=alecsmall,
            na.action = na.exclude)
summary(imp3)
imp4 <- lm (genanxa ~ ETHNICITY + YEAR+ inatt + hyper,data=alecsmall,
            na.action = na.exclude)
summary(imp4)
imp5 <- lm (genanxa ~ ETHNICITY + AGE+ inatt + hyper,data=alecsmall,
            na.action = na.exclude)
summary(imp5)
imp6 <- lm (genanxa ~ ETHNICITY + YEAR+ inatt*hyper,data=alecsmall,
            na.action = na.exclude)
## could also use:  inatt + hyper + inatt:hyper
summary(imp6)
imp7 <- lm (genanxa ~ ETHNICITY + YEAR + inatt + hyper*GENDER,data=alecsmall,
            na.action = na.exclude)
summary(imp7)
## Pick two imputation models
## Model 1 for hot deck imputation
## Model 6 for regression imputation

#########################
## Actual analysis
options(na.actions=na.fail) # Fail if there is unexpected missing data.


## 0.  Omit missing values.
tout0 <- t.test(genanxa ~ group, data=alecsmall, na.action=na.omit)
dout0 <- cohen.d(genanxa ~ group, data=alecsmall, na.action=na.omit)

## Calculating effect sizes (this is the hard way, easy way is up above)
tout0$sds <- sapply(split(alecsmall$genanxa,alecsmall$group),sd,
                    na.rm=TRUE)
tout0$ns <- sapply(split(alecsmall$genanxa,alecsmall$group),
                    function (g) sum(!is.na(g)))
tout0$sd <- sum(tout0$sds^2*(tout0$ns-1))/(sum(tout0$ns)-2)

tout0$effect.size <- -diff(tout0$estimate)/tout0$sd

baseline <- c(meandiff=-diff(tout0$estimate),
              se.diff=abs(diff(tout0$estimate))/tout0$statistic,
              effsize=dout0$estimate,
              se.effsize=sqrt(dout0$var) )

########################
## Allocate space for results
Nmethods <- 5
result.tab <- matrix(NA,Nmethods,length(baseline))
colnames(result.tab) <- names(baseline)
colnames(result.tab)[1:2] <- c("mean.diff", "se.diff")
rownames(result.tab) <- paste("meth",1:Nmethods)


#############################################
## Method 1 Grand Mean imputation
rownames(result.tab)[1] <- "Grand Mean"
## imputation
alecsmall.imp1 <- alecsmall
alecsmall.imp1$genanxa <- 
  ifelse(is.na(alecsmall.imp1$genanxa),
         mean(alecsmall.imp1$genanxa,na.rm=TRUE),
         alecsmall.imp1$genanxa)
## Analysis
# use na.fail here so R will detect problems in imputation.
tout1 <- t.test(genanxa ~ group, data=alecsmall.imp1, na.action=na.fail)
dout1 <- cohen.d(genanxa ~ group, data=alecsmall.imp1, na.action=na.fail)
## store results
result.tab[1,] <- c(meandiff=-diff(tout1$estimate),
                    se.diff=abs(diff(tout1$estimate))/tout1$statistic,
                    effsize=dout1$estimate,
                    se.effsize=sqrt(dout1$var) )

#############################################
## Method 2 Conditional Mean imputation
rownames(result.tab)[2] <- "Conditional Mean"
## imputation
alecsmall.imp2 <- alecsmall
## Make NAs in year a separate category.
## Alternative, use the addNA() function  
levels(alecsmall.imp2$YEAR) <- c(levels(alecsmall.imp2$YEAR),"Omitted")
alecsmall.imp2$YEAR[is.na(alecsmall.imp2$YEAR)] <- "Omitted"
## calculate means for each group
cmeans <- sapply(split(alecsmall.imp2$genanxa,alecsmall.imp2$YEAR),
                 mean,na.rm=TRUE)
## Use the grand mean for the imputations.
cmeans["Omitted"] <- mean(alecsmall.imp2$genanxa,na.rm=TRUE)

alecsmall.imp2$genanxa <- 
  ifelse(is.na(alecsmall.imp2$genanxa),
         cmeans[alecsmall.imp2$YEAR],
         alecsmall.imp2$genanxa)
## Analysis
tout2 <- t.test(genanxa ~ group, data=alecsmall.imp2, na.action=na.fail)
dout2 <- cohen.d(genanxa ~ group, data=alecsmall.imp2, na.action=na.fail)
## store results
result.tab[2,] <- c(meandiff=-diff(tout2$estimate),
                    se.diff=abs(diff(tout2$estimate))/tout2$statistic,
                    effsize=dout2$estimate,
                    se.effsize=sqrt(dout2$var) )

#### Alternate approach.  Do conditional mean and then unconditional mean imputation.
cmeans1 <- sapply(split(alecsmall$genanxa,alecsmall$YEAR),
                  mean,na.rm=TRUE)
alecsmall.imp2$genanxa1 <- 
  ifelse(is.na(alecsmall$genanxa),
         cmeans1[alecsmall$YEAR],
         alecsmall$genanxa)
alecsmall.imp2$genanxa1 <-
  ifelse(is.na(alecsmall.imp2$genanxa1),
         mean(alecsmall.imp2$genanxa1,na.rm=TRUE),
         alecsmall.imp2$genanxa1)

#############################################
## Method 3 Regression imputation
rownames(result.tab)[3] <- "Regression"
## imputation
alecsmall.imp3 <- alecsmall

## Fit imputation model
model3 <- lm (genanxa ~ ETHNICITY + YEAR + inatt + hyper*GENDER,data=alecsmall.imp3,
            na.action = na.exclude)
summary(model3)
## Mean imputation on other variables.  (This should be okay, because that is
## the right prediction)
alecsmall.imp3$inatt[is.na(alecsmall.imp3$inatt)] <- 
  mean(alecsmall.imp3$inatt,na.rm=TRUE)
alecsmall.imp3$hyper[is.na(alecsmall.imp3$hyper)] <- 
  mean(alecsmall.imp3$hyper,na.rm=TRUE)
## For categorical variables, impute the baseline.
alecsmall.imp3$ETHNICITY[is.na(alecsmall.imp3$ETHNICITY)] <- fmode(alecsmall.imp3$ETHNICITY) #"White"
alecsmall.imp3$YEAR[is.na(alecsmall.imp3$YEAR)] <- 
     median(alecsmall.imp3$YEAR,na.rm=TRUE) # "junior"  # in middle of distribution
#alecsmall.imp3$FEMALE[is.na(alecsmall.imp3$FEMALE)] <- "female"


model3.pred <- predict(model3,alecsmall.imp3)
summary(model3.pred)

alecsmall.imp3$genanxa <- 
  ifelse(is.na(alecsmall.imp3$genanxa),
         model3.pred,
         alecsmall.imp3$genanxa)
## Analysis
tout3 <- t.test(genanxa ~ group, data=alecsmall.imp3, na.action=na.fail)
dout3 <- cohen.d(genanxa ~ group, data=alecsmall.imp3, na.action=na.fail)
## store results
result.tab[3,] <- c(meandiff=-diff(tout3$estimate),
                    se.diff=abs(diff(tout3$estimate))/tout3$statistic,
                    effsize=dout3$estimate,
                    se.effsize=sqrt(dout3$var) )

#############################################
## Method 4 Stochastic Regression imputation
rownames(result.tab)[4] <- "Stochastic Regression"
## imputation
alecsmall.imp4 <- alecsmall

## Fit imputation model
model4 <- lm (genanxa ~ ETHNICITY + YEAR + inatt + hyper*GENDER,data=alecsmall.imp4,
              na.action = na.exclude)
summary(model4)
## Mean imputation on other variables.  (This should be okay, because that is
## the right prediction)
alecsmall.imp4$inatt[is.na(alecsmall.imp4$inatt)] <- 
  mean(alecsmall.imp4$inatt,na.rm=TRUE)
alecsmall.imp4$hyper[is.na(alecsmall.imp4$hyper)] <- 
  mean(alecsmall.imp4$hyper,na.rm=TRUE)
## For categorical variables, impute the baseline.
alecsmall.imp4$ETHNICITY[is.na(alecsmall.imp4$ETHNICITY)] <- fmode(alecsmall.imp4$ETHNICITY) #"White"
alecsmall.imp4$YEAR[is.na(alecsmall.imp4$YEAR)] <- 
  median(alecsmall.imp4$YEAR,na.rm=TRUE) # "junior"  # in middle of distribution
#alecsmall.imp4$FEMALE[is.na(alecsmall.imp4$FEMALE)] <- "female"


model4.pred <- predict(model4,alecsmall.imp4,se.fit=TRUE)
summary(model4.pred)
## Calculating the prediction se from the fit se and the residual se
model4.pred$se.pred <- sqrt(model4.pred$se.fit^2 + 
                              model4.pred$residual.scale^2)
  
alecsmall.imp4$genanxa <- 
  ifelse(is.na(alecsmall.imp4$genanxa),
         rnorm(nrow(alecsmall.imp4),model4.pred$fit,model4.pred$se.pred),
         alecsmall.imp4$genanxa)
## Analysis
tout4 <- t.test(genanxa ~ group, data=alecsmall.imp4, na.action=na.fail)
dout4 <- cohen.d(genanxa ~ group, data=alecsmall.imp4, na.action=na.fail)
## store results
result.tab[4,] <- c(meandiff=-diff(tout4$estimate),
                    se.diff=abs(diff(tout4$estimate))/tout4$statistic,
                    effsize=dout4$estimate,
                    se.effsize=sqrt(dout4$var) )




#############################################################
## Calculate bias, pbias and rmse
## Raw bias
result.bias <- sweep(result.tab,2,baseline,"-")
## Percentage bias
result.pbias <- 100*sweep(result.bias,2,baseline,"/")
## biases are in columns 1 and 3 of bias table.
## s.e.s are in columns 2 and 4 of result.tab
result.rmse <-sqrt(result.bias[,c(1,3)]^2+result.tab[,c(2,4)]^2 )

result.se.bias <- sweep(result.tab[,c(2,4)],2,baseline[c(2,4)],"/")
result.se.bias


## Getting the results out:
## Approach 1, write out to a tab separated file.
write.table(result.bias,"biastable.xls", sep="\t")

## Approach 2, use the xtable package.
library(xtable)
xtable(result.bias,digits=3)
## To get HTML print into a file
print(xtable(result.bias,digits=3),type="html",file="bias.html")
