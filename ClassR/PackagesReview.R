# For a detailed example, see the file "mlmmmmex.s" distributed
# with this function. Here is a simple example of how mlmmm.em()
# might be used to produce Ml estimates.
# don’t forget to install it
library(mlmmm)
data(adg)
y<-cbind(adg$y.1,adg$y.2)
colnames(y)=c("adg","initwt")
subj=adg$subj
# see the relationship between avd and intwt which are jointly modeled
library(lattice)
xyplot(y[,1]~log(y[,2]) | subj, ylab="Average Daily Gain",xlab="Initial Weight")
# below adg$subj is the block or barn
subj<-adg$subj
pred <- cbind(adg$pred.int,adg$pred.dummy1,adg$pred.dummy2,adg$pred.dummy3)
xcol<-1:4
zcol<-1
unst.psi.result <- mlmmm.em(y,subj,pred,xcol,zcol,maxits=200,eps=0.0001)
unst.psi.result
summary(unst.psi.result)

library(mice)
nhanes

##Step 1
#Creating imputations
imp0 <- mice(nhanes, method = "norm", seed=23109) #default m=5
print(imp0)
complete(imp0)

imp1 <- mice(nhanes, seed=23109) #default m=5 and default method pmm
complete(imp1)

imp2 <- mice(nhanes, m=50, seed=23109)
complete(imp2)

##Step 2
#Analysis of imputed data
fit0 <- with(imp0, lm(chl ~ age + bmi))
fit1 <- with(imp1, lm(chl ~ age + bmi))
fit2 <- with(imp2, lm(chl ~ age + bmi))
#fit0 and fit1 contain the results of 5 complete-data analyses, 50 for fit2

##Step 3
#Pool the results
print(pool(fit0))
round(summary(pool(fit0)),2)

print(pool(fit1))
round(summary(pool(fit1)),2)

print(pool(fit2))
round(summary(pool(fit2)),2)

#pooled results for all data analyses
#where lambda is the proportion of total variance contributed by the missing values
#and fmi (fraction of missing info) due to nonresponse suggests how many imputations should be used (Rubin)

#univariate imputation methods
complete(imp0) #creates the imputed data set
#the method argument specifies the imputation method per column 
imp1 <- mice(nhanes, meth = c("", "norm", "pmm", "mean")) #col-wise specification
#no method for age, Bayesian linear regression for bmi, predictive mean matching for hyp, unconditional mean for chl
#automatically skips over complete data
complete(imp1)
cbind(complete(imp0),complete(imp1),nhanes)

#predictorMatrix function
imp <- mice(nhanes, method = "norm", print=FALSE)
pred <- imp$predictorMatrix
#age is complete and doesn't have any predictors
pred[,"bmi"]<-0
pred
imp3 <- mice(nhanes, pred=pred, method= "norm", pri=FALSE)
complete(imp3)
cbind(complete(imp3),nhanes)

#The predictorMatrix specifies the set of predictors to be used for each target column. 
#It also allows the user to manipulate which predictors will be used. 
#Notice that BMI is still imputed, but it is not used as a predictor.  


library("MissMech")
n<-300
p<-5
pctmiss<-0.2
set.seed(1010)
y<-matrix(rnorm(n*p), nrow=n)
missing<-matrix(runif(n*p),nrow=n)<pctmiss
y[missing]<-NA
out<-TestMCARNormality(data=y)
out

n <- 300
p <- 5 
pctmiss <- 0.2 
set.seed(1010) 
y <- matrix(rt(n * p, 5), nrow = n) 
missing <- matrix(runif(n * p), nrow = n) < pctmiss 
y[missing] <- NA 
out <- TestMCARNormality(data = y) 
out 
