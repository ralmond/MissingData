## Class missing Scripts

df <- data.frame(x=1:50, y=rnorm(50),z=runif(50)<.5)
is.na(df[c(10,20,30),1]) <- TRUE
is.na(df[c(11,22,33),2]) <- TRUE
mydata <- as.matrix(df)

### Problem 1 Zhongrui
#Count the number of missing values in each column of a data matrix and data.frame.
#For a data frame:   sum(is.na(df$col))

## Data frame
na.count <-sapply(df, function(y) sum(is.na(y)))

sapply(df$col, function(x) sum(length(which(is.na(x)))))

## For a data matrix:   
colSums(is.na(mydata))
apply(mydata,2,function(col) sum(is.na(col)))

### Problem 3
Math1 = matrix(sample(50:100,20,1))
Math1[5:8,1]<- NA
Math3 = matrix(sample(50:100,20,1))
Math3[5:8,1] <- NA 
Math3[11:14,1] <- NA
Math2 = matrix(sample(50:100,20,1))
Math2[5:8,1] <- NA 
Math2[11:14,1] <- NA
Math2[16:19,1] <- NA
Pdat <- cbind(Math1,Math2,Math3)
library(cat)
Pdat1<- prelim.cat(Pdat)                               ##This function performs grouping and 
##sorting operations on categorical datasets with missing values.
##It creates a list that is needed for input to em.cat, da.cat, imp.cat, etc. 
Pdat1$x                                                       ##push the common NAs downwards
Pdat1$nmis                                                  ## provide number of missing data
sort(Pdat1$nmis, index.return = TRUE)           ## provide decreasing order and monotone pattern
Pdat1$x[,c(1,3,2)]     
# problem 6
#R_puzzle #6

#Randomly sample 25% of one variable and make the values missing.

#Generate sample

sample <- rnorm(1000,mean=0.5,sd=2)

#check data in sample

summary(sample)

sd(sample)



#randomly select 250 integers in 1:1000

num<-sample.int(1000,size=250,replace=FALSE)

#replace sample[num] by NA

#for (i in num){
#  sample[i] <- NA 
#}
sample[num] <- NA
#sample[num, ] <- NA  ## Samples rows of matrix/data frame

#check missing data

#is.na(sample) check the positions of missing data



#The percentage of missing data

percentMiss<-sum(is.na(sample))/length(sample)*100
HW7 <- data.frame(x=1:50, y=rnorm (50), z=runif(50))
HW7
HW7$Missing <- rbinom(50,1,prob=HW7$z)
HW7

HW7m <- HW7
HW7m[as.logical(HW7$Missing),1:3] <- NA


## Qsn 8
library(dplyr)

Survey <- matrix(sample(1:5,160,replace = T),10,16)

head(Survey)

Survey2 <- as.data.frame(Survey)
Survey2$Topic1 <- rowSums(Survey2[,1:10])
Survey2$Topic2 <- rowSums(Survey2[,11:16])
head(Survey2)

mutate(Survey2, Topic1 = V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, Topic2 = V11+V12+V13+V14+V15+V16)

# 9
t1<-data.frame(replicate(10,sample(c(1:5,NA),100,rep=TRUE)))
t2<-data.frame(replicate(6,sample(c(1:5,NA),100,rep=TRUE)))
head(t1)
head(t2)
# 9.A
#Do not count missing values in the numerator or denominator

X<-rowMeans(t1)  # does not work, due to NAs
head(X)
a1<-rowMeans(t1,na.rm=TRUE) # exclude NAs
a2<-rowMeans(t2,na.rm=TRUE) # exclude NAs
head(a1)
head(a2)
s<-cbind(t1,t2,a1,a2)
head(s)

# 9.B
# Count missing values as 0
t1[is.na(t1)]<-0
t2[is.na(t2)]<-0
head(s)
f1<-rowMeans(t1)
ff1 <- rowSums(t1,na.rm=TRUE)/ncol(t1)
f2<-rowMeans(t2)
head(f1)
head(f2)



## Problem 11 The bootstrap

## The idea is to make a new data set by replicating the old one.
Niter <- 10
StatVals <- matrix(NA,Niter,ncol(df)) # Hold the results
for (Iiter in 1:Niter) {
  samp <- sample(nrow(df),nrow(df), replace=TRUE) # Sample with replacement
  StatVals[Iiter,] <- colMeans(df[samp,],na.rm=TRUE) # Calculate my statistic  
} #Next data set

## Problem 12 The jackknife
jkVals <- matrix(NA,nrow(df),ncol(df)) # Hold the results
for (Iiter in 1:nrow(df)) {
  jkVals[Iiter,] <- colMeans(df[-Iiter,],na.rm=TRUE) # Calculate my statistic  
} #Next row left out


#R Puzzle #10

#Randomly select 10% of the cases, separate data set into test set (10%) and training set (remaining 90%)

#First, I created the data matrix and named it. 


ten <- matrix(1:100,nrow=10)

#Next, I tried to create the 2 independent groups from the data, one with 10% and the other with 90%

samp <--sample(1:nrow(ten), .1*nrow(ten), replace= TRUE, prob = c(0.1,0.9))

test.data <--ten [samp,]

rest.data <--ten [-samp,]