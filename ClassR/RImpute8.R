#Question 8 Perform an unconditional Hot Deck imputation

install.packages("HotDeckImputation")

library(HotDeckImputation)

set.seed(421)

#Generate matrix of random integers

Y<-matrix(sample(0:100,replace=TRUE,size=100*5),nrow=100)
YY <- Y
YY[,c(1,2)] <- matrix(sample(0:5,replace=TRUE,size=100*2),nrow=100)
#generate missing values except first two columns

#first two columns are used to create imputation classes

Y[,-c(1,2)][sample(1:length(Y[,-c(1,2)]),size=floor(0.15*length(Y[,-c(1,2)])))]<-NA

#impute missing data by CPS sequential hot-deck imputation

Y.CPS_SEQ <- impute.CPS_SEQ_HD(DATA=Y,covariates=c(1,2),initialvalues=-1, navalues=NA, modifyinplace = FALSE)
Y.SEQ <- impute.SEQ_HD(DATA=Y,initialvalues=-1, navalues=NA, modifyinplace = FALSE)
Y.NN <- impute.NN_HD(DATA=Y)
YY.CPS_SEQ <- impute.CPS_SEQ_HD(DATA=YY,covariates=c(1,2),initialvalues=-1, navalues=NA, modifyinplace = FALSE)
#other hot deck imputation

#impute.mean replacing by column mean of complete cases

#impute.NN_HD nearest neighbor hot deck

#impute.SEQ_HD replacing by most recently observed value in varivable
##9
library(Hmisc)     # load package  
mydata <- spss.get("R_school.sav", use.value.labels=TRUE)
mydata
## Locate missing data,
mpr<- unique (unlist (lapply (mydata, function (mydata) which (is.na (mydata)))))   # finding all the rows in a data frame with at least one NA

mpr   # Show the row number with missing value, 

is.na(mydata[sample.int(nrow(mydata),50),"tchinfl"]) <- TRUE

head (mydata$g10ctrl1)   # show the value of  g10ctrl1 which is the common chracteristics we are looking for to replace the missing value.  g10ctrl1 =1, 2, 3, or  4 .  
summary(mydata$g10ctrl1)
summary(mydata$teachmor)
##I find rows with missing data only have  g10ctrl1 =1, 2, 3, or  4 .


## sample data, newdata 1 only include data where g10ctrl1=1, 


#newdata1 <- mydata[mydata$ g10ctrl1 =='1' ], 
newdata <- mydata
for (t in unique(mydata$g10ctrl1)) {
  subdata <- mydata[mydata$g10ctrl1==t,]
  ## Sample with replacement from the data in the same category
  imp <- sample(na.omit(subdata$tchinfl),nrow(subdata),
                replace=TRUE)
  subdata$tchinfl <- ifelse(is.na(subdata$tchinfl),imp,
                            subdata$tchinfl)
  ## put subdata back into imputed data set.
  newdata[mydata$g10ctrl1==t,] <- subdata
}


## replace the missing value with the column means for new data where g10ctrl1=1, 



newdata1.imp<-newdata1, 



for  (i in 1:ncol (newdata1.imp))
  
  
  
{ if (anyNA (newdata1.imp [,i]))
  
  
  
{ newdata1 [is.na(newdata1 [, i]), i ]) <-mean ( newdata1[, i], na.rm = T )}
  
  
  
}







## sample data,  newdata 2  only include data where  g10ctrl1 =2, 



newdata2 <- mydata[ which(mydata$ g10ctrl1 =='2' )],  



## replace the missing value with the column means for new data where g10ctrl1=2, 



newdata2.imp<-newdata,



for  (i in 1:ncol (newdata2.imp))
  
  
  
{ if (anyNA (newdata2.imp [,i]))
  
  
  
{ newdata2 [is.na(newdata2 [, i]), i ]) <-mean ( newdata2[, i], na.rm = T )}
  
  
  
}





## sample data,  newdata 4  only include data where  g10ctrl1 =4, 



newdata4 <- mydata[ which(mydata$ g10ctrl1 =='4' )],  



## replace the missing value with the column means for new data where g10ctrl1=2, 



newdata4.imp<-newdata4



for  (i in 1:ncol (newdata4.imp))
  
  
  
{ if (anyNA (newdata4.imp [,i]))
  
  
  
{ newdata4 [is.na(newdata4 [, i]), i ]) <-mean ( newdata4[, i], na.rm = T )}
  
  
  
}



