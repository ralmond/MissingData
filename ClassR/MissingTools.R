
###############################
### Missing Data Tools
###############################
### Description:  Various code for dealing with 
### missing data
### Authors: Missing Data Class, R Group, Umit Tokac, Russell Almond
### Maintainer:  Russell Almond <ralmond@fsu.edu>
### Version: 0.1 
### Last Modified Date: 2016-02-04
### Change History:
### Date -- initials -- description
### 2016-02-01 UT Converted from Homework answers
### 2016-02-04 RGA Changed into functions


#################
### @name: countNA.data.frame
### @description: Counts NAs in a data frame
### @inputs:
### @arg x -- a data frame with potentially missing values
### @output: a numeric vector of length equal to the number of columns in x,
### each entry gives the number of missing values in that column
### @sideEffects: None


"countNA.data.frame" <- function (x) {
  sapply(x, function(y) {
    sum(is.na(y))
    })
}



#################
### @name: countNA.matrix
### @description: Counts NAs in a matrix by column
### @inputs:
### @arg x -- a mantri with potentially missing values
### @output: a numeric vector of length equal to the number of columns in x,
### each entry gives the number of missing values in that column
### @sideEffects: None


"countNA.matrix" <- function (x) {
  colSums(is.na(x))
}


#################
### @name: countNA
### @description: Counts NAs in a matrix like object
### @inputs:
### @arg x -- a data frame or matrix with potentially missing values
### @output: a numeric vector of length equal to the number of columns in x,
### each entry gives the number of missing values in that column
### @sideEffects: None
"countNA" <- function(x) {
  UseMethod("countNA")
}

### Fall back if not a matrix of a data frame
### should work reasonably well for vectors 
### and fail gracefully(?) for other types of objects
"countNA.default" <- function (x) {
  sum(is.na(x))
}


#### Test Function
df <- data.frame(x=1:50, y=rnorm(50),z=runif(50)<.5) # creating a data.frame
is.na(df[c(10,20,30),1]) <- TRUE                     # adding missing cases in first col
is.na(df[c(11,22,33),2]) <- TRUE                     # adding missing cases in second col
stopifnot(all(c(3,3,0)==countNA(df)))
mydata <- as.matrix(df)                              # converting the data.frame to data matrix
stopifnot(all(c(3,3,0)==countNA(mydata)))
stopifnot(all(6==countNA(as.vector(mydata))))

