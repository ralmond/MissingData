## Median method for ordered factors.
## Follows the same syntax as median.
## Adding this to your workspace will cause median() to work on ordered
## variables.
median.ordered <- function (x, na.rm = FALSE) {
  result <- round(median(as.numeric(x),na.rm=na.rm))
  ordered(levels(x)[result],levels=levels(x))
}

### This finds the mode of a numeric or factor variable.
## If the data set has multiple modes, then the result will be a vector
## of length equal to the number of modes.
## The exclude and useNA arguments are passed to table, and are described
## there.
## Legal values for useNA are "no", "ifany" and "always".
fmode <- function (x, exclude=ifelse(useNA=="no",c(NA,NaN),NULL),
                   useNA=NULL) {
  ftab <- table(x,exclude=exclude,useNA=useNA)
  md <- ftab[which(ftab==max(ftab))]
  if (is.numeric(x)) {
    md <- as.numeric(names(md))
  }
  if (is.factor(x)) {
    if (is.ordered(x)) {
      md <- ordered(names(md),levels(x))
    } else {
      md <- factor(names(md),levels(x))
    }
  }
  md
}

