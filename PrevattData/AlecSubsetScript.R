## Work with Alec subset
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
