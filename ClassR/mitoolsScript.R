####
## This looks at the mitools pacakge
##
## installation
install.packages("mitools")
## load
library(mitools)

## List contents
help(package="mitools")

### Note existence of smi.pdf vingette.  The rest of this follows that.

#### Step 1.  Create an imputationList from external imputations.
### Note:  these are stored as stata files, so we need foreign library
### to load them.
library(foreign)
data.dir <- system.file("dta",package="mitools") #In mitools install
                                        #library
list.files(data.dir, pattern="..\\.dta",full=TRUE)
women <- imputationList(lapply(list.files(data.dir,
                                          pattern="f.\\.dta",full=TRUE),
                               read.dta, warn.missing.labels=FALSE))
men <- imputationList(lapply(list.files(data.dir,
                                        pattern="m.\\.dta",full=TRUE),
                             read.dta, warn.missing.labels=FALSE))


#### Step 2.  Manipulate the imputation lists

### update() can be used to add variables.
women <- update(women,sex=0)
men <- update(men,sex=1)
summary(men$imputations[[1]])

### rbind() can be used on two imputation lists to combine the
### observations
people <- rbind(women,men)

### Get info about the data frames
people
colnames(people)

#### Step 3.  Run repeated analyses using with.

### Two cases:  first is ordinary function.  We need to give fun=
### argument to with
with(people,fun=summary)

### Second case, if the expression is one that take a "data" argument,
### we can just give the expression.  This particularly allows model
### fitting.

with(people, table(sex,drkfre))

### Add a regular drinker variable
people <- update(people,drkreg=as.numeric(drkfre)>2)
with(people, table(sex,drkreg))

### Now an example using plot
par(mfrow=c(3,2))                       #Need at least 5 plots on a
                                        #page
with(people,plot(cistot~drkfre))

#### Step 4.  Fit the model of interest.
model1 <- with(people, glm(drkreg ~wave*sex, family=binomial()))
model1                                  #Runs model 5 times


#### Step 5.  Combine the results.
MIcombine(model1)
summary(MIcombine(model1))

#### Step 6.  Extract coefficients of interest
### fun argument is the function that grabs the coefficient of
### interest.

beta <- MIextract(model1,fun=coef)
vars <- MIextract(model1,fun=vcov)
### Each returns a list of results
beta
vars
## Can put them back together again.
MIcombine(beta,vars)

##Can use it to extract other things.
AICs <- MIextract(model1,fun=AIC)
AICs
## This is a list of values, so flatten it
AICs <- do.call("c",AICs)               #R dirty trick to flatten
                                        #lists
AICs
mean(AICs)
sd(AICs)
