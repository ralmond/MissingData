####
## Demonstration of the mi package.
library(mi)

#### Set options for parallel processing.
### My computer has 8 cores.
### 1 for the system
### 1 for powerpoint
### 1 for tegrity
### 5 left for MCMC
options(mc.cores=5)
#### By default uses parallel for *nix and not parallel for Windows)

#### Load in the sample data
data(nlsyV,package="mi")
?nlsyV

#### Convert to a missing_data.frame
mdf <- missing_data.frame(nlsyV)

#### Check on the guesses.
show(mdf)

#### Oops.  It guess race is categorical.  Better fix that
?missing_variable ## Table of legal types
mdf <- change(mdf, "income",what="type",to="non")

show(mdf)
mdf <- change(mdf, "momrace",what="type",to="un")
show(mdf)
### Can change multiple variables at once by using vectors for y and
### to arguments.

##### View missingness pattern
image(mdf)

#### Look at summaries of the data:
summary(mdf)
hist(mdf)


#### Okay do the imputations
rm(nlsyV) # not needed any more, save memory
imputations <- mi(mdf, n.iter=20, n.chains=5, max.minutes=20)
show(imputations)


#### Check for convergence
### All the means should be about the same.
round(mipply(imputations,mean,to.matrix=TRUE),3)
## Gelman Rubin R's -- key value is less that 1.1
Rhats(imputations)

#### Extend another 5 imputations
imputations <- mi(imputations, n.iter=5)
show(imputations)
Rhats(imputations)

#### Look at what we have
hist(imputations) # Per chain histograms

plot(imputations) # Plots histogram, expected vs resid, and binned
                                        # residuals

image(imputations) # Same as before
summary(imputations) # Compares observed and imputed values


#### The pool function does the analysis
analysis <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed +
                     momrace,
                 data = imputations, m = 5)
## Note:  pool will figure out appropriate analysis type based on
## class of ppvtr.36

display(analysis)

## Save completed data sets
dfs <- complete(imputations, m=5)
## Put into the mitools world
library(mitools)
nlsyV.imps <- imputationList(dfs)

## Write out in one big file:
mi2stata(imputations,5,"nlsyV.imps.csv",missing.ind=TRUE)

