## Analysis of Prevatt data

## setwd("C:/Users/ralmond/ownCloud/classes/MissingData/PrevattData")

library("foreign")
alec <- read.spss("AlecMay2015-deidentified.sav")
alec <- as.data.frame(alec)
## Get some summaries
summary(alec)

## Drop item specific data
interesting.cols <- c(1:34,164:180,190,202:222,263,470:509,549,572:575,656:663,753:782)
alec.sub <- alec[,interesting.cols]
summary(alec.sub)

## Check to make sure 0's in SAT/ACT are people not taking the test
alec.sub[which(alec.sub$SATRTTOT==0),1:25]
alec.sub[which(alec.sub$ACTSCOREREAD==0),1:25]
## Change them to NAs
is.na(alec.sub[which(alec.sub$SATRTTOT==0),11:15]) <- TRUE
is.na(alec.sub[which(alec.sub$ACTSCOREREAD==0),17:21])<- TRUE

## Look at some cross tabs
table(alec.sub$group,alec.sub$YEAR)

## Do some plots
plot(ANXIETY~group,data=alec.sub)
plot(GADD~group,data=alec.sub)
plot(paa~group,data=alec.sub)
plot(hyper~group,data=alec.sub)
table(is.na(alec.sub$hyper),alec.sub$group)
plot(GADD~hyper,data=alec.sub)
cor(na.omit(alec.sub[,c("GADD","hyper")]))
## Simpler way to get rid of NAs
cor(alec.sub[,147:157],use="complete.obs")
## How to do pairwise removal:
cor(alec.sub[,147:157],use="pairwise.complete.obs")

## Jitter points to get look at multiple plots:
plot(jitter(alec.sub$hyper,1),jitter(alec.sub$GADD,1),
     xlab="Hyperactivity",ylab="General Anxiety")
lines(lowess(alec.sub$hyper,alec.sub$GADD))
