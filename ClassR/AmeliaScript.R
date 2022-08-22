####
## This looks at the mitools pacakge
##
## installation
install.packages("Amelia")
## load
library(Amelia)

### Start the standaloe UI.
AmeliaView()

#### Test data set is called freetrade
data(freetrade)
summary(freetrade)

summary(lm(tariff ~ polity + pop +gdp.pc + year + country,
           data=freetrade))


### Simple version
### "year" is a time series, time variable
### "country" is a cross-sectional variable
### These need special declaration.

a.out.1 <- amelia(freetrade, m=5, ts="year", cs="country")
summary(a.out.1)
### Map of the missing data pattern
missmap(a.out.1)

## With more diagnostics
amelia(freetrade,m=2,ts="year",cs="country",p2s=2)
## p2s=0 is quiet

#### Adding more imputaitons
a.out.1a <- amelia(freetrade, m=5, ts="year", cs="country")
a.out.1b <- ameliabind(a.out.1,a.out.1a)
summary(a.out.1b)

#### Post-imputation transformations
a.out.1b <- transform(a.out.1b,lgdp=log(gdp.pc))
a.out.1b$imputations[[6]][1:6,c("country","year","gdp.pc","lgdp")]




#### Capturing the output for later analysis
library(mitools)
freetrade.imp <- imputationList(a.out.1b$imputations)

par(mfrow=c(5,2))
with(freetrade.imp,hist(tariff))

#### Saving the imputations.

### Save as an R object
save(a.out.1b, file="Ameila.out.Rdata")
##load("Amelia.out.Rdata")   ## This reads it back in.

### As a bunch of csv files
write.amelia(a.out.1a,file.stem="am.out",format="csv")
### Legal choices for format:
## csv -- comma separated value
## table -- tab separated value
## dta -- stata

######
#### Declaring variable types.
### Polity is a 20 point scale running from -10 (full autocracy) to 10
### (full democracy)  
sort(unique(a.out.1$imputations[[3]]$polity))
table(a.out.1$imputations[[3]]$polity)

### Signed is whether or not country signed the IMF treaty
sort(unique(a.out.1$imputations[[3]]$signed))
table(a.out.1$imputations[[3]]$signed)

### pop and gdp.pc are likely to be postively skewed
par(mfrow=c(2,2))
hist(freetrade$tariff)
hist(freetrade$pop)
hist(freetrade$gdp.pc)

par(mfrow=c(2,2)) # Restart plotting at 1,1
hist(freetrade$tariff)
hist(sqrt(freetrade$tariff))
hist(log(freetrade$tariff))

par(mfrow=c(2,2)) # Restart plotting at 1,1
hist(freetrade$pop)
hist(sqrt(freetrade$pop))
hist(log(freetrade$pop))


par(mfrow=c(2,2)) # Restart plotting at 1,1
hist(freetrade$gdp.pc)
hist(sqrt(freetrade$gdp.pc))
hist(log(freetrade$gdp.pc))

#### Mark these transformations in the imputation model.
a.out.2 <- amelia(freetrade, m=5, ts="year", cs="country",
                  ords="polity",noms="signed",
                  logs=c("tariff","pop","gdp.pc"),p2s=0)


table(a.out.2$imputations[[3]]$polity)
table(a.out.2$imputations[[3]]$signed)

#### Diagnostic plots
#### Simply plotting will plot imputed vs not imputed variables.
plot(a.out.2)


### compare.density is the work horse command
par(mfrow=c(2,1))
compare.density(a.out.1,var="tariff",main="Without logs",col=c(4,1))
compare.density(a.out.2,var="tariff",main="With logs",col=c(4,1))

#### Overimputation diagnostic
par(mfrow=c(1,1))

overimpute(a.out.1,var="tariff",lty=1)
overimpute(a.out.1,var="fiveop")

##### EM Convergence diagnostics
### Start with overdisperse starting points and see if it converges
### Note M is number of EM chains, not number of imputations.
disperse(a.out.2,dims=1,m=5)
disperse(a.out.2,dims=2,m=5)


#######
#### Doing the actual analysis.

freetrade.imp2 <- imputationList(a.out.2$imputations)

#### Model 1
freetrade.mod1 <- with(freetrade.imp2,
                       lm(tariff ~ polity + pop + gdp.pc + year +
                              country))
MIcombine(freetrade.mod1)
summary(MIcombine(freetrade.mod1))
