### Data cleaning for Alec data
library(foreign)
Alec.all <- read.spss("AlecMay2015-deidentified.sav")
summary(Alec.all)
summary(Alec.all$GENDER)
