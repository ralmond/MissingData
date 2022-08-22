setwd("E:/Project/NewData2")

library(MissingDataGUI)

if (interactive()) {
  MissingDataGUI()
}


prj.mdata <- read.table(file="harvest_new.txt", header=TRUE, sep="\t")

MissingDataGUI(data = prj.mdata)

# erkan vim package

install.packages("VIM")
install.packages("VIMGUI")

library(VIM)
library(VIMGUI)
vmGUImenu()


data(tao)
summary(tao)
x<-aggr(tao)  # Aggregates missing data
x

y <- tao[,c("Air.Temp", "Humidity")]
summary(y)
histMiss(y)


marginplot(tao[,c("Air.Temp", "Humidity")])  ## creates a plot matrix with information about missing values
matrixplot(tao[,c("Air.Temp", "Humidity")])  ## creates a plot matrix with information about missing values

summary(tao)
imputed.tao <-irmi(tao)
summary(imputed.tao)



# sleep data

install.packages("VIM")
install.packages("VIMGUI")

library(VIM)
library(VIMGUI)
vmGUImenu()

data(sleep)
names(sleep)
a <- aggr(sleep) # Aggregates missing data
a

x<-sleep[,c("Exp", "Sleep")]
summary(x)
barMiss(x) #displays bar charts
histMiss(x)

z <- sleep[, 1:5]
z[,c(1,2,3)] <- log10(z[,c(1,2,3)])
summary(z)
summary(sleep)
marginmatrix(z) # creates a scatter plot matrix with information about missing values

b <- sleep[, -(8:10)]
b[,c(1,2,4,6,7)] <-log10(b[,c(1,2,4,6,7)])
matrixplot(b, sortby = "BrainWgt")

#Multiple Imputation
summary(sleep)
imputed.sleep<-irmi(sleep)
summary(imputed.sleep)
