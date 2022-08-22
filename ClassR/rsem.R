install.packages("rsem")
library(rsem)
library(lavaan)

# check missing data pattern
dset<-read.table('MardiaMV25.txt',header=T) 
dset1<-data.matrix(dset)
summary(dset)
n<-dim(dset1)[1]
p<-dim(dset1)[2]
miss_pattern<-rsem.pattern(dset1)
miss_pattern

#Robust mean and covariance matrix
em_results<-rsem.emmusig(miss_pattern, varphi=.1, max.it=1000)
#em_results
#compare the original mean and covariance
summary(dset)
cov(dset,use="pairwise.complete.obs")
cov(dset,use="na.or.complete")

#Sanwich-type covariance matrix
mu<-em_results$mu
sigma<-em_results$sigma
rsem.Ascov(miss_pattern,em_results)

#CFA analysis
names(dset)<-paste('V', 1:5, sep='')
fa.model<-'f1 =~ V1 + V2
f2 =~ V4 + V5
f1 ~ 1
f2 ~ 1
V1 ~0*1
V2 ~0*1
V4 ~0*1
V5 ~0*1'
analysis<-rsem.lavaan(dset, fa.model)
#compare to original CFA
fit0<-cfa(fa.model,dset)
summary(fit0)
