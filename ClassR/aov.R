gender <- as.factor(c("male","female"))
treat <- as.factor(c("control","treat1","treat2"))
Reps <- 5
X <- rnorm(Reps*length(gender)*length(treat))
Y <- rnorm(Reps*length(gender)*length(treat))
aov.frame <- data.frame(Y=round(Y,digits=2),X=round(X,digits=2),
                        gender=rep(gender,length(treat),each=Reps),
                        treat=rep(treat,each=Reps*length(gender)))
model.matrix(Y ~ X*gender + gender*treat,data=aov.frame)


