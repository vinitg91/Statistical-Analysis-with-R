#Variable Selection

library(MASS)
library(car)

reg1 = lm(medv~.,data=Boston)
res = step(reg1,~.^2)
res$anova

reg2 = lm(medv~.+rm:lstat+rad:lstat,data=Boston)
summary(reg2)

stepAIC(reg2,direction = "backward")

library(leaps)

reg3 = regsubsets(medv~.+rm:lstat+rad:lstat,data=Boston)
summary(reg3)

reg3 = regsubsets(medv~.+rm:lstat+rad:lstat,data=Boston,nvmax = 15)
summary(reg3)
summary(reg3)$bic
which.min(summary(reg3)$bic)
