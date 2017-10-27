#Session 7

# Regression Diagnostics

View(women)
attach(women)
plot(height,weight)

reg1 = lm(weight~height)
summary(reg1)
abline(reg1,col="blue")

library(MASS)
View(Boston)
reg2 = lm(medv~.-indus-age,data=Boston)
summary(reg2)

#Scenario 1 : Simple Regression
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
reg1 = lm(y~x)
summary(reg1)  
err=reg1$residuals
shapiro.test(err)

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

#Scenario 2: Heteroscedasticity 
x = runif(500,1,20) 
y = 100+2*x + x*rnorm(500) 
reg1 = lm(y~x)

summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) #plot shows that errors are not normally distributed. 
#There is heteroschedasticity in errors

#Scenario 3: Nonlinear 1 
x = runif(500,1,20) 
y = ifelse(x<15,100+2*x +rnorm(500),100+5*x+rnorm(500)) 
reg1 = lm(y~x)

summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 


#Scenario 4: Nonlinear 2 
x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
reg1 = lm(y~x)

summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Scenario 5: Interaction 
x1 = runif(500,1,20) 
x2 = runif(500,1,20) 
y = x1+4*x2+0.5*x1*x2 + rnorm(500) 
reg1 = lm(y~x1+x2)

summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Checking the Women data again taking clues from above scenarios
reg1=lm(weight~height)
summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Boston data 
reg1=lm(medv~.,data=Boston)
summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Scenario 6
#Extreme values (x)
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 
reg1 = lm(y~x)
summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Scenario 7
#Extreme values (y)
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
y[499] = 2000 
reg1 = lm(y~x)
summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Scenario 8: Multicollinearity 
#if x1 and x2 are highly correlated, the coefficients of regression become unreliable
x1 = runif(500,1,10) 
lambda = 0.9 
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
summary(reg1)  

opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

err=reg1$residuals
shapiro.test(err) 

#Formal Tests
library(MASS)
library(car)

x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
reg1 = lm(y~x)
ncvTest(reg1) #For homoscadesticity
#Null hypothesis for ncvtest is that errors are homoscadestic

x = runif(500,1,20) 
y = 100+2*x + x*rnorm(500) 
reg1 = lm(y~x)
ncvTest(reg1)

reg1=lm(medv~.,data=Boston)
ncvTest(reg1)
spreadLevelPlot(reg1)

#Cooks distance to find influential observation
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 
reg1 = lm(y~x)
z=cooks.distance(reg1)
round(z,4)
cutoff = 4/length(x)
z[z>cutoff]
plot(reg1,which = 4, cook.levels = cutoff)
abline(h=cutoff,col="red")


reg1=lm(medv~.,data=Boston)
z=cooks.distance(reg1)
round(z,4)
cutoff = 4/nrow(Boston)
z[z>cutoff]
plot(reg1,which = 4, cook.levels = cutoff)
abline(h=cutoff,col="red")


reg2=lm(medv~.,data=Boston[-c(369,373),])
z=cooks.distance(reg2)
round(z,4)
cutoff = 4/nrow(Boston)
z[z>cutoff]
plot(reg2,which = 4, cook.levels = cutoff)
abline(h=cutoff,col="red")

round(reg1$coefficients,4)
round(reg2$coefficients,4)
cbind(round(reg1$coefficients,4), round(reg2$coefficients,4))

#VIF - Multicollinearity

x1 = runif(500,1,10) 
lambda = 0.2 #change values of lambda and observe change in vif
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
vif(reg1) #keep vif less than 4 ideallys

reg2=lm(medv~.,data=Boston)
vif(reg2)
summary(reg2)


reg1 = lm(weight~height,data=women)
summary(reg1)
ncvTest(reg1)
z=cooks.distance(reg1)
round(z,4)
cutoff = 4/nrow(Boston)
z[z>cutoff]
plot(reg1,which = 4, cook.levels = cutoff)
abline(h=cutoff,col="red")
vif(reg1)


#Weighted Least Square Regression and Robust Regression
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 

reg1 = lm(y~x)
summary(reg1)  
z = cooks.distance(reg1)
cutoff = 4/length(x)
w = ifelse(z<cutoff, 1, cutoff/z)
reg2 = lm(y~x,weights = w)
summary(reg2)
reg3 = rlm(y~x) #Robust linear regression
summary(reg3)


reg1=lm(medv~.,data=Boston)
summary(reg1)
z = cooks.distance(reg1)
cutoff = 4/nrow(Boston)
w = ifelse(z<cutoff, 1, cutoff/z)
reg2 = lm(medv~.,data=Boston)
summary(reg2)
reg3 = rlm(medv~.,data=Boston) #Robust linear regression
summary(reg3)


x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
reg1 = lm(y~x)
summary(reg1)  
opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)
reg2 = lm(y~x+I(x^2))
summary(reg2)
anova(reg1,reg2) #evaluate both models using anova
AIC(reg1,reg2) #evaluate both models using AIC
opar = par() 
par(mfrow=c(2,2)) 
plot(reg2) 
par(opar)


reg1 = lm(weight~height,data=women)
summary(reg1)
opar = par() 
par(mfrow=c(2,2)) 
plot(reg1) 
par(opar)

reg2 = lm(weight~height+I(height^2),data=women)
summary(reg2)
anova(reg1,reg2) #evaluate both models using anova
AIC(reg1,reg2) #evaluate both models using AIC
opar = par() 
par(mfrow=c(2,2)) 
plot(reg2) 
par(opar)

reg3 = lm(weight~height+I(height^2)+I(height^3),data=women)
summary(reg3)
anova(reg1,reg2,reg3) #evaluate both models using anova
AIC(reg1,reg2,reg3) #evaluate both models using AIC
opar = par() 
par(mfrow=c(2,2)) 
plot(reg3) 
par(opar)

#If something in 'y' needs to be transformed, it is BoxCox!!!
#boxcox gives modification in LHS structure
reg1=lm(medv~.,data=Boston)
boxCox(reg1,family="yjPower",plotit = T)
reg2=lm(log(medv)~.,data=Boston)
summary(reg1)
summary(reg2)
#here we did not use anova or aic because the target is different here

reg1 = lm(weight~height,data=women)
boxCox(reg1,family="yjPower",plotit = T)
summary(reg1)


x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
reg1 = lm(y~x)
boxCox(reg1,family="yjPower",plotit = T)


#Box-tidwell only takes in consideration positive values
#Box-tidwell gives power to RHS
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
boxTidwell(y~x)


x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
boxTidwell(y~x)


reg1 = lm(weight~height,data=women)
reg2=lm(weight~height+I(height^4),data=women)
anova(reg1,reg2) #null hypothesis: errors coming from both regression are same
#we dont want the errors to be same
AIC(reg1,reg2)


boxTidwell(medv~age+dis+rad,data=Boston)


#step - suggests Interaction between variables

x1 = runif(500,1,20)
x2 = runif(500,1,20)
y= x1+4*x2+x1*x2+rnorm(500)

reg1 = lm(y~x1+x2)
res = step(reg1,~.^2)
res$anova

reg1 = lm(medv~.,data=Boston)
res = step(reg1,~.^2)
res$anova
