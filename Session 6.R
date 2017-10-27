#27 Mar, 2017

Corn <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/Corn.csv")
attach(Corn)
plot(nitrate,yield)

#Regression estimation
f1=function(a0,a1,sig)
{
  err = yield-a0-a1*nitrate
  LLsum = sum(dnorm(err,mean = 0,sd = sig,log = TRUE))
  return(-1*LLsum)
}

a0s=mean(yield)
a1s=0
sigs=sd(yield)
f1(a0s,a1s,sigs)

res = mle2(minuslogl = f1,start = list(a0=a0s,a1=a1s,sig=sigs),
           method="L-BFGS-B",lower=c(sig=0))
summary(res) #regression model

lm(yield~nitrate) #our res matches with the linear model built by 'lm'


#Binary - Logistic Regression
admit <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/admit.csv")

f1=function(a0,a1,a2,a3)
{
  x=a0+a1*admit$gre+a2*admit$gpa+a3*admit$rank
  p=exp(x)/(1+exp(x))
  L = ifelse(admit$admit==0,1-p,p)
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start=list(a0=0,a1=0,a2=0,a3=0))
summary(res)

#Binary - Probit Regression
admit <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/admit.csv")

f1=function(a0,a1,a2,a3)
{
  x=a0+a1*admit$gre+a2*admit$gpa+a3*admit$rank
  p=pnorm(x,mean=0,sd=1)
  L = ifelse(admit$admit==0,1-p,p)
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start=list(a0=0,a1=0,a2=0,a3=0))
summary(res)

# Poisson Regression

student <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/student.csv")

f1 = function(a0,a1,a2,a3)
{
  X = a0+a1*student$gender+a2*student$math+a3*student$prog
  l = exp(X)
  LLsum = sum(dpois(student$daysabs,lambda = l,log = TRUE))
  return(-1*LLsum)
}

f1(0,0,0,0)

res = mle2(minuslogl = f1,start=list(a0=0,a1=0,a2=0,a3=0))
summary(res)

# heteroschedasticity
head(cars)

plot(cars$speed,cars$dist)

f1 = function(a1,s)
{
  err = cars$dist-a1*cars$speed
  LLsum = sum(dnorm(err, mean = 0, sd = s, log=TRUE))
  return(-1*LLsum)
}

f1(0,sd(cars$dist))

res = mle2(minuslogl = f1,start = list(a1=0,s=sd(cars$dist)))
summary(res)

f1 = function(a1,s1,s2)
{
  err = cars$dist-a1*cars$speed
  L = ifelse(cars$speed<15,dnorm(err,mean = 0,sd = s1),dnorm(err,mean = 0,sd = s2))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

f1(0,sd(cars$dist),sd(cars$dist))
res = mle2(minuslogl = f1,start = list(a1=0,s1=sd(cars$dist),s2=sd(cars$dist)))
summary(res)

#assume that cut off is not 15 here. So keep it as another parameter and estimate it too!

# Forecasting

timeseries <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 6/Data/timeseries.csv")
z = timeseries$x

plot(z,type="l")

#try the usual likelihood approach on time series
f1 = function(m,s)
{
  err = z-m
  LLsum = sum(dnorm(x = err,mean = 0,sd = s,log = TRUE))
  return(-1*LLsum)
}
f1(mean(z),sd(z))
res = mle2(minuslogl = f1,start = list(m=mean(z),s=sd(z)) )
summary(res)


#Z(t) = mean + rho(Z(t-1)-mean) + err
f1 = function(m,rho,s)
{
  err = z[2:10000]-m-rho*(z[1:9999]-m)
  LLsum = sum(dnorm(x = err,mean = 0,sd = s,log = TRUE))
  return(-1*LLsum)
}
f1(mean(z),0,sd(z))

res = mle2(minuslogl = f1,start = list(m=mean(z),rho=0,s=sd(z)) )
summary(res)


# Two stage model
clickbuy <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 6/Data/clickbuy.csv")

f1 = function(a0,a1,a2,b0,b1,b2)
{
  Xc = a0+a1*clickbuy$gender+a2*clickbuy$age
  Xb = b0+b1*clickbuy$gender+b2*clickbuy$income
  p1 = exp(Xc)/(1+exp(Xc))
  p2 = exp(Xb)/(1+exp(Xb))
  L = ifelse(clickbuy$click==0,1-p1,ifelse(clickbuy$buy==0,p1*(1-p2),p1*p2))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

f1(0,0,0,0,0,0)

res = mle2(minuslogl = f1,start = list(a0=0,a1=0,a2=0,b0=0,b1=0,b2=0))
summary(res)


f1 = function(b0,b1,b2)
{
 
  Xb = b0+b1*clickbuy$gender+b2*clickbuy$income
  p2 = exp(Xb)/(1+exp(Xb))
  L = ifelse(clickbuy$buy==0,(1-p2),p2)
  LLsum = sum(log(L))
  return(-1*LLsum)
}

f1(0,0,0)

res = mle2(minuslogl = f1,start = list(b0=0,b1=0,b2=0))
summary(res)


# Censored Data and Survival Models

#Survival model
#-How much time will elapse till the event occurs? 
#-How much time will the thing survive
#Survival usually follows exponential pattern

aml <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 6/Data/aml.csv")

x = rexp(n = 1000,rate = 1/15) #rate is 1/average
plot(density(x))
mean(x)

f1 = function(l)
{
  LLsum = sum(dexp(x = aml$time,rate = 1/l,log = TRUE))
  return(-1*LLsum)
}
f1(mean(aml$time))

res = mle2(minuslogl = f1,start = list(l=10))
summary(res)

# Censored
f1 = function(l)
{
  L = ifelse(aml$status==0, 1-pexp(aml$time,1/l),dexp(aml$time,1/l))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(l=5))
summary(res)

# Survival model
as.numeric(aml$x)
mt = ifelse(aml$x=="Maintained",1,0)
f1 = function(a0,a1)
{
  x=a0+a1*mt
  l = exp(x)
  L = ifelse(aml$status==0, 1-pexp(aml$time,1/l),dexp(aml$time,1/l))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(a0=5,a1=0))
summary(res)

#exp(a1) = 2.6
#Thus it means that if a machine is maintained, 
#it will last 2.6 times longer than when it is not maintained

x=10
A = dexp(x,1/15)
B = 1-pexp(x,1/15)
A/B #Hazard
# Exp distribution has a constant hazard rate
# Hazard is the rate of exponential distribution
# The probability that the person will die at time t is hazard

lung <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 6/Data/lung.csv")

f1 = function(a0,a1,a2,a3)
{
  l = exp(a0+a1*lung$age+a2*lung$sex+a3*lung$ph.karno)
  L = ifelse(lung$status==1, dexp(lung$time,1/l),1-pexp(lung$time,1/l))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(a0=log(mean(lung$time)),a1=0,a2=0,a3=0))
summary(res)
#x = a0+a1*age+a2*sex+a3*ph.karno
#lambda = exp(x)
#x=a0 for baseline
#thus lambda = exp(a0) => a0 = log(lambda) => a0 = log(mean(lung$time))

# Seemingly Unrelated Regression

hsb2 <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 6/Data/hsb2.csv")

reg1 = lm(hsb2$read~as.numeric(hsb2$female)+as.numeric(hsb2$ses)+hsb2$socst)
reg2 = lm(hsb2$math~as.numeric(hsb2$female)+as.numeric(hsb2$ses)+hsb2$science)
summary(reg1)
summary(reg2)

LLSUR = function(a0,a1,a2,a3,b0,b1,b2,b3,s1,s2,s12) 
{ 
  y1 = a0 + a1*(as.numeric(hsb2$female)) + a2*(as.numeric(hsb2$ses)) + a3*(hsb2$socst) 
  y2 = b0 + b1*(as.numeric(hsb2$female)) + b2*(as.numeric(hsb2$ses)) + b3*(hsb2$science) 
  e1 = hsb2$read-y1 
  e2 = hsb2$math-y2 
  S = matrix(c(s1,s12,s12,s2),nrow = 2,ncol = 2) 
  LLsum = sum(dmvnorm(cbind(e1,e2),mean = c(0,0),sigma = S,log = T)) 
  return(-1*LLsum) 
}

res1 = mle2(minuslogl = LLSUR, start = list(a0=mean(hsb2$read),
              a1=0,a2=0,a3=0,b0=mean(hsb2$math),
              b1=0,b2=0,b3=0,s1=100,s2=100,s12=cov(hsb2$read,hsb2$math)))
summary(res1)


