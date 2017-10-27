#Statistical Estimation
data1 <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/data1.csv")
View(data1)

x=data1$x1

#Data Generation Process(DGP) N(M,2)
M=6 #let's assume
LLsum = sum(dnorm(x = x,mean = M,sd = 2,log = TRUE)) #Log Likelihood

f1=function(M)
{
  LLsum = sum(dnorm(x=x,mean = M,sd = 2,log = TRUE)) #Log Likelihood
  return(LLsum)
}
f1(10)
mseq=seq(0,10,by = 0.05)
LLres=sapply(mseq,f1)
plot(LLres)
i = which.max(LLres)
mseq[i]

# bbmle package
install.packages("bbmle")
library(bbmle)

#DGP - N(M,2)

f1=function(M)
{
  LLsum = sum(dnorm(x,mean = M,sd = 2,log = T))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(M=10))
summary(res)


#DGP - N(M,S)

f1=function(M,s)
{
  LLsum = sum(dnorm(x,mean = M,sd = s,log = T))
  return(-1*LLsum)
}
f1(10,3)

res = mle2(minuslogl = f1,start = list(M=10,s=3),method = "L-BFGS-B",lower=c(s=0))
summary(res)

#DGP - Pois(lambda)
data2 <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 5/Data/data2.csv")
View(data2)

data=data2$x1

f1=function(M)
{
  LLsum = sum(dpois(data,lambda = M,log = T))
  return(-1*LLsum)
}
f1(2)

res = mle2(minuslogl = f1,start = list(M=1))
summary(res)


#zero inflated posison

x = data2$x2
plot(table(x))

#v=(sick, nonsick)
#prob=(1-p,p)
#-----------------------------------------
#sample data  |       #probability
#-----------------------------------------
#     0       |   p+(1-p)*dpois(0,lambda)
#     1       |   (1-p)*dpois(1,lambda)
#     3       |   (1-p)*dpois(3,lambda)
#-----------------------------------------

f1=function(l,p)
{
  L = ifelse(x==0,p+(1-p)*dpois(0,l),(1-p)*dpois(x,l))
  LLsum = sum(log(L))
  return(-1*LLsum)
}
f1(5,0.5)

res = mle2(minuslogl = f1,start = list(l=5,p=0.5))
summary(res)
#there is 33% zero inflation
#fix the warnings
res = mle2(minuslogl = f1,start = list(l=1,p=0.5),method = "L-BFGS-B",
           lower=c(l=0,p=0),upper=c(l=Inf,p=1))
summary(res) #this method does not seem to produce good result. 



