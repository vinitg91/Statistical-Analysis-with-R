#Statistics

#coin toss
#data generation process
v = c("H","T") #possible outcomes
p = c(0.5,0.5) #probability of each outcome
sample(x = v,size = 10,prob = p,replace = T) #Population

v = seq(1,6)
p = rep(1/6,6)
sample(size = 5, x=v, prob = p,replace = T)

#normal distribution
#dnorm gives density value. Probability at any given point on x axis
#pnorm gives area when we specify point. Area is the cumulative probability
#qnorm gives quantile value when we give area
#rnorm random normal distribution 
#the above functions are for population formula. When we have data, it is a sample.

#convolutions
r1 = rnorm(3000)
u1 = runif(3000)

plot(density(r1))
plot(density(u1)) #In shapiro test, the null hypothesis is that sample is normally distributed
shapiro.test(r1)
shapiro.test(u1)

r2 = rnorm(3000)
u2 = runif(3000)
u3 = runif(3000)
u4 = runif(3000)
u5 = runif(3000)

x = r1+r2
plot(density(x))
shapiro.test(x)

x = u1+u2+u3+u4+u5
plot(density(x))
shapiro.test(x)

#adding enough number of uniform distributions makes a normal distribution 

t1 = rnorm(n = 100000, mean = 5, sd = 4)
t2 = runif(n = 100000, min = 3, max = 7)
plot(density(t1))
plot(density(t2))

#task 2 done after task 1
t = t1+t2
plot(density(t))
summary(t)
length(t[t<10])
length(t[t<10])/length(t) #Probability of t being less than 10

#if both tasks done parallely
t = ifelse(t1>t2,t1,t2)
plot(density(t))
summary(t)
length(t[t<10])
length(t[t<10])/length(t) #Probability of t being less than 10


#sampling error
x = rnorm(20, mean = 12, sd = 7)
x
summary(x) #Observe that the mean of 'x' is not equal to 12
sd(x) #Observe that the SD of 'x' is not equal to 7
#This accounts for the sampling error. 

v = c(0,1)
p = c(0.5,0.5)
x = sample(size = 10,prob = p,x = v,replace = T)
sum(x)

f1 = function()
{
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = 10,prob = p,x = v,replace = T)
  return(sum(x))
  
}
x = rep(f1(), 10000) #returns the same result 10000 times 
x = replicate(n = 10000, f1())  #runs the function 10000 times and returns different results
summary(x)
table(x)
hist(x)
prop.table(table(x))




#statistical testing
admission <- read.csv("D:/MSBAPM/Sem-2/OPIM-5503-Data-Analytics-using-R/Session 4/Data/admission.csv")
View(admission)
str(admission)
GMAT = admission$GMAT
GMAT

#test statistic
tstat = mean(GMAT)

#draw a sample and compute the statistic
f1 = function()
{
  s = rnorm(n = length(GMAT),mean = 510,sd = sd(GMAT))
  return(mean(s))
}
f1()

#create the sampling distribution
sdist = replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat) 
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

#-----------------------------------------------------------------------------------
#hypothesis : sd = 78

tstat = sd(GMAT)
f1 = function()
{
  s = rnorm(n = length(GMAT), mean = mean(GMAT), sd = 78)
  return(sd(s))
}

sdist = replicate(10000, f1())
plot(density(sdist))
abline(v=tstat, col="red")

gap = abs(mean(sdist)-tstat) 
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist) #cannot reject null hypothesis

#----------------------------------------------------------------------------#
tstat = median(GMAT)
f1 = function()
{
  s = rnorm(n = length(GMAT), mean = 500, sd = sd(GMAT))
  return(median(s))
}

sdist = replicate(10000, f1())
plot(density(sdist))
abline(v=tstat, col="red")

gap = abs(mean(sdist)-tstat) 
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist) #cannot reject null hypothesis

#----------------------------------------------------------------------------
#hypothesis : mean = 6.5*(sd)

#test statistic
tstat = mean(GMAT)/sd(GMAT)

#draw a sample and compute the statistic
f1 = function()
{
  s = rnorm(n = length(GMAT),mean = 6.5,sd = 1)
  return(mean(s)/sd(s))
}

#create the sampling distribution
sdist = replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat) 
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)


#----------------------------------------------------------------------------

#6 March 2017

#Null Hypothesis : 75%ile = 600

qnorm(mean = 532.5,sd = 100,p = 0.75)

tstat = quantile(GMAT,probs = 0.75)

f1 = function()
{
  x = rnorm(n=length(GMAT), mean = 532.5, sd=100)
  return(quantile(x,0.75))
}

sdist = replicate(10000,f1())

plot(density(sdist))
abline(v=tstat, col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap | sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

#pvalue = 0, we reject the null hypothesis. 

#----------------------------------------------------------------------------
#cor test

GPA = admission$GPA

#hypothesis :correlation between GPA and GMAT is zero

#test statistic
tstat = cor(GPA, GMAT)
tstat

#draw a sample and compute the statistic
f1 = function()
{
  xGPA = rnorm(n = length(GMAT),mean = mean(GPA),sd = sd(GPA))
  xGMAT = rnorm(n = length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  return(cor(xGPA,xGMAT))
}

#create the sampling distribution
sdist = replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat) 
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

#----------------------------------------------------------------------------



#shape test

#hypothesis : GPA and GMAT distribution shapes are same

plot(density(GMAT))
plot(density(GPA))

#Normalize GPA and GMAT to bring them on same scale
GPA1 = (GPA-mean(GPA))/sd(GPA)
GMAT1 = (GMAT-mean(GMAT))/sd(GMAT)

plot(density(GMAT1))
lines(density(GPA1),col="blue")

q = c(0.1,0.3,0.5,0.7,0.95)

qGPA = quantile(GPA1,q)
qGMAT = quantile(GMAT1,q)

tstat = sum(abs(qGPA-qGMAT))

f1 = function()
{
  xGPA = rnorm(n = length(GMAT), mean = 0,sd = 1)
  xGMAT = rnorm(n = length(GMAT), mean = 0,sd = 1)
  q1 = quantile(xGPA,q)
  q2 = quantile(xGMAT,q)
  return(sum(abs(q1-q2)))
}

sdist = replicate(10000,f1())

plot(density(sdist))
abline(v=tstat,col="red")

s1 = sdist[sdist>tstat]
pvalue = length(s1)/length(sdist)
pvalue

#pvalue = 0.3571, cannot reject null hypothesis

#----------------------------------------------------------------------------

#factor data
View(admission)
DE = admission$De
#Hypothesis : 40% students get admit

tstat = length(DE[DE=="admit"])/length(DE)
#OR
tstat = prop.table(table(DE))[1]

f1 = function()
{
  v = c("admit","other")
  p = c(0.4,0.6)
  x = sample(size = length(GMAT),x = v,prob = p,replace = TRUE)
  return(prop.table(table(x))[1])
}

sdist = replicate(10000,f1())

plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

#----------------------------------------------------------------------------


#Non parametric testing - median = 500

#test statistic

tstat = sum(ifelse(GMAT > 500, 1,0))

f1 = function()
{
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = length(GMAT),x = v,prob = p,replace = TRUE)
  return(sum(x))
}

sdist = replicate(10000,f1())

plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue


#Non parametric test cannot be done on mean and sd. we can do on quantiles.
#The test is less powerful than parametric test 
#because we use less information compared with parametric test

#In case of conflict, non parametric is more accurate as there are no assumption


#Confidence Interval

#Parametric
n = length(GMAT)
f1 = function()
{
  x = rnorm(n=n , mean=mean(GMAT),sd=sd(GMAT))
  return(median(x))
}

sdist = replicate(10000,f1())

#plot and compute confidence Interval
plot(density(sdist))
quantile(sdist,c(0.025,1-0.025))


#Bootstrapped confidence interval

f1= function()
{
  x =sample(size = n,x = GMAT,replace = TRUE)
  return(median(x))
}

sdist = replicate(10000,f1())

#plot and compute confidence Interval
plot(density(sdist))
quantile(sdist,c(0.025,1-0.025))


#Two sample non parametric test
twosample <- read.csv("D:/MSBAPM/Sem 2/OPIM 5503- Data Analytics using R/Session 4/Data/twosample.csv")

treatment = twosample[twosample$group == "Treatment",2]
control = twosample[twosample$group == "Control",2]

n = length(treatment)

tstat = mean(treatment) - mean(control)

f1 = function()
{
  x = c(treatment, control)
  x = sample(x)
  m1 = mean(x[1:n])
  m2 = mean(x[(n+1):length(x)])
  return (m1 - m2)
}

sdist = replicate(10000,f1())

plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue


#multivariate distribution and hypothesis testing
#Correlation between two variables
#correlation = covariance/(sigma1*sigma2)

install.packages("mvtnorm")
library(mvtnorm)

M = c(10,4)
S = matrix(c(2,1,1,3),nrow = 2,ncol = 2)
x = rmvnorm(n = 1000,mean = M,sigma = S)
cor(x[,1],x[,2])


#hypothesis: cor is 0.6
tstat = cor(GPA,GMAT)

f1 = function()
{
  M = c(mean(GPA),mean(GMAT))
  S = matrix(c(var(GPA),0.6*sd(GPA)*sd(GMAT),0.6*sd(GPA)*sd(GMAT),var(GMAT)),
             nrow = 2,ncol = 2)
  x = rmvnorm(n = length(GMAT),mean = M,sigma = S)
  return(cor(x[,1],x[,2]))
}

sdist = replicate(10000,f1())
plot(density(sdist))
abline(v=tstat,col="red")

gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist < mean(sdist)-gap | sdist > mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue