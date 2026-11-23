# R for Microeconometrics: binary response models (LPM, logit, probit)
# Data: mroz (Wooldridge), labor force participation of married women
#
# Install the packages once:
# install.packages(c("wooldridge", "car", "lmtest", "DescTools"))

data(mroz, package='wooldridge')

library(car); library(lmtest)  # for robust SE

# Estimate linear probability model
linprob <- lm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)

# Regression table with heteroscedasticity-robust SE and t tests:
coeftest(linprob,vcov=hccm)

# list for two "extreme" women:
xpred <- list(nwifeinc=c(100,0),educ=c(5,17),exper=c(0,30),age=c(20,52),kidslt6=c(2,0),kidsge6=c(0,0))

# we can specify our link function via "family=binomial(link=logit) or family=binomial(link=probit)" commands

# Estimate logit model
logitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=logit),data=mroz)

# to obtain maximum likelihood estimation of parameters
# Summary of results:
summary(logitres)

# Log likelihood value:
logLik(logitres) 

# Odds ratios:
exp(coef(logitres))

# McFadden's pseudo R2:
1 - logitres$deviance/logitres$null.deviance
# Other pseudo R2 measures (DescTools package):
library(DescTools)
PseudoR2(logitres, which = "all")

# Estimate probit model
probitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=probit),data=mroz)

# Summary of results:
summary(probitres)

# Log likelihood value:
logLik(probitres) 

# McFadden's pseudo R2:
1 - probitres$deviance/probitres$null.deviance
PseudoR2(probitres, which = "all")

# Test of overall significance:
# Manual calculation of the LR(likelihood ratio) test statistic:
probitres$null.deviance - probitres$deviance

# Automatic calculations including p-values,...:

library(lmtest)
lrtest(probitres)

# Test of H0: experience and age are irrelevant

restr <- glm(inlf~nwifeinc+educ+ kidslt6+kidsge6,family=binomial(link=probit),data=mroz)
lrtest(restr,probitres)

# Predictions from linear probability, probit and logit model for two "extreme" women:
# with type = "response" command we can directly obtain probabilities 

linprob2.p <-predict(linprob,  xpred,type = "response")
logitres2.p <-predict(logitres, xpred,type = "response")
probitres2.p <-predict(probitres,xpred,type = "response")

# Predictions from linear probability, probit and logit model for whole dataset: 

linprob.p <-predict(linprob, mroz,type = "response")
logitres.p <-predict(logitres,mroz,type = "response")
probitres.p <-predict(probitres, mroz,type = "response")

# APEs 

# Calculation of linear index at individual values:
xb.log <- predict(logitres)
xb.prob<- predict(probitres)

# APE factors = average(g(xb))
factor.log <- mean( dlogis(xb.log) )
factor.prob<- mean( dnorm(xb.prob) )
cbind(factor.log,factor.prob)

# average partial effects = beta*factor:
APE.lin <- coef(linprob) * 1
APE.log <- coef(logitres) * factor.log
APE.prob<- coef(probitres) * factor.prob

# Table of APEs
cbind(APE.lin, APE.log, APE.prob)

# We can plot graphs for three models above from data generating process 
# For example, binary yi , i.e. yi = 1; if 1 + 3Xi + "i > 2 and yi = 0 if otherwise.

set.seed(28052020)
x<-rnorm(1000,0,1)
e<-rnorm(1000,0,1)
y <- ifelse (1+3*x+e>2,1,0)

linear<-lm(y~x)
logit<-glm(y~x,family=binomial(link=logit))
probit<-glm(y~x,family=binomial(link=probit))

xp<- seq(from=min(x),to=max(x),length=1000)

linearp<-predict(linear,list(x=xp),type = "response")
logitp<-predict(logit,list(x=xp),type = "response")
probitp<-predict(probit,list(x=xp),type = "response")

plot(x,y)
lines(xp,linearp,col=2,lwd=1,lty=1)
lines(xp,logitp,col=3,lwd=2,lty=2)
lines(xp,probitp,col=4,lwd=1,lty=1)
legend("topleft",c("LPM","Logit","Probit"), col = 2:4,lwd=c(1,2,1),lty=c(1,2,1))
