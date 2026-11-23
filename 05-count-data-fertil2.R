# R for Microeconometrics: count data models (Poisson, negative binomial, hurdle, zero-inflated Poisson)
# Data: fertil2 (Wooldridge), number of children
#
# Install the packages once:
# install.packages(c("wooldridge", "pastecs", "MASS", "pscl", "VGAM"))

data(fertil2, package='wooldridge')  # Data on the number of children ever born.
library(pastecs) #to calculate descriptive statistics
library(VGAM)

stat.desc(fertil2$children)
hist(fertil2$children, breaks=15, col="blue")

dens <- density(fertil2$children)
plot(dens, frame = FALSE, col = "blue", main = "Density plot of the number of children") 

# A. Poisson Regression
poissonreg <- glm (children~tv+electric+evermarr+urban+educ+age+I(age^2), data=fertil2, family=poisson)
summary(poissonreg)

#to see fitted values in a histagrom
hist(poissonreg$fitted.values)

#alternative way to see predicted values in a histagrom
poissonpredict<-predict(poissonreg, fertil2, type="response") 
hist(poissonpredict)

#to see predicted values
head(poissonpredict)

# B. NEGBIN (Negative Binomial) Regression
library(MASS) #to run negbin regression 
negbinreg <- glm (children~tv+electric+evermarr+urban+educ+age+I(age^2), data=fertil2, family = negative.binomial(theta =1) )
summary(negbinreg)

hist(negbinreg$fitted.values)

negbinpredict<-predict(negbinreg, fertil2, type="response") 
hist(negbinpredict)

head(negbinpredict)

#another method to run negbin regression and see theta
library(MASS) 
negbinreg1 <- glm.nb (children~tv+electric+evermarr+urban+educ+age+I(age^2), data=fertil2 )
summary(negbinreg1)
negbinreg1$theta
# A very large theta means there is no overdispersion: the negative binomial
# model reduces to the Poisson model (glm.nb warns that the iteration limit is reached).

# C.Count Data Hurdle Model
library(pscl) #to run Hurdle and ZIP model
hurdlereg <- hurdle(children~tv+electric+evermarr+urban+educ+age+I(age^2), data=fertil2,
                    dist = "poisson", zero.dist = "binomial", link = "logit")
summary(hurdlereg)

hist(hurdlereg$fitted.values)

hurdlepredict<-predict(hurdlereg, fertil2, type="response")
hist(hurdlepredict)

head(hurdlepredict)

# D.ZIP (Zero-Inflated Poisson) Model
ZIP <- zeroinfl(children~tv+electric+evermarr+urban+educ+age+I(age^2), data=fertil2,
                dist = "poisson", link = "logit")
summary(ZIP)

hist(ZIP$fitted.values)

ZIPpredict<-predict(ZIP, fertil2, type="response") 
hist(ZIPpredict)

head(ZIPpredict)
