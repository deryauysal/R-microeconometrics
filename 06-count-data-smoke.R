# R for Microeconometrics: count data models (Poisson, negative binomial, hurdle, zero-inflated Poisson)
# Data: smoke (Wooldridge), cigarettes smoked per day
#
# Install the packages once:
# install.packages(c("wooldridge", "pastecs", "MASS", "pscl", "VGAM"))

data(smoke, package='wooldridge')  # Data on the number of cigarettes smoked per day.
smoke$agesq <- smoke$age^2/100  # age squared divided by 100 (avoids numerical problems in hurdle and ZIP)
library(pastecs) #to calculate descriptive statistics

stat.desc(smoke$cigs)
hist(smoke$cigs, breaks=15, col="blue")

dens <- density(smoke$cigs)
plot(dens, frame = FALSE, col = "blue", main = "Density plot of cigarettes consumption") 

#Poisson Regression
poissonreg <- glm (cigs~log(cigpric)+log(income)+restaurn+white+educ+age+agesq, data=smoke, family=poisson)
summary(poissonreg)

poissonpredict<-predict(poissonreg, smoke, type="response") 
head(poissonpredict)
hist(poissonpredict)

library(pscl)
# Average predicted probability of each count (0, 1, 2, ...) from the Poisson model
trial <- predprob(poissonreg)
barplot(colMeans(trial), col = "blue", main = "Average predicted probabilities from poisson")

#NEGBIN (Negative Binomial) Regression
library(MASS) #to run negbin regression 
negbinreg <- glm (cigs~log(cigpric)+log(income)+restaurn+white+educ+age+agesq, data=smoke, family = negative.binomial(theta =1) )
summary(negbinreg)

negbinpredict<-predict(negbinreg, smoke, type="response") 
head(negbinpredict)
hist(negbinpredict)
dens2 <- density(negbinpredict)
plot(dens2, frame = FALSE, col = "blue", main = "Predicted density plot of cigarettes consumption from negbin") 

# Estimate theta as well. MASS::glm.nb does not converge for these data
# (theta drifts to infinity), so we use VGAM::vglm with the negbinomial family.
library(VGAM)
negbinreg1 <- vglm(cigs~log(cigpric)+log(income)+restaurn+white+educ+age+agesq, negbinomial, data=smoke)
summary(negbinreg1)
exp(coef(negbinreg1)[["(Intercept):2"]])  # theta (size parameter)

#Count Data Hurdle Model
library(pscl) #to run Hurdle and ZIP model
hurdlereg <- hurdle(cigs~log(cigpric)+log(income)+restaurn+white+educ+age+agesq, data=smoke,
       dist = "poisson", zero.dist = "binomial", link = "logit")
summary(hurdlereg)

hurdlepredict<-predict(hurdlereg, smoke, type="response")
head(hurdlepredict)
hist(hurdlepredict)
dens3 <- density(hurdlepredict)
plot(dens3, frame = FALSE, col = "blue", main = "Predicted density plot of cigarettes consumption from hurdle") 

#ZIP (Zero-Inflated Poisson) Model
ZIP <- zeroinfl(cigs~log(cigpric)+log(income)+restaurn+white+educ+age+agesq, data=smoke,
         dist = "poisson", link = "logit")
summary(ZIP)

ZIPpredict<-predict(ZIP, smoke, type="response") 
head(ZIPpredict)
hist(ZIPpredict)
dens4 <- density(ZIPpredict)
plot(dens4, frame = FALSE, col = "blue", main = "Predicted density plot of cigarettes consumption from ZIP")
