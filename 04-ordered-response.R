# R for Microeconometrics: ordered probit and ordered logit
# Data: pension (Wooldridge), asset allocation in pension plans
#
# Install the packages once:
# install.packages(c("wooldridge", "rms"))

data(pension, package='wooldridge')  #Data on on asset allocation in pension plans.
# Responses: mostly bonds, mixed, and mostly stocks, coded 0, 50, and 100.

library(rms)

ddist<- datadist("choice", "prftshr","finc25","finc35", "finc50","finc75","finc100", "finc101","female", "black", "age", "educ", "wealth89")
options(datadist="ddist")

#Ordered Probit Model
oprobit<- orm(formula = pctstck ~ choice + female + black + prftshr+ age + educ + wealth89 + finc25 + finc35 + finc50 + finc75 + finc100 + finc101, data = pension,  family = probit)
oprobit

AIC(oprobit); BIC(oprobit)

#Ordered Logit Model 
ologit<- orm(formula = pctstck ~ choice + female + black + prftshr+ age + educ + wealth89 + finc25 + finc35 + finc50 + finc75 + finc100 + finc101, data = pension) #logistic distribution is default
ologit

AIC(ologit); BIC(ologit)
