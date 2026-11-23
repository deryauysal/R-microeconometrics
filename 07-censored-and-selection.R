# R for Microeconometrics: Tobit (censored regression) and Heckman selection model
# Data: fringe (Wooldridge); mus16data (Cameron and Trivedi) for the Heckman model
#
# Install the packages once:
# install.packages(c("wooldridge", "censReg", "survival", "haven", "sampleSelection"))

data(fringe, package='wooldridge')


# Tobit regression 
library(censReg)
tobit <- censReg(hrbens ~ exper + age + educ + tenure + married + male 
                 + white + nrtheast + nrthcen + south + union, data=fringe)
summary(tobit)

# Partial effects at the average x
margEff(tobit)

# Alternative way for tobit regression
library(survival)
tobitx <- survreg(Surv(hrbens, hrbens>0, type="left")~ exper + age + educ + tenure 
                  + married + male + white + nrtheast + nrthcen + south + union, data=fringe, dist='gaussian')
summary(tobitx)

# Heckman selection model (2 step)
# Data: mus16data.dta from Cameron and Trivedi, "Microeconometrics Using Stata".
# Save the file in your working directory first.
library(haven)
mus16data <- read_dta("mus16data.dta")
  # short data manipulation
mus16data$dy<-0
mus16data$dy[mus16data$ambexp>0] <-1
mus16data$dy<-factor(mus16data$dy, levels=c(0, 1), labels = c(0, 1), ordered=TRUE)
mus16data$lny<-log(mus16data$ambexp)
mus16data$lny[!is.finite(mus16data$lny)] <- NA

library(sampleSelection)
heckman <- selection(dy~age+female+educ+blhisp+totchr,
                     lny~age+female+educ+blhisp+totchr, data=mus16data, method="2step")
summary(heckman)
