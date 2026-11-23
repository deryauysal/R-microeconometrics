# R for Microeconometrics: multinomial, conditional, mixed and nested logit; multinomial probit
# Data: Fishing and HC (mlogit package)
#
# Install the packages once:
# install.packages(c("mlogit", "lmtest"))

data("Fishing", package = "mlogit")

library("mlogit")

# in order to convert the dataset
mFishing <- dfidx(Fishing, choice = "mode", varying = c(2:9))

# Multinomial Logit (only individual specific characteristics)
multi<-  mlogit(mode ~ 0 | income, mFishing, reflevel = 'boat' )
summary(multi)

# Conditional Logit (only alternative specific characteristics)  
# with intercept
condi <- mlogit(mode ~ catch + price , mFishing)
summary(condi)

# without intercept
condix <- mlogit(mode ~ catch + price | 0 , mFishing)
summary(condix)

# Mixed Logit (Hybrid Logit) both alternative specific and individual characteristics
mix <- mlogit(mode ~ catch + price | income, mFishing, reflevel = 'boat' )
summary(mix)

# tests (could be applied any hypothesis)
library("lmtest")

lrtest(mix, condix)
waldtest(mix, multi)
scoretest(multi, mix)

# Nested Logit

data("HC", package = "mlogit")
HC <- dfidx(HC, varying = c(2:8, 10:16), choice = "depvar")
cooling.modes <- idx(HC, 2) %in% c('gcc', 'ecc', 'erc', 'hpc')
room.modes <- idx(HC, 2) %in% c('erc', 'er')

HC$icca[! cooling.modes] <- 0
HC$occa[! cooling.modes] <- 0

HC$inc.cooling <- HC$inc.room <- 0
HC$inc.cooling[cooling.modes] <- HC$income[cooling.modes]
HC$inc.room[room.modes] <- HC$income[room.modes]

HC$int.cooling <- as.numeric(cooling.modes)

nested <- mlogit(depvar ~ ich + och +icca + occa + inc.room + inc.cooling + int.cooling | 0, HC,
             nests = list(cooling = c('gcc','ecc','erc','hpc'), 
                          other = c('gc', 'ec', 'er')), un.nest.el = TRUE)
summary(nested)

# (probit models may take some time)
# Multinomial Probit (only individual specific characteristics)
multi<-  mlogit(mode ~ 0 | income, mFishing, reflevel = 'boat' , probit = TRUE)
summary(multi)

# Conditional Probit (only alternative specific characteristics)
# with intercept
pcondi <- mlogit(mode ~ catch + price , mFishing, probit = TRUE)
summary(pcondi)

# without intercept
pcondix <- mlogit(mode ~ catch + price | 0 , mFishing, probit = TRUE)
summary(pcondix)

# Mixed Probit (Hybrid Probit) both alternative specific and individual characteristics
pmix <- mlogit(mode ~ catch + price | income, mFishing, reflevel = 'boat', probit = TRUE )
summary(pmix)





