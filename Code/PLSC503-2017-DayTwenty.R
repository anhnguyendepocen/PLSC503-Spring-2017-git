########################################################
# PLSC 503 -- Spring 2017: Code for Day Twenty.
########################################################

# Get HIV data:

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2017-git/master/Data/HIV2005.csv")
HIV<-read.csv(text=temp, header=TRUE)
attach(HIV)
rm(temp)

# Libraries...

library(maxLik)
library(aod)
library(lmtest)

# log-lik function:

HIVll <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  ll <- -0.5*log(sigma^2) - (0.5*((x-mu)^2/sigma^2))
  ll
}

x<-logHIV

# fit the model:

hats <- maxLik(HIVll, start=c(0,1))
summary(hats)

# mean-only linear model for comparison:

HIVLM<-lm(logHIV~1)
summary(HIVLM)

# components:

hats$estimate
hats$gradient
hats$hessian

-(solve(hats$hessian))
sqrt(-(solve(hats$hessian)))

# Wald test:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,verbose=TRUE)

# More Wald tests:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(0,2))

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(-0.5,2))

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=2:2,H0=2)

# nonsensical:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(1,-2))

# LR test...

HIVllOne <- function(param) {
       mu <- param[1]
    ll <- -0.5*log(4) - (0.5*((x - mu)^(2)/4))
    ll
  }

hatsF <- maxLik(HIVll, start=c(0,1))
hatsR <- maxLik(HIVllOne, start=c(0))

hatsF$maximum
hatsR$maximum

-2*(hatsR$maximum-hatsF$maximum)

pchisq(-2*(hatsR$maximum-hatsF$maximum),df=1,lower.tail=FALSE)


