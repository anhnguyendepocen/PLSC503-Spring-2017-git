################################################
# PLSC 503 -- Spring 2017
#
# Code for Day Six ("Stupid Regression Tricks")
################################################

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2017-git/master/Data/africa2001.csv")
africa<-read.csv(text=temp, header=TRUE)

attach(africa)
fit<-lm(adrate~muslperc)
summary(fit)

# First figure

SEs<-predict(fit,interval="confidence")
Sort<-order(muslperc)

pdf(SRTFig1.pdf,6,6)
plot(muslperc, adrate, xlab="Muslim Percentage of the Population", ylab="HIV Prevalence Rate",pch=16) 
abline(fit,lwd=3)
lines(sort(muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 10

africa$muslplusten<-muslperc+10
fit2<-lm(adrate~muslplusten,data=africa)
summary(fit2)

SEs<-predict(fit2,interval="confidence")

pdf("SRTFig2.pdf",6,6)
plot(africa$muslplusten, adrate, xlab="Muslim Percentage of the Population + 10", ylab="HIV Prevalence Rate",pch=16)
abline(fit2,lwd=3)
lines(sort(africa$muslplusten),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslplusten),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

