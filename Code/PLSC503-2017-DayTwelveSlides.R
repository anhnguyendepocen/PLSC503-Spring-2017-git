################################################
# PLSC 503 -- Spring 2017: Code for Day Twelve.
################################################

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2017-git/master/Data/Justices.csv")
Justices<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Justices)

OLSfit<-with(Justices, lm(civrts~score))
summary(OLSfit)

WLSfit<-with(Justices, lm(civrts~score,weights=lnNedit))
summary(WLSfit)

pdf("WLSBubblePlotR.pdf",6,6)
par(mar=c(4,4,2,2))
with(Justices, symbols(score, civrts,circles=Neditorials,
        ylab="Civil Rights Voting",xlab="Segal-Cover Score",
        ylim=c(0,100)))
abline(reg=OLSfit,lwd=2)
abline(reg=WLSfit,lwd=2,lty=2)
with(Justices, points(score,civrts,pch=20))
legend("topleft",bty="n",lty=c(1,2),lwd=2,
       legend=c("OLS","WLS"))
dev.off()

# "Robust"

library(car)
hccm(OLSfit, type="hc1")

library(rms)
OLSfit2<-ols(civrts~score, x=TRUE, y=TRUE)
RobSEs<-robcov(OLSfit2)
RobSEs

