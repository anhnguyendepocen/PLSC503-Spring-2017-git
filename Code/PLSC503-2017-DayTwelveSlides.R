################################################
# PLSC 503 -- Spring 2017: Code for Day Twelve.
################################################

# ANES 2016 pilot study aggregation example...

library(plyr)
library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-Spring-2017-git/master/Data/ANES-pilot-2016.csv")
ANES<-read.csv(text=temp, header=TRUE)
rm(temp)

ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(state),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE))
summary(StateFT)

respfit<-with(StateFT, lm(meantherm~log(Nresp)))

pdf("StateThermPlot.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$state,cex=0.8))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
dev.off()

#Justices data:

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

