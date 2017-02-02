################################################
# PLSC 503 -- Spring 2016: Code for Day Eleven.
################################################
# Exercise One:

seed<-7222009
set.seed(seed)
U1<-runif(1000)
hist(U1)
abline(h=100,lty=2)

# 1.b

listU<-paste("U",1:1000,sep="")
U<-sapply(listU, function(U) U<-runif(1000))

# or

U<-data.frame(matrix(nrow=1000,ncol=1000))
colnames(U)<-paste("U",1:1000,sep="")
for (i in 1:1000) {
  U[,i]<-runif(1000)
}

# 1.c

Seq<-seq(1,1000,1)
V<-U+Seq

# 1.d

A<-numeric(1000)
for(i in 1:1000) {
  A[i]<-sum(V[,i])  }
hist(A,freq=FALSE)
lines(density(A))

# 1.e

S<-numeric(1000)
for(i in 1:1000) {
  S[i]<-sum(V[i,])  }
hist(S,freq=FALSE)
lines(density(S))

# 2.a

G <- 1-2*(log(-log(U)))

# 2.b

VG<-numeric(1000)
for(i in 1:1000) {
  VG[i]<-var(G[,i])  }
plot(density(VG))
abline(v=((3.14159265^2) / 6) * 4,lty=2)

# 2.c

Q75<-numeric(1000)
for(i in 1:1000) {
  Q75[i]<-quantile(G[,i], .75)
}
plot(density(Q75))

# 3.a

Y<-(-2*G)+(rnorm(1000,mean=0,sd=2))

# 3.b

Corrs<-numeric(1000)
for(i in 1:1000){
  Corrs[i]<-cor(G[,i],Y[,i]) 
}
plot(density(Corrs))
abline(v=mean(Corrs),lty=2)


# Bootstrapping:

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

# Bootstrapping

N<-100
reps<-999

set.seed(1337)
X<-rnorm(N)
Y<-2+2*X+rnorm(N)
data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975)) # <-- 95% c.i.s
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

