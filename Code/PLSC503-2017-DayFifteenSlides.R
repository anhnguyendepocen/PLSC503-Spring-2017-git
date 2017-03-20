################################################
# PLSC 503 -- Spring 2017: Code for Day Fifteen.
################################################
#
# Simulate data:

set.seed(7222009)
N <- 100
X1<-rnorm(N)             # <- X1
X2<-(-X1)+1.5*(rnorm(N)) # <- correlated w/X1
Y<-X1-(2*X2)+(2*(rnorm(N))) # <- Y
Z<-10*rnorm(N)           # <- irrelevant
data <- data.frame(Y=Y,X1=X1,X2=X2,Z=Z)

# Scatterplot matrix:

library(car)

pdf("MisspecificationExampleScatterplotMatrixR.pdf",7,7)
scatterplotMatrix(data)
dev.off()

# "Correct" model:

correct<-lm(Y~X1+X2)
summary(correct)

# "Overspecified" model:

overspec<-lm(Y~X1+X2+Z)
summary(overspec)

# "Underspecified" model:

incorrect<-lm(Y~X1)
summary(incorrect)

# Omitted variable plot:

pdf("MisspecifiedResidualsR.pdf",6,6)
plot(data$X2,incorrect$residuals,pch=20,
     xlab="Omitted Variable",ylab="Residuals")
dev.off()

