library(faraway)
library(car)
##Q4
v<-c(-4,1,3,0,0,-1,-1)
set.seed(2466)
X<-cbind(1,sample(v,50,replace=TRUE),
         sample(v,50,replace=TRUE),
         sample(c(0,1),50,replace=TRUE),
         sample(1:10,50,replace=TRUE),
         sample(1:10,50,replace=TRUE))
colnames(X)<-c("X1","X2","X3","X4","X5","X6")
set.seed(2488)
eps<-sample(c(-5,-4,-3,-2,-1,1,2,3,4,5),50,replace=TRUE)
beta<-cbind(c(2,1,0,0,1,2))
y<-X%*%beta+eps
Hw3data<-data.frame(X[,-1],y)

Hw3lm2<-lm(y~X2+X5+X6,data = Hw3data)
X<-model.matrix(Hw3lm2)
x1<-cbind(X[1,])

##b
Hw3lm2.1<-lm(y~X2+X5+X6,data = Hw3data[-1,])

epsilon_hat<-residuals(Hw3lm2)
SSE<-deviance(Hw3lm2)
H<-X%*%solve(t(X)%*%X)%*%t(X)
beta_hat<-coef(Hw3lm2)
h<-diag(H)
beta_hat.1<-beta_hat-solve(t(X)%*%X)%*%cbind(x1)*epsilon_hat[1]/(1-h[1])
SSE.1<-SSE-(epsilon_hat[1]^2/(1-h[1]))

##c
n<-nrow(X);p<-ncol(X)
sigma_hat<-sqrt(SSE/(n-p))
sigma_hat.1<-sqrt(SSE.1/(n-1-p));sigma_hat.1


hatvalues(Hw3lm2)[1]
rstandard(Hw3lm2)[1]
rstudent(Hw3lm2)[1]
cooks.distance(Hw3lm2)[1]hatvalues(Hw3lm2)[1]
rstandard(Hw3lm2)[1]
rstudent(Hw3lm2)[1]
cooks.distance(Hw3lm2)[1]


##d
influencePlot(Hw3lm2)


##e
SSE.i<-influence(Hw3lm2)$sigma^2*(n-1-p)
plot(SSE.i,type="n")
text(SSE.i,labels=1:50,cex=.7,col=4)


##f
summary(influence.measures(Hw3lm2))
