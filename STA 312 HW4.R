library(faraway)
library(car)

##Q3
###a
XFrame<-data.frame(X1=c(rep(1,5)),X2=c(1,0,0,1,0))
X<-as.matrix(XFrame)
y<-as.matrix(c(2,0,3,0,3))
solve(t(X)%*%X)
X1<-X[2:5,]
solve(t(X1)%*%X1)

###b
t(X)%*%X


###c
beta_hat<-(solve(t(X)%*%X)%*%t(X))%*%y;beta_hat
y_hat<-X%*%beta_hat;y_hat
epsilon_hat<-y-y_hat;epsilon_hat
SSE<-t(epsilon_hat)%*%epsilon_hat;SSE

###d
H_mtx<-X%*%solve(t(X)%*%X)%*%t(X)
H_mtx
sum(diag(H_mtx))


###e
beta_hat_1<-beta_hat-solve(t(X)%*%X)%*%X[1,]*(epsilon_hat[1]/(1-H_mtx[1,1]))
SSE1<-SSE-(epsilon_hat[1]/(1-H_mtx[1,1]))

###f
y1<-y[-1,]
beta_hat_1_f<-solve(t(X1)%*%X1)%*%t(X1)%*%y1;beta_hat_1_f
y_hat_1_f<-X1%*%beta_hat_1_f;y_hat_1_f
epsilon_hat_1_f<-y1-y_hat_1_f;epsilon_hat_1_f
SSE1_f<-t(epsilon_hat_1_f)%*%epsilon_hat_1_f;SSE1_f

###g
sig_sq<-SSE/(nrow(X)-ncol(X));sig_sq
sig_sq1<-SSE1/((nrow(X1))-ncol(X1));sig_sq1
r1<-sqrt((1)/((sig_sq)*(1-0.5)));r1
t<-1/(sqrt(sig_sq1)*sqrt(1-0.5));t
D1<-r1^2*(H_mtx[1,1]/(1-H_mtx[1,1]))*(1/ncol(X));D1

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
beta_hat.1<-beta_hat-solve(t(X)%*%X)%*%cbind(x1)*epsilon_hat[1]/(1-h[1]);beta_hat.1
SSE.1<-SSE-(epsilon_hat[1]^2/(1-h[1]));SSE.1

##c
n<-nrow(X);p<-ncol(X)
sigma_hat<-sqrt(SSE/(n-p));sigma_hat
sigma_hat.1<-sqrt(SSE.1/(n-1-p));sigma_hat.1


hatvalues(Hw3lm2)[1]
rstandard(Hw3lm2)[1]
rstudent(Hw3lm2)[1]
cooks.distance(Hw3lm2)[1]



##d
influencePlot(Hw3lm2)


##e
SSE.i<-influence(Hw3lm2)$sigma^2*(n-1-p);SSE.i
plot(SSE.i,type="n")
text(SSE.i,labels=1:50,cex=.7,col=4)


##f
summary(influence.measures(Hw3lm2))



##Q8