library(faraway)
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
Hw3lm2.1<-lm(y~X2+X5+X6,data = Hw3data[-1,])
