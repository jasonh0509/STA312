library(leaps)

##a
DataA<-data.frame(V1=c(1,1,0),V2=c(2,0,0),V3=c(3,1,1))
A<-cbind(DataA$V1,DataA$V2,DataA$V3)


##b
set.seed(2266);Z<-matrix(rnorm(600),3,200)
round(Z[,1:5],3)

##c

V<-A%*%Z
round(V[,1:5],3)


##d
cov(t(V))




##Q4
v<-c(-4,1,3,0,0,-1,1)

set.seed(2466)
X<-cbind(1,sample(v,50,replace = TRUE),
         sample(v,50,replace = TRUE),
         sample(c(0,1),50,replace = TRUE),
         sample(1:10,50,replace = TRUE),
         sample(1:10,50,replace = TRUE))
colnames(X)<-c("X1","X2","X3","X4","X5","X6")
head(X)


##c
set.seed(2488);eps<-sample(c(-5,-4,-3,-2,-1,1,2,3,4,5),50,replace = TRUE)
var(eps)

beta<-cbind(c(2,1,0,0,1,2))
y<-X%*%beta+eps
HW3data<-data.frame(X[,-1],y)

##f
cor(HW3data)

pairs(HW3data)

##g
Hw3full<-lm(y~X2+X3+X4+X5+X6,data = HW3data)
Hw3null<-lm(y~1,data = HW3data)
summary(Hw3full)
summary(Hw3null)

##h

Hw3regs<-regsubsets(y~.,nbest = 3,data = HW3data)
plot(Hw3regs,scale = "Cp")
plot(Hw3regs,scale = "adjr2")

##i
summary(Hw3regs)$outmat
summary(Hw3regs)$cp
summary(Hw3regs)$adjr2
summary(Hw3regs)$rss

##j
library(car)
subsets(Hw3regs,statistic="cp",legend=FALSE,ylim=c(2,7))
abline(1,1)

##k
step(Hw3full,direction = "backward",
     scale = summary(Hw3full)$sigma^2,trace = TRUE)
step(Hw3null,scope=list(lower=Hw3null,upper=Hw3full),
     direction = "forward",scale = summary(Hw3full)$sigma^2,trace = TRUE)
step(Hw3full,scope = list(lower=Hw3null,upper=Hw3full),
     direction = "both",scale=summary(Hw3full)$sigma^2,trace = TRUE)

##i

