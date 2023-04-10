library(leaps)
permcol<-
  function (X,y,Permute,n=1000,hist=TRUE,lm=FALSE)
  {
    X2<-X
    if (is.null(colnames(X2))){cn<-1:ncol(X)} else {cn<-colnames(X2)}
    cat("\n")
    cat("===========\n")
    cat(noquote(paste(c("Response (y): ",paste(cn[y],collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("All: ",paste(cn,collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("Keep (X2): ",paste(cn[-c(Permute,y)],collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("Permute: ",paste(cn[Permute],collapse=", "),"\n"),collapse="")))
    X<-X2[,-c(Permute,y)];V<-X2[,Permute];y<-X2[,y]
    cat("-----------\n")
    if (is.null(X)) {X<-rep(1,length(y))}
    V<-as.matrix(cbind(V)); X<-as.matrix(cbind(X));
    if(dim(X)[2]==0) {X<-cbind(X,1)}
    D<-NULL
    for (i in 1:n)
    {
      v2<-(apply(V,2,sample)); ylm2<-lm(y~X+v2);
      D<-c(D,deviance(ylm2))
    }
    ylm<-lm(y~X+V); Dr<-deviance(ylm)
    D<-round(D,8);Dr<-round(Dr,8)
    if(hist){
      hist(D,main="Histogram of deviances")
      if (min(D)<Dr) rug(Dr,col=4,lwd=4)}
    cat(noquote(paste(c("Target Deviance: ",round(Dr,3),"\n"),collapse="")))
    cat("-----------\n")
    a<-length(D[D<Dr])+length(D[D==Dr])/2
    if ((a/n)==0) {
      cat(paste(c("Smaller deviances: ","None","\n"),collapse=""))
      cat(noquote(paste(c("Best/smallest: ", head(round(min(D),3)),"\n"),
                        collapse="")))
    } else {
      U1<-paste(round(head(sort(D[D<=Dr])),2),collapse=", ")
      #U2<-paste(round(tail(sort(D[D<=Dr])),2),collapse=", ")
      cat(paste(c("Smaller deviances: ",a,"\n"),collapse=""))
      cat(noquote(paste(c(U1,"\n"),collapse="")))
      cat("-----------\n")
      cat(noquote(paste(c("Proportion: ",a/n,"\n"),collapse="")))
    }
    cat("-----------\n")
    Keep<-X;Permute<-V
    lm1<-lm(y~Keep)
    ylm<-lm(y~Keep+Permute)
    if(lm) {print(summary(ylm))}
    print(anova(lm1,ylm))
    cat("===========\n")
  }




##a
DataA<-data.frame(V1=c(1,1,0),V2=c(2,0,0),V3=c(3,1,1))
A<-cbind(DataA$V1,DataA$V2,DataA$V3)


##b
set.seed(2266);Z<-matrix(rnorm(600),3,200)
round(Z[,1:5],3)

##c

V<-A%*%Z
round(V[,1:5],3)
V[1,1]
Z[1,1]+2*Z[2,1]+3*Z[3,1]


##d
cov(t(V))

##e
V%*%(I)




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

eps


##e
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
model.backward<-lm(y~X2+X5+X6,data = HW3data)
step(Hw3null,scope=list(lower=Hw3null,upper=Hw3full),
     direction = "forward",scale = summary(Hw3full)$sigma^2,trace = TRUE)
model.foward<-lm(y~X6+X5+X2,data = HW3data)
step(Hw3full,scope = list(lower=Hw3null,upper=Hw3full),
     direction = "both",scale=summary(Hw3full)$sigma^2,trace = TRUE)
model.both<-lm(y~X2+X5+X6,data = HW3data)


##l

##m
round(cor(HW3data$y,fitted(Hw3full)),3)


##o

##p
###改full and reduce
fullF<-lm(y~.,data = HW3data)
reducedF<-lm(y~X2+X3+X6,data = HW3data)
SSR<-deviance(reducedF)-deviance(fullF)
F_sta4p<-(SSR/(reducedF$df.residual-fullF$df.residual))/(deviance(fullF)/(fullF$df.residual));F_sta4p

permcol(X=HW3data,y=6,Permute = c(3,4))
