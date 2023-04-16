library(leaps)
library(faraway)
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



#Q3
##a
DataA<-data.frame(V1=c(1,1,0),V2=c(2,0,0),V3=c(3,1,1))
A<-cbind(DataA$V1,DataA$V2,DataA$V3)
t(A)
t(A)%*%A


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
cov(t(V))
Iden<-diag(200)
H1<-matrix(1,200,1)
H_MTX1<-H1%*%solve(t(H1)%*%H1)%*%t(H1)#This is the actual h matrix
(V%*%(Iden-H_MTX1)%*%t(V))/(200-1)





##Q4
###a
v<-c(-4,1,3,0,0,-1,-1)

###b
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

##d
mean(eps)
var(eps)
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
sort(summary(Hw3regs)$rss)

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
summary(model.backward)
step(Hw3null,scope=list(lower=Hw3null,upper=Hw3full),
     direction = "forward",scale = summary(Hw3full)$sigma^2,trace = TRUE)
model.foward<-lm(y~X6+X5+X2,data = HW3data)
summary(model.foward)
step(Hw3full,scope = list(lower=Hw3null,upper=Hw3full),
     direction = "both",scale=summary(Hw3full)$sigma^2,trace = TRUE)
model.both<-lm(y~X2+X5+X6,data = HW3data)
summary(model.both)



##l
F1<-lm(y~.,data = HW3data)
XF1<-model.matrix(F1);p<-ncol(XF1);n<-nrow(XF1)
F2<-lm(y~X2+X5+X6,data = HW3data)
XF2<-model.matrix(F2);q<-ncol(XF2);n<-nrow(XF2)
SSEp<-deviance(F1);SSEq<-deviance(F2)
SSE1<-deviance(Hw3null)
Cp<-SSEq/(SSEp/(n-p))-n+2*q;Cp
R_sq<-(SSE1-SSEq)/SSE1;R_sq
R_sq_adj<-(SSE1/(n-1)-SSEq/(n-q))/(SSE1/(n-1));R_sq_adj

##m
round(cor(HW3data$y,fitted(Hw3full)),3)

##n(?)
beta_hat<-solve(t(X)%*%X)%*%t(X)%*%y
L<-solve((t(X)%*%X))
Sigma_beta_hat<-deviance(Hw3full)/(n-p)*solve(t(X)%*%X)
sd<-sqrt(Sigma_beta_hat[2,2]);sd
t<-as.vector(beta_hat)/sqrt(diag(Sigma_beta_hat))
t
summary(Hw3full)

##o
t_sta<-qt(.975,n-p)
lower<-1.02-t_sta*0.2843;lower
upper<-1.02+t_sta*0.2843;upper
confint(F1)

##p
F_sta4p<-((SSEq-SSEp)/(p-q))/(SSEp/(n-p));F_sta4p
permcol(X=HW3data,y=6,Permute = c(2,3))


##Q8

##Cp Function
function (cp,...)
{
  p <- max(cp$size)
  i <- (cp$Cp < (p + 1.5))
  plot(cp$size[i], cp$Cp[i], xlab = "p", ylab = "Cp",
       type = "n",...)
  labels <- apply(cp$which, 1, function(x)
    paste(as.character((1:(p - 1))[x]), collapse = ""))
  text(cp$size[i], cp$Cp[i], labels[i],...)
  abline(0, 1)
}
data(sat)

###a
head(sat)


##b
sat2<-sat[,c(1:4,7)]

##c
sat2full<-lm(total~.,data = sat2)
summary(sat2full)
sat2null<-lm(total~1,data = sat2)
summary(sat2null)


##d
leaps(sat2[,c(1:4)],sat2[,5],nbest = 3,method = "Cp")

##e
leap.ajdr2<-leaps(sat2[,c(1:4)],sat2[,5],nbest = 3,method = "adjr2")
maxadjr(leap.ajdr2,best=6)


##f
satregs<-regsubsets(total~.,data = sat2,nbest = 3)
plot(satregs,scale = "Cp")
plot(satregs,scale = "adjr2")
plot(satregs,scale = "r2")


##g
subsets(satregs, statistic="cp",ylim=c(0,8),legend=FALSE)
abline(1,1)#only for Cp
subsets(satregs, statistic="adjr2",legend=FALSE,ylim=c(0.78,0.82))
subsets(satregs, statistic="rsq",legend=FALSE,ylim=c(0.79,0.82))
subsets(satregs, statistic="rss",legend=FALSE,ylim=c(48000,59000))
###expected value of cp is p or q,abline tells us what expected cp would be if the model is correct
###correct models should be near the line 

##h
step(sat2full,direction = "backward",
     scale=summary(sat2full)$sigma^2,trace = TRUE)
model.sat2.back<-lm(total~ratio+salary+takers,data = sat2)
summary(model.sat2.back)

step(sat2null,scope=list(lower=sat2null,upper=sat2full),
     direction = "forward",scale=summary(sat2full)$sigma^2,trace = TRUE)
model.sat2.forward<-lm(total~takers+expend,data = sat2)
summary(model.sat2.forward)

step(sat2full,scope = list(lower=sat2null,upper=sat2full),
     direction = "both",scale=summary(sat2full)$sigma^2,trace = TRUE)
model.sat2.both<-lm(total~ratio+salary+takers,data = sat2)
summary(model.sat2.both)

##i
XSAT2.full<-model.matrix(sat2full);p.sat<-ncol(XSAT2.full);n.sat<-nrow(XSAT2.full)
XSAT2.reduce<-model.matrix(model.sat2.forward);q.sat<-ncol(XSAT2.reduce);n.sat<-nrow(XSAT2.reduce)
SSEp.sat<-deviance(sat2full);SSEq.sat<-deviance(model.sat2.forward)
SSE1.sat<-deviance(sat2null)
Cp.sat<-SSEq.sat/(SSEp.sat/(n.sat-p.sat))-n.sat+2*q.sat;Cp.sat
R_sq.sat<-(SSE1.sat-SSEq.sat)/SSE1.sat;R_sq.sat
R_sq_adj.sat<-(SSE1.sat/(n.sat-1)-SSEq.sat/(n.sat-q.sat))/(SSE1.sat/(n.sat-1));R_sq_adj.sat
summary(model.sat2.forward)

#Q9
data(prostate)

##a
head(prostate)
##b
prostatefull<-lm(lpsa~.,data = prostate)
summary(prostatefull)

##c
X<-model.matrix(prostatefull)
X<-X[,]


##d(need compute)
summary(prostatefull)
X.prostate<-model.matrix(prostatefull)
n.prost<-nrow(X.prostate);p.prost<-ncol(X.prostate)
t_prostate<-qt(0.975,n.prost-p.prost)
prost.lower<-(-0.02)-t_prostate*0.011173;prost.lower
prost.upper<-(-0.02)+t_prostate*0.011173;prost.upper

confint(prostatefull,level = 0.95)#almost identical

##e
x0<-c(1, 1.44692, 3.62301, 65, .30010, 0, -0.79851, 7, 15)
new<-data.frame(lcavol=1.44692,lweight=3.62301,
                age=65,lbph=.30010, svi=0,lcp=-0.79851,gleason=7,
                pgg45=15)

EstX0<-t(x0)%*%coef(prostatefull)
StdX0<-sqrt((deviance(prostatefull)/(n.prost-p.prost))*t(x0)%*%solve(t(X.prostate)%*%X.prostate)%*%x0)
x0.lower<-EstX0-t_prostate*StdX0;x0.lower
x0.upper<-EstX0+t_prostate*StdX0;x0.upper
predict(prostatefull,newdata=new,level=.95,
        interval="confidence")

##f(missing)
x.new<-c(1, 1.44692, 3.62301, 20, .30010, 0, -0.79851, 7, 15)
new2<-data.frame(lcavol=1.44692,lweight=3.62301,
                  age=20,lbph=.30010, svi=0,lcp=-0.79851,gleason=7,
                  pgg45=15)
EstX.new<-t(x.new)%*%coef(prostatefull)
StdX.new<-sqrt((deviance(prostatefull)/(n.prost-p.prost))*t(x.new)%*%solve(t(X.prostate)%*%X.prostate)%*%x.new)
EstX.new.lower<-EstX.new-t_prostate*StdX.new;EstX.new.lower
EstX.new.upper<-EstX.new+t_prostate*StdX.new;EstX.new.upper
predict(prostatefull,newdata=new2,level=.95,
        interval="confidence")
#Q10
##a
data("teengamb")
head(teengamb)

##b
gamble.full<-lm(gamble~.,data = teengamb)
summary(gamble.full)
gamble.null<-lm(gamble~1,data = teengamb)
summary(gamble.null)
