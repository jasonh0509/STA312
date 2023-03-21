install.packages("faraway")

library(faraway)
library(ggplot2)
##Permcol
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

##Permcol table
permcol_table<-function (X,y,PermuteL=(1:ncol(X))[-y],n=1000)
{
  X2<-X;y2<-y
  if (is.null(colnames(X2))){cn<-1:ncol(X)} else {cn<-colnames(X2)}
  cat("\n")
  cat("===========\n")
  cat(noquote(paste(c("Response (y): ",paste(cn[y],collapse=", "),"\n"),collapse="")))
  cat(noquote(paste(c("All: ",paste(cn,collapse=", "),"\n"),collapse="")))
  cat(noquote(paste(c("Permute: ",paste(cn[PermuteL],collapse=", "),"\n"),collapse="")))
  ans<-NULL
  y<-X2[,y]
  for (Permute in PermuteL){
    ansv<-NULL
    X<-X2[,-c(Permute,y2)];V<-X2[,Permute]
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
    ansv<-c(ansv,round(Dr,3))
    a<-length(D[D<Dr])+length(D[D==Dr])/2
    ansv<-c(ansv,c(n,a,a/n))
    Keep<-X;Permute<-V
    lm1<-lm(y~Keep)
    ylm<-lm(y~Keep+Permute)
    an<-anova(lm1,ylm)
    ansv<-c(ansv,round(an$Pr[2],6),round(an$F[2],3))
    ans<-rbind(ans,ansv)
  }
  colnames(ans)<-c("Deviance","N","smaller","prop","PrF","F")
  rownames(ans)<-cn[PermuteL]
  ans
}


DataX<-data.frame(V1=c(1,1,1),V2=c(4,2,6))

#Q3
X<-cbind(DataX$V1,DataX$V2)
X_t<-t(X)
X_t%*%X
H_mtx<-X%*%solve(X_t%*%X)%*%X_t
sum(diag(H_mtx))
h_eigen<-eigen(H_mtx)
E<-h_eigen$values
sum(h_eigen$values)
sum(diag(H_mtx))
h_eigen_vec<-h_eigen$vectors
h_eigen_vec
DIAG<-matrix(c(1,0,0,0,1,0,0,0,0),nrow = 3,ncol = 3)
h_eigen_vec%*%DIAG%*%t(h_eigen_vec)


#c
H<-X%*%solve(t(X)%*%X)%*%t(X)
one<-cbind(rep(1,3))
H1<-one%*%solve(t(one)%*%one)%*%t(one)
I<-diag(3)
A1<-I-H
A2<-I-H1
A3<-H-H1

lapply(eigen(A1),round,4)
lapply(eigen(A2),round,4)
lapply(eigen(A3),round,4)

eigenA1<-eigen(A1)
eigenA2<-eigen(A2)
eigenA3<-eigen(A3)


#Q4
##a
set.seed(2264)
M<-matrix(rnorm(10000),1000,10)
head(M)

##b
c<-apply(M,1,function(v){sum(v^2)})
head(c)
hist(c)

##c
###Based on the histogram of c, the common distribution of any entry c_i in c is chi-square distribution

##d
k1<-qchisq(0.025,df=10)
k2<-qchisq(1-0.025,df=10)

###Thus, k1= 3.25, k2=20.48

##e
c[c<k1]
c[c>k2]
num1<-length(c[c<k1])
num2<-length(c[c>k2])
proportion1<-num1/1000
proportion2<-num2/1000

###The proportion of values in c that are less than k1 is 0.023. The proportion of values in c that are less than k2 is 0.026

##f
u<-c[1:100]/c[501:600]
head(u)
hist(u)

##g
### The common distribution of any entry in u is F distribution

##h
rho1<-qf(0.1,df1=10,df2=10)
rho1
rho2<-qf(0.9,df1=10,df2 = 10)
rho2
## Thus, rho1 = 0.431,rho2 = 2.323

##i
u[u<rho1]
u[u>rho2]
num_less_rho1<-length(u[u<rho1])
num_less_rho1
num_more_rho2<-length(u[u>rho2])
num_more_rho2
prop_rho1<-num_less_rho1/100
prop_rho1
prop_rho2<-num_more_rho2/100
prop_rho2


#Q5
##a
beta<-c(10,1:5,rep(0,5))
set.seed(2644);epss<-rnorm(1000,mean=0,sd=2)
y_star<-cbind(1,M)%*%beta+epss
head(y_star)

##b
lm_ystar<-lm(y_star~M)
summary(lm_ystar)

###The t-values ranges from -1.903 to 76.28. The F-statistics is 1314. R_square is 0.93. The estimates of parameter from M1 to M10 ranges from -0.12331 to 4.91

##c
set.seed(2644)
epsd<-sample(c(-3,-1,-1,0,0,1,4),1000,replace = TRUE)
y_dag<-cbind(1,M)%*%beta+epsd


##d

##e
lm_ydag<-lm(y_dag~M)
summary(lm_ydag)

###The t value ranges from -1.528 to 75.55. The F-statistics is 1280 and the R_squared is 0.9283. We can see that the change of these metrics between model from (b) and (d) are minimal. 


#Q10
##a
data(prostate)
head(prostate)

lpsa_model<-lm(lpsa~.,data = prostate)
summary(lpsa_model)

permcol_table(X=prostate,y=9)

##Q11
###a
lm_no_above0.05<-lm(lpsa~lcavol+lweight+svi,data = prostate)

###b
summary(lpsa_model)
summary(lm_no_above0.05)
SSR<-deviance(lm_no_above0.05)-deviance(lpsa_model)
F_sta<-(SSR/(lm_no_above0.05$df.residual-lpsa_model$df.residual))/(deviance(lpsa_model)/(lpsa_model$df.residual))

anova(lm_no_above0.05,lpsa_model)
SSR<-deviance(lm_no_above0.05)-deviance(lpsa_model)
F_sta<-(SSR/(lm_no_above0.05$df.residual-lpsa_model$df.residual))/(deviance(lpsa_model)/(lpsa_model$df.residual))
prostate_sub<-prostate[,c(1,2,5,9)]
permcol_table(X=prostate_sub,y=4)
####Proportion of smaller deviance and given p value is identical for variable lcavol, but different from each other for variable lweight and svi.

#Q12
##a
data("teengamb")
gamble_model<-lm(gamble~.,data = teengamb)
summary(gamble_model)
###The variable sex and income have associated t statistics larger than qt(0.975,442)

##b
permcol_table(X=teengamb,y=5)
summary(gamble_model)

###Based on the output, the reasonableness of the p-value for testing variable status, income and verbal is verified.The proportion of the smaller deviance is almost identical to those from the model.

##c
income_only<-lm(gamble~income,data = teengamb)
deviance(income_only)
deviance(gamble_model)
SSR2<-deviance(income_only)-deviance(gamble_model)
F_sta2<-(SSR2/(income_only$df.residual-gamble_model$df.residual))/(deviance(gamble_model)/(gamble_model$df.residual))
F_sta2
teengamb_reduce<-teengamb[,c(3,5)]
permcol(X=teengamb,y=5,Permute = c(1,2,3))
summary(gamble_model)
##Based on the p-values in the output, the reasonableness is verified.


#Q13
##a
data(sat)
head(sat)
lm_sat<-lm(total~expend+ratio+salary,data = sat)
summary(lm_sat)

##b
summary(lm_sat)
###The t statistics is -1.878

sat2<-sat[,-c(4:6)]
permcol_table(X=sat2,y=4)

###Based on the output, the p-value associated with variable salary is reasonable.


##c
lm_sat_null<-lm(total~1,data = sat2)
SSE<-deviance(lm_sat)
SSE1<-deviance(lm_sat_null)
R_sq<-(SSE1-SSE)/SSE1;R_sq
Adj_rsq<-1-(((SSE)/(50-4))/(SSE1/(50-1)));Adj_rsq
summary(lm_sat)
##The values calculated are identical to the summary output.


##d
F_sta_Sat<-(SSR_sat/(lm_sat_null$df.residual-lm_sat$df.residual))/(deviance(lm_sat)/(lm_sat$df.residual));F_sta_Sat
permcol(X=sat,y=7,Permute = c(1,2,3))

###Based on the output, the p-values associated with variable expend, ratio and salary is reasonable

##e
sat3<-sat[,-c(5:6)]
lm_sat3<-lm(total~.,data=sat3)
summary(lm_sat3)

##f
###The t statistics is -12.559
permcol_table(X=sat3,y=5)
## The reasonableness of the p-value associated with variable takers is verified.

##g
lm_takers_only<-lm(total~takers,data = sat3)
SSR_taker_only<-deviance(lm_takers_only)-deviance(lm_sat3)
F_sta_taker<-(SSR_taker_only/((lm_takers_only$df.residual-lm_sat3$df.residual)))/(deviance(lm_sat3)/lm_sat3$df.residual);F_sta_taker
anova(lm_takers_only,lm_sat3)
permcol(X=sat3,y=5,Permute = c(1,2,3))

##Based on the output, the p-values associated with variable expend, ratio and salary are reasonable.Verified