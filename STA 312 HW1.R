install.packages("faraway")
library(faraway)


### Q2
Q1data<-data.frame(y=c(10,19,10,22,19),x1=1:5,x2=c(2,5,1,4,3))
y<-Q1data$y
X<-cbind(1,Q1data$x1,Q1data$x2)

Q1full<-lm(y~.,data = Q1data)

Q1full$coefficients
allBeta<-coef(Q1full)
##a
beta1<-allBeta[1]

##b

X_t<-t(X)
X%*%X_t
betas_calc<-solve(X_t%*%X)%*%X_t%*%y
betas_calc
summary(Q1full)

##c
y_hat<-X%*%betas_calc
y_hat
fitted(Q1full)
ep_hat<-y-y_hat
ep_hat
residuals(Q1full)

##d
delta_beta<-t(y-y_hat)%*%(y-y_hat)
deviance(Q1full)
delta_beta

##e
del<-c(0.01,-0.01,0.02)
beta_star<-coef(Q1full)+del
delta_beta_star<-t(y-X%*%beta_star)%*%(y-X%*%beta_star)
delta_diff<-delta_beta_star-delta_beta
delta_diff

##f
variance<-delta_beta/(5-3)
stdv<-sqrt(variance)
stdv
summary(Q1full)
##g
cov_beta<-as.numeric(variance)*solve(X_t%*%X)

summary(Q1full)

intercept_cov<-1.7335^2
beta1_cov<-0.4066^2
beta2_cov<-0.4066^2
vcov(Q1full)
intercept_cov
beta1_cov
beta2_cov
##h
Q1small<-lm(y~x1,data = Q1data)

summary(Q1small)

##i
deviance(Q1small)

##j
Identity<-diag(5)
one<-matrix(1,5,1)
H_matrix<-one%*%solve(t(one)%*%one)%*%t(one)
matrix_i<-(Identity-H_matrix)%*%X
round(matrix_i,digits = 3)
#This measures the how far an entry from the mean of the entries in a column 

##k
round(sum(Q1full$residuals))
round(sum(Q1small$residuals))

Q1small0<-lm(y~x1-1,data = Q1data)

sum(Q1small0$residuals)

#The sum of the residuals is approximately equal to 8.81, which is non-zero

##l
avgY<-mean(y)
avg.Q1.Full<-mean(Q1full$fitted.values)
avg.Q1small<-mean(Q1small$fitted.values)
avg.Q1small0<-mean(Q1small0$fitted.values)
avgY
avg.Q1.Full
avg.Q1small
avg.Q1small0
##m
q1.intercept<-lm(y~1,data = Q1data)
R.sq.Full<-(deviance(q1.intercept)-deviance(Q1full))/(deviance(q1.intercept))
R.sq.Small<-(deviance(q1.intercept)-deviance(Q1small))/(deviance(q1.intercept))
R.sq.Full
R.sq.Small
summary(Q1full)
summary(Q1small)


### 3
beta<-cbind(c(1,2,3))

##a
set.seed(2264);ans<-NULL
for(i in 1:1000){
  eps<-sample(c(-1,0,1),5,replace = TRUE)## manually create different epsilon and sample it each
  y2<-X%*%beta+eps;ylm<-lm(y2~x1+x2,data = Q1data)
  ans<-rbind(ans,coef(ylm))
}

head(round(ans,3))

##b


##c
View(ans)
apply(ans, 2, mean)


ans.mean<-apply(ans,2,mean)


##d
par(mfrow=c(1,3));for(i in 1:3)
  hist(ans[,i],xlim=c(-1.6,3.7),main=i)

##e (first part is on written part)

##f
mtx3<-(2/3)*solve((t(X)%*%X))
diag(mtx3)
apply(ans,2,var)


#9
data("teengamb")
head(teengamb)
##a
teengamblm<-lm(gamble~sex+status+income+verbal,data = teengamb)
summary(teengamblm)
##b
teengamblm$residuals
max(teengamblm$residuals)## The observation with largetst positive residual is the 24th observation
##c
round(sum(teengamblm$residuals),6)
##d
teengamblm1<-lm(gamble~1,data = teengamb)
SSE<-deviance(teengamblm)
SSE1<-deviance(teengamblm1)
R.sqQ9<-(SSE1-SSE)/SSE1
R.sqQ9
##E
teengamblme<-lm(residuals(teengamblm)~.-gamble,data=teengamb)

round(coef(teengamblme)) ## The new vector beta is a vector of zeros. This will hold in general since th sum of the entries in the residual vector is zero.