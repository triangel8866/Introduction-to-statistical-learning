#6
#(a)
#CV
library(ISLR)
library(boot)
Wage=na.omit(Wage)
attach(Wage)
set.seed(1)
cv.errors=rep(NA,10)
for (i in 1:10){
  poly.fit=glm(wage~poly(age,i),data=Wage)
  cv.errors[i]=cv.glm(Wage,poly.fit,K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)
#ANOVA
fit.1=glm(wage~poly(age,1),data=Wage)
fit.2=glm(wage~poly(age,2),data=Wage)
fit.3=glm(wage~poly(age,3),data=Wage)
fit.4=glm(wage~poly(age,4),data=Wage)
fit.5=glm(wage~poly(age,5),data=Wage)
fit.6=glm(wage~poly(age,6),data=Wage)
fit.7=glm(wage~poly(age,7),data=Wage)
fit.8=glm(wage~poly(age,8),data=Wage)
fit.9=glm(wage~poly(age,9),data=Wage)
fit.10=glm(wage~poly(age,10),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10,test="F")

#plot
xlims=range(age)
age.grid=seq(from=xlims[1],to=xlims[2])
pred=predict(fit.4,newdata=list(age=age.grid),se=T)
se.bands=cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)
plot(age,wage,cex=.5,col="darkgrey")
lines(age.grid,pred,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col='blue',lty=3)

#(b)
data(Wage)
cv.errors=rep(NA,9)
for (i in 2:10){
  Wage$cutage=cut(age,i)
  fit.cut=glm(wage~cutage,data=Wage)
  cv.errors[i]=cv.glm(Wage,fit.cut,K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)
lm.fit=glm(wage~cut(age,8),data=Wage)
preds=predict(lm.fit,newdata=data.frame(age=age.grid),se=T)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,cex=.5,col="darkgrey",ylim=)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty="dashed")

#7
summary(Wage$maritl)
summary(Wage$jobclass)
#lm
fit.lm=lm(wage~poly(age,4)+maritl+jobclass,data=Wage)
summary(fit.lm)
deviance(fit.lm)
#gam
library(gam)
library(akima)
fit=gam(wage~maritl+jobclass+s(age,4),data=Wage)
summary(fit)
deviance(fit)
plot(fit,se=T)

#8
pairs(Auto)
Auto=na.omit(Auto)
attach(Auto)
#mpg with displacement,horsepower and weight
#polynomial
cv.errors=rep(NA,10)
fits=list()
for (i in 1:10){
  fits[[i]]=glm(mpg~poly(displacement,i),data=Auto)
  cv.errors[i]=cv.glm(Auto,fits[[i]],K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)
anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]],fits[[5]],fits[[6]],fits[[7]],fits[[8]],fits[[9]],fits[[10]],test="F")
#quadratic is enough.
fit.2=glm(mpg~poly(displacement,2),data=Auto)
grid.dis=seq(from=range(displacement)[1],to=range(displacement)[2])
plot(displacement,mpg,col="darkgrey",cex=.5)
preds=predict(fit.2,newdata=list(displacement=grid.dis),se=T)
lines(grid.dis,preds$fit,col="blue",lwd=2)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
matlines(grid.dis,se.bands,col="blue",lwd=1,lty=3)

#splines
library(splines)
cv.errors=rep(NA,10)
for (i in 2:10){
  sp.lm=glm(mpg~ns(displacement,df=i),data=Auto)
  cv.errors[i]=cv.glm(Auto,sp.lm,K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)

#GAM
library(gam)
fit.gam=gam(mpg~ns(displacement,4)+ns(horsepower,4)+ns(weight,4),data=Auto)
summary(fit.gam)
deviance(fit.gam)

#9
#(a)
library(MASS)
attach(Boston)
poly.fit=lm(nox~poly(dis,3),data=Boston)
summary(poly.fit)
dis.grid=seq(range(dis)[1],range(dis)[2])
preds=predict(poly.fit,newdata=list(dis=dis.grid),se=T)
plot(dis,nox,cex=.5,col="darkgrey")
lines(dis.grid,preds$fit,col="blue",lwd=2)

#(b)
rss=rep(NA,10)
for (i in 1:10){
  fit=lm(nox~poly(dis,i),data=Boston)
  summary.fit=summary(fit)
  rss[i]=sum(summary.fit$residuals^2)
}
plot(rss)

#(c)
library(boot)
cv.errors=rep(NA,10)
for (i in 1:10){
  fit=glm(nox~poly(dis,i),data=Boston)
  cv.errors[i]=cv.glm(Boston,fit,K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)

#(d)
library(splines)
spline.reg=glm(nox~bs(dis,df=4),data=Boston)
summary(spline.reg)
preds=predict(spline.reg,newdata=list(dis=dis.grid))
plot(dis,nox,cex=.8,col="darkgrey")
lines(dis.grid,preds,col="red")

#(e)
cv.erros=rep(NA,13)
for (i in 3:13){
  spline.fit=glm(nox~bs(dis,df=i),data=Boston)
  cv.errors[i]=sum(summary(spline.fit)$residuals^2)
}
plot(cv.errors,type="b")
which.min(cv.errors)

#(f)
cv.errors=rep(NA,13)
for (i in 3:13){
  spline.fit=glm(nox~bs(dis,df=i),data=Boston)
  cv.errors[i]=cv.glm(Boston,spline.fit,K=10)$delta[1]
}
plot(cv.errors,type="b")
which.min(cv.errors)

#10
#(a)
library(leaps)
library(ISLR)
train=sample(c(TRUE,FALSE),nrow(College),replace=T)
test=(!train)
best.fit=regsubsets(Outstate~.,data=College[train,],nvmax=17,method="forward")
best.summary=summary(best.fit)
which.min(best.summary$bic)
which.min(best.summary$cp)
which.max(best.summary$adjr2)
par(mfrow=c(1,3))
plot(best.summary$bic,type="l")
#one standard error rule
abline(h=min(best.summary$bic)+sd(best.summary$bic),col="red",lty=2)
plot(best.summary$cp,type="l")
abline(h=min(best.summary$cp)+sd(best.summary$cp),col="red",lty=2)
plot(best.summary$adjr2,type="l")
abline(h=max(best.summary$adjr2)-sd(best.summary$adjr2),col="red",lty=2)
#choose 4 predictors
coefi=coef(best.fit,id=4)
names(coefi)

#(b)
library(gam)
gam.fit=gam(Outstate~Private+s(Room.Board,df=4)+s(perc.alumni,df=4)+s(Expend,df=4),data=College[train,])
par(mfrow=c(2,2))
plot(gam.fit,se=T,col="blue")

#(c)
preds=predict(gam.fit,newdata=data.frame(College[test,]))
error=mean((College$Outstate[test]-preds)^2)
rss=1-error/mean((College$Outstate[test]-mean(College$Outstate[test]))^2)
rss              
#OLS
summary(lm(Outstate~Private+Room.Board+perc.alumni+Expend,data=College[train,]))
#R-square of gam is similar to OLS

#(d)
summary(gam.fit)
#Expend has strong evidence of non-linear.

#11
#(a)
set.seed(1)
x1=rnorm(100)
x2=rnorm(100)
eps=rnorm(100,sd=0.1)
y=0.4*x1+0.6*x2+eps

#(b)
beta1=0.5

#(c)
a=y-beta1*x2
beta2=lm(a~x2)$coef[2]

#(d)
a=y-beta2*x2
beta1=lm(a~x1)$coef[2]

#(e)
beta0=rep(NA,1000)
beta1=rep(NA,1000)
beta2=rep(NA,1000)
beta1[1]=0.5
for (i in 1:1000){
  a=y-beta1[i]*x1
  beta2[i]=lm(a~x2)$coef[2]
  a=y-beta2[i]*x2
  if (i<1000){
    beta1[i+1]=lm(a~x1)$coef[2]
  }
  beta0[i]=lm(a~x1)$coef[1]
}

par(mfrow=c(1,1))
plot(beta0,type="l",col="blue",ylim=c(-1,1))
lines(beta1,type="l",col="red")
lines(beta2,type="l",col="green")

#(f)
lm.fit=lm(y~x1+x2)
abline(h=lm.fit$coefficients[1],lty=3)
abline(h=lm.fit$coefficients[2],lty=3)
abline(h=lm.fit$coefficients[3],lty=3)
legend("bottom",c("beta0","beta1","beta2","lm fit"),lty=c(1,1,1,3),col=c("blue","red","green","black"))

#(g)
#one iteration is enough to obtain a "good" approximation

#12
set.seed(1)
n=1000
p=100
x=matrix(nrow=n,ncol=p)
coefi=rep(NA,p)
for (i in 1:p){
  x[,i]=rnorm(n)
  coefi[i]=rnorm(1)
}
beta=rep(0,p)
errors=rep(NA,1000)
y=x %*% coefi+rnorm(n)
for (j in 1:1000){
  for (i in 1:p){
    a=y-x%*%beta+beta[i]*x[,i]
    beta[i]=lm(a~x[,i])$coef[2]
  }
  errors[j]=sum((y-x%*%beta)^2)
}
plot(1:11,errors[1:11])
#10 iteration is enough.

