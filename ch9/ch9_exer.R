#1
x1=seq(from=-10,to=10)
x2=1+3*x1
plot(x2,x1,type="l")
lines(1-1/2*x1,x1,type="l",col="red")

#3
x1=c(3,2,4,1,2,4,4)
x2=c(4,2,4,4,1,3,1)
plot(x1,x2,col=c("red","red","red","red","blue","blue","blue"),xlab="x1",ylab="x2")
abline(-0.5,1)
abline(-1,1,lty="dashed")
abline(0,1,lty="dashed")

plot(x1,x2,col=c("red","red","red","red","blue","blue","blue"),xlab="x1",ylab="x2")
abline(-0.8,1)

points(2,3,col="blue")

#4
set.seed(100)
x1=rnorm(100)
x2=x1^2+x1+rnorm(100)
plot(x1,x2)
slice=sample(100,50)
x2[slice]=x2[slice]+3
x2[-slice]=x2[-slice]-3
plot(x1[slice],x2[slice],col="blue",ylim=c(-6,12))
points(x1[-slice],x2[-slice],col="red")
y=rep(0,100)
y[slice]=1
y[-slice]=-1
dat=data.frame(x1=x1,x2=x2,y=as.factor(y))
train=sample(100,50)
#linear kernel
library(e1071)
set.seed(100)
svm.lm=svm(y~.,data=dat[train,],kernel="linear",cost=10)
plot(svm.lm,dat[train,])
table(predict(svm.lm,newdata=dat[train,]),dat$y[train])
table(predict(svm.lm,newdata=dat[-train,]),dat$y[-train])
#2 training errors
#2 classification errors

#polynomial kernel
svm.poly=svm(y~.,data=dat[train,],kernel="polynomial",cost=10)
plot(svm.poly,dat[train,])
table(predict(svm.poly,newdata=dat[train,]),dat$y[train])
table(predict(svm.poly,newdata=dat[-train,]),dat$y[-train])
#2 training errors
#3 test classification errors

#radial kernel
svm.rad=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=10)
plot(svm.rad,dat[train,])
table(predict(svm.rad,newdata=dat[train,]),dat$y[train])
table(predict(svm.rad,newdata=dat[-train,]),dat$y[-train])
#0 training errors 
#0 test classification errors
#radial kernel performs best on the test data.

#5
set.seed(100)
x1=runif(500)-0.5
x2=runif(500)-0.5
plot(x1,x2)
y=1*(x1^2-x2^2>0)

plot(x1,x2,col=(y+1))

dat=data.frame(x1=x1,x2=x2,y=as.factor(y))
logit=glm(y~.,data=dat,family=binomial)
summary(logit)

prob=predict(logit,dat,type="response")
pred=ifelse(prob>0.5,1,0)
plot(x1,x2,col=(pred+1))

logit=glm(y~I(x1^2)+I(x2^2)+I(x1*x2),data=dat,family=binomial)
summary(logit)

#6
prob=predict(logit,dat,type="response")
pred=ifelse(prob>0.5,1,0)
plot(x1,x2,col=(pred+1))

library(e1071)
svm.lm=svm(y~.,data=dat,kernel="linear",cost=10)
plot(svm.lm,dat)
summary(svm.lm)
pred=predict(svm.lm,newdata=dat)
plot(x1,x2,col=(as.numeric(pred)+1))

svm.rad=svm(y~.,data=dat,kernel="radial",gamma=1,cost=10)
plot(svm.rad,dat)
pred=predict(svm.rad,newdata=dat)
plot(x1,x2,col=as.numeric(pred)+1)

#7
set.seed(222)
x1.red=runif(500,0,10)
x2.red=runif(500,(x1.red+1),11)
x1.blue=runif(500,0,10)
x2.blue=runif(500,-1,(x1.blue-1))
plot(x1.red,x2.red,col="red",xlab="x1",ylab="x2")
points(x1.blue,x2.blue,col="blue")
x1.noise.red=runif(50,0,10)
x1.noise.blue=runif(50,0,10)
x2.noise.red=x1.noise.red
x2.noise.blue=x1.noise.blue
points(x1.noise.red,x2.noise.red,col="red")
points(x1.noise.blue,x2.noise.blue,col="blue")

y=rep(0,1100)
y[1:550]=1
dat=data.frame(x1=c(x1.red,x1.noise.red,x1.blue,x1.noise.blue),
               x2=c(x2.red,x2.noise.red,x2.blue,x2.noise.blue),y=y)
tune.out=tune(svm,as.factor(y)~.,data=dat,kernel="linear",ranges=list(cost=c(0.01,1,10,100,1000)))
summary(tune.out)
data.frame(cost=tune.out$performances$cost,error=tune.out$performances$error*1100)
#cost=1000 yields best performance

test=c(sample(1:550,50),sample(551:1100,50))
cost=c(0.001,0.01,1,10,100,1000)
test.errors=rep(NA,length(cost))
for (i in 1:length(cost)){
  svm.fit=svm(as.factor(y)~.,data=dat,kernel="linear",cost=cost[i])
  pred=predict(svm.fit,dat[test,])
  test.errors[i]=sum(pred!=dat$y[test])
  
}
data.frame(cost=cost,test.error=test.errors)
#cost=1 yields best performance

#7
library(ISLR)
library(e1071)
Auto=na.omit(Auto)
mpg.ind=rep(0,nrow(Auto))
mpg.ind[Auto$mpg>median(Auto$mpg)]=1

Auto=data.frame(Auto,mpg.ind=as.factor(mpg.ind))
cost=c(0.01,0.1,1,5,10,100)
tune.out=tune(svm,mpg.ind~.,data=Auto,kernel="linear",ranges=list(cost=cost))
summary(tune.out)
data.frame(cost=cost,error=tune.out$performances$error)

set.seed(123)
tune.poly=tune(svm,mpg.ind~.,data=Auto,kernel="polynomial",ranges=list(cost=cost,degree=c(2,3,4)))
summary(tune.poly)

tune.radial=tune(svm,mpg.ind~.,data=Auto,kernel="radial",ranges=list(cost=cost,gamma=c(0.01,0.1,1,10)))
summary(tune.radial)

svm.lm=svm(mpg.ind~.,data=Auto,kernel="linear",cost=1)
plot(svm.lm,Auto,mpg~horsepower)

svm.poly=svm(mpg.ind~.,data=Auto,kernel="polynomial",cost=100,degree=2)
plot(svm.poly,Auto,mpg~horsepower)

svm.radial=svm(mpg.ind~.,data=Auto,kernel="radial",cost=100,gamma=0.01)
plot(svm.radial,Auto,mpg~horsepower)

#8
library(ISLR)
library(e1071)
oj=na.omit(OJ)
train=sample(nrow(oj),800)
oj.train=oj[train,]
oj.test=oj[-train,]

svm.lm=svm(Purchase~.,data=oj.train,kernel="linear",cost=0.01)
summary(svm.lm)

train.error=mean(predict(svm.lm,oj.train)!=oj.train$Purchase)
test.error=mean(predict(svm.lm,oj.test)!=oj.test$Purchase)

#linear
set.seed(123)
cost=c(0.01,0.1,1,5,10)
tune.out=tune(svm,Purchase~.,data=oj.train,kernel="linear",ranges=list(cost=cost))
summary(tune.out)

svm.lm=svm(Purchase~.,data=oj.train,kernel="linear",cost=tune.out$best.parameters$cost)
error.train=mean(predict(svm.lm,oj.train)!=oj.train$Purchase)
error.test=mean(predict(svm.lm,oj.test)!=oj.test$Purchase)

#radial
tune.out=tune(svm,Purchase~.,data=oj.train,kernel="radial",ranges=list(cost=cost))
svm.rad=svm(Purchase~.,data=oj.train,kernel="radial",cost=tune.out$best.parameters$cost)
train.error=mean(predict(svm.rad,oj.train)!=oj.train$Purchase)
test.error=mean(predict(svm.rad,oj.test)!=oj.test$Purchase)

#polynomial
tune.out=tune(svm,Purchase~.,data=oj.train,kernel="polynomial",ranges=list(cost=cost),degree=2)
svm.poly=svm(Purchase~.,data=oj.train,kernel="polynomial",cost=tune.out$best.parameters$cost,degree=2)
train.error=mean(predict(svm.poly,oj.train)!=oj.train$Purchase)
test.error=mean(predict(svm.poly,oj.test)!=oj.test$Purchase)

#linear kernel is better.