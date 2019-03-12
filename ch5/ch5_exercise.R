#5
#(a)
library(ISLR)
set.seed(1)
dim(Default)
glm.fit=glm(default~income+balance,data=Default,family=binomial)
summary(glm.fit)

#(b)
attach(Default)
vaset=function(){
  train=sample(10000,5000)
  glm.fit=glm(default~income+balance,data=Default,family=binomial,subset=train)
  glm.prob=predict(glm.fit,Default[-train,],type="response")
  glm.pred=rep("No",5000)
  glm.pred[glm.prob>0.5]="Yes"
  return(mean(glm.pred!=Default$default[-train]))
}
print(vaset())
#2.86% validation set error

#(c)
print(vaset())
print(vaset())
print(vaset())

#(d)
train=sample(10000,5000)
glm.fit=glm(default~income+balance+student,data=Default,family=binomial,subset=train)
glm.prob=predict(glm.fit,Default[-train,],type="response")
glm.pred=rep("No",5000)
glm.pred[glm.prob>0.5]="Yes"
mean(glm.pred!=Default$default[-train])
#2.64% validation set error
#it seems to not lead to reduction in the test error rate.

#5
#(a)
set.seed(1)
glm.fit=glm(default~income+balance,data=Default,family=binomial)
summary(glm.fit)

#(b)
boot.fn=function(data,index){
  glm.fit=glm(default~income+balance,data=data,family=binomial,subset=index)
  return(coef(glm.fit))
}

#(c)
library(boot)
boot(Default,boot.fn,100)

#(d)
#similar

#7
#(a)
attach(Weekly)
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.fit)

#(b)
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial)
summary(glm.fit)

#(c)
predict(glm.fit,Weekly[1,],type="response")>0.5

#(d)
error=rep(0,dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]){
  glm.fit=glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial)
  glm.probs=predict(glm.fit,Weekly[i,],type="response")>0.5
  true_probs=Weekly[i,]$Direction=="Up"
  if (true_probs!=glm.probs)
    error[i]=1
}

#(e)
mean(error)

#8
#(a)
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
#n is 100, p is 2

#(b)
plot(x,y)
#The plot is like turn-over U shape.

#(c)
library(boot)
set.seed(1)
df=data.frame(x,y)
glm.fit=glm(y~x,data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2),data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2)+I(x^3),data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2)+I(x^3)+I(x^4),data=df)
cv.glm(df,glm.fit)$delta

#(d)
set.seed(10)
glm.fit=glm(y~x,data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2),data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2)+I(x^3),data=df)
cv.glm(df,glm.fit)$delta

glm.fit=glm(y~x+I(x^2)+I(x^3)+I(x^4),data=df)
cv.glm(df,glm.fit)$delta

#The results are the same because LOOCV only leave one observation out.

#(e)
#iii has the smallest error. This meets the shape of plot.

#(f)
summary(glm.fit)
#linear and quadratic predictors are all statistically significant. The results agrees with the conclusion on LOOCV.
#Even though ii did not yield the smallest error, the error is close to the error of iii. 
#And the reduce of bias is in the cost of the rise in variance. If the error is close, we should choose simpler model.

#9
#(a)
library(MASS)
mu=mean(Boston$medv)
mu

#(b)
se=sd(Boston$medv)/sqrt(length(Boston$medv))
se

#(c)
library(boot)
boot.fn=function(data,index){
  return(mean(data[index]))
}
bost=boot(Boston$medv,boot.fn,1000)

#(d)
c(bost$t0-2*se,bost$t0+2*se)
t.test(Boston$medv)

#(e)
mumed=median(Boston$medv)
mumed

#(f)
boot.fn=function(data,index){
  return(median(data[index]))
}
boot(Boston$medv,boot.fn,1000)

#(g)
quantile(Boston$medv,probs=0.1)

#(h)
boot.fn=function(data,index){
  return(quantile(data[index],probs=0.1))
}
boot(Boston$medv,boot.fn,1000)


