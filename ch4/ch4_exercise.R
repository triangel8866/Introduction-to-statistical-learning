#10
#(a)
library(ISLR)
summary(Weekly)
dim(Weekly)
pairs(Weekly)
attach(Weekly)
plot(Volume)
#Volume increases over time.

#(b)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,family=binomial)
summary(glm.fit)
#Lag2 appears to be significant.

#(c)
glm.prob=predict(glm.fit,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.prob > 0.5]="Up"
table(glm.pred,Direction)
mean(glm.pred == Direction)

#(d)
set.seed(1)
train=(Year<2009)
week.09=Weekly[!train,]
glm.fit=glm(Direction~Lag2,family=binomial,data=Weekly,subset=train)
summary(glm.fit)
glm.prob=predict(glm.fit,week.09,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.prob > 0.5]="Up"
table(glm.pred,week.09$Direction)
mean(glm.pred == week.09$Direction)

#(e)
library(MASS)
Direction.09=Direction[!train]
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.pred=predict(lda.fit,week.09)$class
table(lda.pred,Direction.09)
mean(lda.pred==Direction.09)

#(f)
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.pred=predict(qda.fit,week.09)$class
table(qda.pred,Direction.09)
mean(qda.pred==Direction.09)

#(g)
library(class)
train.X=as.matrix(Lag2[train])
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.09)
mean(knn.pred==Direction.09)

#(h)
#lda and logistic provide the best results.

#(i)
library(MASS)
#Lag2 interacts with Lag1
lda.fit=lda(Direction~Lag1*Lag2,data=Weekly,subset=train)
lda.pred=predict(lda.fit,week.09)$class
mean(lda.pred==Direction.09)
#transformation of Lag2
lda.fit=lda(Direction~Lag1+I(Lag2^2),data=Weekly,subset=train)
lda.pred=predict(lda.fit,week.09)$class
mean(lda.pred==Direction.09)
#test error rate is the same as (e)

#k=3
knn.pred=knn(train.X,test.X,train.Direction,k=3)
mean(knn.pred==Direction.09)

#11
#(a)
summary(Auto)
attach(Auto)
mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
Auto.new=data.frame(mpg01,Auto)

#(b)
cor(Auto.new[,-10])
pairs(Auto.new[,-10])
attach(Auto.new)
boxplot(mpg01,cylinders)

#(c)
train=(year>75)
Auto.train=Auto.new[train,]
Auto.test=Auto.new[!train,]

#(d)
library(MASS)
lda.fit=lda(mpg01~cylinders+displacement+horsepower+weight,data=Auto.new,subset=train)
lda.pred=predict(lda.fit,Auto.test)$class
mean(lda.pred!=Auto.test$mpg01)
#16.7% test error rate

#(e)
library(class)
qda.fit=qda(mpg01~cylinders+displacement+horsepower+weight,data=Auto.new,subset=train)
qda.pred=predict(qda.fit,Auto.test)$class
mean(qda.pred!=Auto.test$mpg01)
#10.56% test error rate

#(f)
glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight,family=binomial,data=Auto.new,subset=train)
summary(glm.fit)
glm.prob=predict(glm.fit,Auto.test,type="response")
glm.pred=rep(0,180)
glm.pred[glm.prob>.5]=1
mean(glm.pred!=Auto.test$mpg01)
#16.11% test error rate

#(g)
library(class)
attach(Auto.new)
train.X=cbind(cylinders,displacement,horsepower,weight)[train,]
test.X=cbind(cylinders,displacement,horsepower,weight)[!train,]
train.Y=mpg01[train]
test.Y=mpg01[!train]
#k=1
knn.fit=knn(train.X,test.X,train.Y,k=1)
mean(knn.fit!=test.Y)
#16.67% test error rate 

#k=3
knn.fit=knn(train.X,test.X,train.Y,k=3)
mean(knn.fit!=test.Y)
#14.44% test error rate

#k=10
knn.fit=knn(train.X,test.X,train.Y,k=10)
mean(knn.fit!=test.Y)
#15.56% test error rate
#k=3 seems to perform the best.

#12
#(a)
Power=function(){
  print(2^3)
}

#(b)
Power2=function(x,a){
  print(x^a)
}

#(c)
Power2(10,3)
Power2(8,17)
Power2(131,3)

#(d)
Power3=function(x,a){
  result=x^a
  return(result)
}

#(e)
x=1:10
plot(x,Power3(x,2),log="xy",xlab="log of x", ylab="log of x^2")

#(f)
PlotPower=function(x,a){
  plot(x,x^a)
}
PlotPower(1:10,3)

#13
#(a)
summary(Boston)
attach(Boston)
crim01=rep(0,length(crim))
crim01[crim>median(crim)]=1
Boston.new=data.frame(crim01,Boston)
cor(Boston.new)
train=1:round(length(crim01)*0.8)
test=(round(length(crim01)*0.8)+1):length(crim01)
Boston.train=Boston.new[train,]
Boston.test=Boston.new[test,]
crim.test=crim01[test]

#logistic
glm.fit=glm(crim01~.-crim-crim01,data=Boston.new,family=binomial,subset=train)
summary(glm.fit)
glm.prob=predict(glm.fit,Boston.test,type="response")
glm.pred=rep(0,length(glm.prob))
glm.pred[glm.pred >.5]=1
mean(glm.pred != crim.test)
#85.14% test error rate

#lda
library(MASS)
lda.fit=lda(crim01~.-crim-crim01,data=Boston.new,subset=train)
lda.pred=predict(lda.fit,Boston.test)$class
mean(lda.pred!=crim.test)
#12.87% test error rate

#qda
qda.fit=qda(crim01~.-crim-crim01,data=Boston.new,subset=train)
qda.pred=predict(qda.fit,Boston.test)$class
mean(qda.pred!=crim.test)
#5.9% test error rate

#knn
library(class)
train.X=as.matrix(Boston.train[c(-1,-2)])
test.X=as.matrix(Boston.test[c(-1,-2)])
train.Y=as.matrix(Boston.train[1])
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(knn.pred!=crim.test)
#7.9% test error rate

knn.pred=knn(train.X,test.X,train.Y,k=10)
mean(knn.pred!=crim.test)
#7.9% test error rate

knn.pred=knn(train.X,test.X,train.Y,k=100)
mean(knn.pred!=crim.test)
#9.9% test error rate