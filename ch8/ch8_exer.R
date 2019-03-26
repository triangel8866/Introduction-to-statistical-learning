#2
pmk=seq(from=0,to=1,0.01)
G=(pmk*(1-pmk))*2
plot(pmk,G,type="l")

D=-(pmk*log(pmk))-(1-pmk)*log(1-pmk)
plot(pmk,D,type="l")

E=1-pmax(pmk,(1-pmk))
plot(pmk,E,type="l")

#6
library(randomForest)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
X.train=Boston[train,-14]
X.test=Boston[-train,-14]
Y.train=Boston[train,14]
Y.test=Boston[-train,14]
p=dim(Boston)[2]-1
p.2=p/2
p.sqrt=sqrt(p)
rf.boston=randomForest(x=X.train,y=Y.train,xtest=X.test,ytest=Y.test,mtry=p,ntree=500)
rf.boston.2=randomForest(x=X.train,y=Y.train,xtest=X.test,ytest=Y.test,mtry=p.2,ntree=500)
rf.boston.sq=randomForest(x=X.train,y=Y.train,xtest=X.test,ytest=Y.test,mtry=p.sqrt,ntree=500)
plot(1:500,rf.boston$mse,col="blue",xlab="number of trees",ylab="mse",type="l")
lines(1:500,rf.boston.2$mse,col="green",type="l")
lines(1:500,rf.boston.sq$mse,col="red",type="l")
legend("topright",c("p","p/2","sqrt(p)"),col=c("blue","green","red"),lty=1)

#8
library(tree)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

tree.car=tree(Sales~.-High,data=Carseats.train)
summary(tree.car)
plot(tree.car)
text(tree.car,pretty=0)
pred=predict(tree.car,newdata=Carseats.test)
mean((pred-Carseats.test[,"Sales"])^2)

cv.car=cv.tree(tree.car,FUN=prune.tree)
par(mfrow=c(1,2))
plot(cv.car$size,cv.car$dev,type="b")
plot(cv.car$k,cv.car$dev,type="b")

#best size=4
prune.car=prune.tree(tree.car,best=4)
par(mfrow=c(1,1))
plot(prune.car)
text(prune.car,pretty=0)
pred.prune=predict(prune.car,newdata=Carseats.test)
mean((pred.prune-Carseats.test$Sales)^2)

p=dim(Carseats)[2]-2
library(randomForest)
bag.car=randomForest(Sales~.-High,data=Carseats.train,mtry=p,ntree=500,importance=T)
importance(bag.car)
bag.pred=predict(bag.car,newdata=Carseats.test)
mean((bag.pred-Carseats.test$Sales)^2)

rf.car=randomForest(Sales~.-High,data=Carseats.train,mtry=sqrt(p),ntree=500,importance=T)
importance(rf.car)
pred.car=predict(rf.car,newdata=Carseats.test)
mean((pred.car-Carseats.test$Sales)^2)

#9
library(ISLR)
train=sample(1:nrow(OJ),800)
train.oj=OJ[train,]
test.oj=OJ[-train,]
tree.oj=tree(Purchase~.,data=train.oj)
summary(tree.oj)

plot(tree.oj)
text(tree.oj,pretty=0)

pred.tree=predict(tree.oj,newdata=test.oj,type="class")
table(pred.tree,test.oj$Purchase)
sum(pred.tree!=test.oj$Purchase)/length(pred.tree)

cv.oj=cv.tree(tree.oj,FUN=prune.misclass)
par(mfrow=c(1,2))
plot(cv.oj$size,cv.oj$dev,type="b")
plot(cv.oj$k,cv.oj$dev,type="b")
#best size=2

prune.oj=prune.tree(tree.oj,best=2)
summary(prune.oj)
#pruned tree is higher

pred.prune=predict(prune.oj,newdata=test.oj,type="class")
sum(pred.prune!=test.oj$Purchase)/length(pred.prune)
#pruned tree is higher

#10
library(ISLR)
Hitters=Hitters[-which(is.na(Hitters$Salary)),]
Hitters$Salary=log(Hitters$Salary)
head(Hitters)

Hitters.train=Hitters[1:200,]
Hitters.test=Hitters[-(1:200),]

library(gbm)
set.seed(1)
lambda=10^seq(-10,-0.2,length=100)
train.mse=rep(NA,100)
test.mse=rep(NA,100)
for (i in 1:100){
  boost.fit=gbm(Salary~.,data=Hitters.train,distribution="gaussian",n.trees=1000,shrinkage=lambda[i])
  train.pred=predict(boost.fit,Hitters.train,n.trees=1000)
  test.pred=predict(boost.fit,Hitters.test,n.trees=1000)
  train.mse[i]=mean((Hitters.train$Salary-train.pred)^2)
  test.mse[i]=mean((Hitters.test$Salary-test.pred)^2)
}
plot(lambda,train.mse,type='l')
lines(lambda,test.mse,col='blue',type="l")
min(test.mse)
lambda[which.min(test.mse)]

lm.fit=lm(Salary~.,data=Hitters.train)
lmpred=predict(lm.fit,Hitters.test)
mean((lmpred-Hitters.test$Salary)^2)

library(glmnet)
X=model.matrix(Salary~.,data=Hitters.train)
y=Hitters.train$Salary
X.test=model.matrix(Salary~.,data=Hitters.test)
lasso.fit=glmnet(X,y,alpha=1)
lasso.pred=predict(lasso.fit,X.test)
mean((lasso.pred-Hitters.test$Salary)^2)

boost.best=gbm(Salary~.,data=Hitters.train,distribution="gaussian",n.trees=1000,shrinkage=lambda[which.min(test.mse)])
summary(boost.best)

library(randomForest)
bag.Hitters=randomForest(Salary~.,data=Hitters.train,mtry=19,ntree=500)
bag.pred=predict(bag.Hitters,Hitters.test)
mean((bag.pred-Hitters.test$Salary)^2)

#12
library(gbm)
set.seed(1)
Caravan$Purchase=ifelse(Caravan$Purchase=="Yes",1,0)
Caravan.train=Caravan[1:1000,]
Caravan.test=Caravan[-(1:1000),]

boost.fit=gbm(Purchase~.,data=Caravan.train,distribution="bernoulli",n.trees=1000,shrinkage=0.01,verbose=F)
summary(boost.fit)
boost.prob=predict(boost.fit,newdata=Caravan.test,n.trees=1000,type="response")
boost.pred=ifelse(boost.prob>0.2,1,0)
table(boost.pred,Caravan.test$Purchase)
33/(33+256)

#knn
library(class)
train.X=Caravan.train[,-ncol(Caravan.train)]
train.y=Caravan.train[,ncol(Caravan.train)]
test.X=Caravan.test[,-ncol(Caravan.test)]
knn.pred=knn(train.X,test.X,train.y,k=3)
table(knn.pred,Caravan.test$Purchase)
8/(8+281)

#logistic
fit=glm(Purchase~.,data=Caravan.train,family=binomial)
lm.prob=predict(fit,Caravan.test,type="response")
lm.pred=ifelse(lm.prob>0.2,1,0)
table(Caravan.test$Purchase,lm.pred)
58/(350+58)

#12
set.seed(1)
head(Wage)

#boosting
library(gbm)
train=sample(nrow(Wage),nrow(Wage)/2)
train.wage=Wage[train,-11]
test.wage=Wage[-train,-11]

boost.wage=gbm(logwage~.,data=train.wage,distribution="gaussian",n.trees=1000,shrinkage=0.01)
pred.boost=predict(boost.wage,newdata=test.wage,n.trees=1000)
mean((test.wage$logwage-pred.boost)^2)

#bagging
library(randomForest)
rf.wage=randomForest(logwage~.,data=train.wage,mtry=ncol(train.wage)-1)
pred.bag=predict(rf.wage,test.wage)
mean((test.wage$logwage-pred.bag)^2)

#random forest
rf.wage=randomForest(logwage~.,data=train.wage,mtry=6)
pred.rf=predict(rf.wage,test.wage)
mean((test.wage$logwage-pred.rf)^2)

#linear model
#exclude regions because it only has one level
train.wage=train.wage[,-which(colnames(train.wage)=='region')]
lm.fit=glm(logwage~.,data=train.wage)
summary(lm.fit)
pred.lm=predict(lm.fit,newdata=test.wage)
mean((test.wage$logwage-pred.lm)^2)

#boosting performs best.
