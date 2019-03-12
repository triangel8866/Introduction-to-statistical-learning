#6
#(a)
lambda=3
y=6
beta=seq(-10,10,0.1)
func=(y-beta)^2+lambda*beta^2
plot(beta,func)
min.point = which.min(func)
points(beta[min.point],func[min.point],col="red")
print(beta[min.point])
beta.opt=y/((1+lambda))
beta.opt

#(b)
func=(y-beta)^2+lambda*abs(beta)
plot(beta,func)
min.point=which.min(func)
points(beta[min.point],func[min.point],col="red")
print(beta[min.point])
beta.opt=y-lambda/2
beta.opt

#1
#(a)
library(leaps)
set.seed(1)
x=rnorm(100)

#(b)
error=rnorm(100)
y=1+2*x+3*x^2+4*x^3+error
df=data.frame(poly(x,10,raw=T),y)
best.fit=regsubsets(y~.,data=df,nvmax=10)
fit.summary=summary(best.fit)
which.min(fit.summary$cp)
which.min(fit.summary$bic)
which.max(fit.summary$adjr2)

#(c)
#cp
plot(fit.summary$cp,type="b")
points(4,fit.summary$cp[4],col="red",pch=5)
#BIC
plot(fit.summary$bic,type="b")
points(3,fit.summary$bic[3],col="red",pch=5)
#adjusted r
plot(fit.summary$adjr2,type="b")
points(4,fit.summary$adjr2[4],col="red",pch=5)

#(d)
forward.fit=regsubsets(y~.,data=df,nvmax=10,method="forward")
fit.summary=summary(forward.fit)
which.min(fit.summary$cp)
which.min(fit.summary$bic)
which.max(fit.summary$adjr2)
coef(forward.fit,id=4)

backward.fit=regsubsets(y~.,data=df,nvmax=10,method="backward")
fit.summary=summary(backward.fit)
which.min(fit.summary$cp)
which.min(fit.summary$bic)
which.max(fit.summary$adjr2)
coef(backward.fit,id=4)

#(e)
set.seed(1)
library(glmnet)
x=model.matrix(y~.,data=df)[,-1]
y=df$y
grid=10^seq(10,-2,length=100)
cv.out=cv.glmnet(x,y,alpha=1)
bestlam=cv.out$lambda.min
bestlam
plot(cv.out)
lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam)
coef(lasso.mod)

#(f)
set.seed(1)
y=1+7*x^7+error
df=data.frame(poly(x,10,raw=T),y)
#best subset selection
best.fit=regsubsets(y~.,data=df,nvmax=10)
summary(best.fit)
which.min(summary(best.fit)$cp)
which.min(summary(best.fit)$bic)
which.min(summary(best.fit)$adjr2)
coef(best.fit,id=2)
coef(best.fit,id=1)

#lasso
x=model.matrix(y~.,data=df)[,-1]
y=df$y
#choose lambda
cv.out=cv.glmnet(x,y,alpha=1)
bestlam=cv.out$lambda.min
bestlam
plot(cv.out)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
predict(lasso.mod,type="coefficients",s=bestlam)

#9
#(a)
library(ISLR)
dim(College)
College=na.omit(College)
train=sample(c(TRUE,FALSE),nrow(College),replace=T)
test=(!train)
College.train=College[train,]
College.test=College[test,]

#(b)
glm.fit=glm(Apps~.,data=College.train)
glm.pred=predict(glm.fit,College.test)
mean((glm.pred-College.test$Apps)^2)

#(c)
library(glmnet)
x=model.matrix(Apps~.,data=College.train)[,-1]
y=College.train$Apps
x.test=model.matrix(Apps~.,data=College.test)[,-1]
grid=10^seq(10,-2,length=100)
out.cv=cv.glmnet(x,y,alpha=0)
bestlam=out.cv$lambda.min
bestlam
ridge.fit=glmnet(x,y,alpha=0,lambda=grid)
ridge.pred=predict(out.cv,s=bestlam,newx=x.test)
mean((ridge.pred-College.test$Apps)^2)

#(d)
cv.out=cv.glmnet(x,y,alpha=1)
bestlam=cv.out$lambda.min
bestlam
lasso.fit=glmnet(x,y,alpha=1,lambda=grid)                   
lasso.coef=predict(lasso.fit,s=bestlam,type="coefficients")[1:18,]
lasso.coef
lasso.pred=predict(lasso.fit,s=bestlam,newx=x.test)

#(e)
library(pls)
pcr.fit=pcr(Apps~.,data=College.train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,College.test,ncomp=10)
mean((pcr.pred-College.test$Apps)^2)

#(f)
pls.fit=plsr(Apps~.,data=College.train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,College.test,ncomp=6)
mean((pls.pred-College.test$Apps)^2)

#(g)
avg.test=mean(College.test$Apps)
lm.rsqr=1-mean((glm.pred-College.test$Apps)^2)/mean((avg.test-College.test$Apps)^2)
ridge.rsqr=1-mean((ridge.pred-College.test$Apps)^2)/mean((avg.test-College.test$Apps)^2)
lasso.rsqr=1-mean((lasso.pred-College.test$Apps)^2)/mean((avg.test-College.test$Apps)^2)
pcr.rsqr=1-mean((pcr.pred-College.test$Apps)^2)/mean((avg.test-College.test$Apps)^2)
pls.rsqr=1-mean((pls.pred-College.test$Apps)^2)/mean((avg.test-College.test$Apps)^2)
barplot(c(lm.rsqr,ridge.rsqr,lasso.rsqr,pcr.rsqr,pls.rsqr),names.arg=c("lm","ridge","lasso","pcr","pls"))
#All the five approaches have high R square
#There is not much difference among the test errors.

#10
#(a)
set.seed(1)
x=matrix(rnorm(20000),nrow=1000,ncol=20,dimnames=list(NULL,paste0("x",seq(20))))
beta=rnorm(20)
error=rnorm(1000)
beta[1]=0
beta[5]=0
beta[12]=0
beta[15]=0
beta[17]=0
y=x%*%beta+error

#(b)
train=sample(seq(1000),100)
test=(-train)
x.train=x[train,]
x.test=x[test,]
y.train=y[train]
y.test=y[test]

#(c)
library(leaps)
df.train=data.frame(x.train,y=y.train)
best.fit=regsubsets(y~.,data=df.train,nvmax=20)
train.mat=model.matrix(y~.,data=df.train)
val.errors=rep(NA,20)
for (i in 1:20){
  coefi=coef(best.fit,id=i)
  pred=train.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((pred-df.train$y)^2)
}
plot(val.errors,ylab="train MSE",pch=20,type="b")

#(d)
df.test=data.frame(x.test,y=y.test)
test.mat=model.matrix(y~.,data=df.test)
val.errors=rep(NA,20)
for(i in 1:20){
  coefi=coef(best.fit,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df.test$y-pred)^2)
}
plot(val.errors,ylab="MSE",type="b",pch=20)

#(e)
which.min(val.errors)

#(f)
coef(best.fit,id=14)

#(g)
names(beta)=rownames(beta,do.NULL=FALSE,prefix="x")
beta.errors=rep(NA,20)
beta.no=rep(NA,20)
for (i in 1:20){
  coefi=coef(best.fit,id=i)
  beta.no[i]=length(coefi)-1
  pred.name=names(coefi)[-1]
  beta.errors[i]=sqrt(sum((beta[pred.name]-coefi[-1])^2)+(sum(beta[!names(beta) %in% pred.name]^2)))
}
plot(beta.no,beta.errors)
which.min(beta.errors)

#11
#(a)
library(MASS)
library(leaps)
library(glmnet)
Boston=na.omit(Boston)

#best subsets selection
k=10
set.seed(1)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

folds=sample(1:k,nrow(Boston),replace=TRUE)
cv.errors=matrix(NA,k,13,dimnames=list(NULL,paste(1:13)))
for (j in 1:k){
  best.fit=regsubsets(crim~.,data=Boston[folds!=j,],nvmax=13)
  for (i in 1:13){
    pred=predict(best.fit,Boston[folds==j,],id=i)
    cv.errors[j,i]=mean((Boston$crim[folds==j]-pred)^2)
  }
}
msep=sqrt(apply(cv.errors,2,mean))
plot(msep,type="b",pch=20)
which.min(mean.cv.errors)
msep[which.min(msep)]

#lasso
x=model.matrix(crim~.,data=Boston)
y=Boston$crim
cv.lasso=cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda==cv.lasso$lambda.min])

#ridge
cv.ridge=cv.glmnet(x,y,alpha=1)
plot(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda==cv.ridge$lambda.min])

#PCR
library(pls)
pcr.fit=pcr(crim~.,data=Boston,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="RMSE")

