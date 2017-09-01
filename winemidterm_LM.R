getwd()
setwd("/Users/danikay/desktop")
mytestdata<-read.csv("wine.test.csv")
mytraindata<-read.csv("wine.train.csv")
mytestdata
summary(mytraindata)


#split wine data by color
newtraindata <- split(mytraindata, mytraindata$color)
redtraindata <- newtraindata$red
whitetraindata <- newtraindata$white
redtraindata
whitetraindata

#removing outliers
#exploratory histograms for red wine data
hist(redtraindata$fixed.acidity) #sample

#exploratory histograms for red and white wine data
hist(whitetraindata$fixed.acidity) #sample

#exploratory scatter plots for red and white wine data
plot(redtraindata$alcohol, redtraindata$volatile.acidity, main="Example", xlab="acid ", ylab="acid ") #sample

####################################################################################################################################################

#SIMPLE MULTIVARIATE LINEAR MODEL

fitred = lm(quality~fixed.acidity+residual.sugar+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+alcohol,data=redtraindata)
summary(fitred)

fitwhite = lm(quality~fixed.acidity+residual.sugar+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+alcohol,data=whitetraindata)
summary(fitwhite)

####################################################################################################################################################
#FORWARD STEPWISE
#forward stepwise subset selection red wine
step(lm(quality~fixed.acidity+residual.sugar+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+alcohol, data = redtraindata), direction="forward")
#forward stepwise subset selection white wine
step(lm(quality~fixed.acidity+residual.sugar+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+alcohol, data = whitetraindata), direction="forward")

####################################################################################################################################################

#installing relevant library for k-fold
install.packages("leaps")
library(leaps)
regfit.full=regsubsets(quality~.,data=redtraindata)
summary(regfit.full)

#Red wine model selection by cross validation (k-fold) plus predict functin definition
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

set.seed(10)
folds=sample(rep(1:10,length=nrow(redtraindata)))
folds
table(folds)
cv.errors=matrix(NA,10,9)

for(k in 1:10){
  best.fit=regsubsets(quality~.,data=redtraindata[folds!=k,],method="forward")
  for(i in 1:9){
    pred=predict(best.fit,redtraindata[folds==k,],id=i)
    cv.errors[k,i]=mean( (redtraindata$quality[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")
###For red wine, RMSE drops signifiicantly at index 7 (probably remove fixed acidity and free sulfur dioxide)

#White wine model selection by cross validation(k-fold) plus predict functin definition
##function
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

set.seed(10)
folds=sample(rep(1:10,length=nrow(whitetraindata)))
folds
table(folds)
cv.errors=matrix(NA,10,9)

for(k in 1:10){
  best.fit=regsubsets(quality~.,data=whitetraindata[folds!=k,],method="forward")
  for(i in 1:9){
    pred=predict(best.fit,whitetraindata[folds==k,],id=i)
    cv.errors[k,i]=mean( (whitetraindata$quality[folds==k]-pred)^2)
  }
}

rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=10,type="b")
###For white wine, RMSE drops significantly at index 7 and then incrementally at 8 and 9. Probably can remove free sulfar dioxide.

####################################################################################################################################################

#Ridge Regression and Lasso for Red Wine data
install.packages("glmnet")
library(glmnet)
x=model.matrix(alcohol~.-1,data=redtraindata) 
y=redtraindata$alcohol
 
#ridge regression
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
summary(cv.ridge)

#lasso model
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

####################################################################################################################################################

#Ridge Regression and Lasso for White Wine data
install.packages("glmnet")
library(glmnet)
#x=model.matrix(quality~.-1,data=redtraindata) 
#y=whitetraindata$quality

x   <- model.matrix(~.-1, data= redtraindata[,-1])
y   <- factor(quality)

#ridge regression
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
summary(cv.ridge)

#lasso model
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)


mod_cv <- cv.glmnet(x=x, y=y, family='binomial')

mod_cv$lambda.1se

coef(mod_cv, mod_cv$lambda.1se)

mod_cv$lambda.min

coef(mod_cv, mod_cv$lambda.min)

