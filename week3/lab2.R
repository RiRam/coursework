library(glmnet)
library(ISLR)

fix(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)

x=model.matrix(Salary~., Hitters)[,-1] # produces matrix corresponding to the predictors and transforms qualitative variables into dummy variables (!!!important because glmnet() only takes numerical input)
y=Hitters$Salary
grid=10^seq(10, -2, length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # alpha determines what type of model is fit, 0 is ridge, 1 is lasso

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20,] # used to get ridge regression coefficients for a new lambda, ex. 50

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# ridge regression model on training set, MSE on test set using lambda=4
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# if fit a model with just an intercept, would've predicted test observations using the mean of training observations
# if so, test set MSE
mean((mean(y[train])-y.test)^2)

# could get same MSE as above by fitting ridge regression model with very large lambda
ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)
# least squares is simply ridge regression with lambda = 0

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]
# using cross validation to choose lambda is better than randomly picking
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam # 211.7416
# the value of lambda that results in the smallest cross validation error is 212
ridge.pred=predict(ridge.mod,s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2) # 96015.51 - better than when lambda=4

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


### The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
# coefficient plot shows that some coefficients will = 0 depending on choice of tuning parameter (lambda)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) # 100743.4
# much lower than test set MSE of null model and least squares, close to ridge w/lambda determined w/ cross validation
# lasso - *advantage over ridge in that resulting coefficient estimates are minimal -> 12 of the 19 coefficient estimates = 0, lasso model only contains seven variables

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0] # 7 nonzero coefs
