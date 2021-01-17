dataset = read.csv("C:/Users/Mounish/Desktop/ML/Lab1 Block 1/parkinsons.csv")

# Q1
p<-dataset$motor_UPDRS
hist(p,main="Distribution of Y_hat")
#Based on the given data , motor_UPDRS is uniformly distributed.
#p(??|y, ??^2) ??? p(??)p(y|??, ??^2)

#Q2

n <- dim(dataset)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.6))  #Training split
train <- dataset[id,]

id1 <- setdiff(1:n, id)         # Setting different data b/w Trainig and test

set.seed(12345) 
id2 <- sample(id1, floor(n*0.4))
test<- dataset[id2,]
dim(train)
dim(test)

# Scale the data
train<-scale(train)
exclude_x_columns = c("subject.","sex","age","test_time","motor_UPDRS","total_UPDRS")

x_train= subset(train, select = -c(subject.,sex,age,test_time,motor_UPDRS,total_UPDRS))
y_train=train[,5]

wi = as.matrix(rnorm(dim(x_train)[2], mean = 0, sd = 0.01))
sigma_i = runif(1, 0.001, 1)

llhood<-function(x,y,w,sigma){
  beta=as.matrix(w)
  sigma=sigma
  x=as.matrix(x)
  y=as.matrix(y)
  n<-nrow(x)
  RSS <- sum(( y - (x%*% beta) )^2)
  ll<-(-(n/2)*log(2*pi*sigma^2)-(1/(2*sigma^2))*RSS)
  return(ll)
              
}
llhood(x_train,y_train,wi,sigma_i)

#ridge function
ridge<- function(x,y,w,sigma,lamda)
{
beta=as.matrix(w)
sigma=sigma
x=as.matrix(x)
y=as.matrix(y)
n<-nrow(x)
RSS <- sum(( y - (x%*% beta) )^2)
penalty<-lamda*sum(beta  ^2)
ll1<--((n/2)*log(2*pi*sigma^2))-((1/(2*sigma^2))*RSS)+penalty
return(ll1)
  
}
ridge(x_train,y_train,wi,sigma_i,1)

sum(wi)

#=============================================================================================================
# Using Package
library(tidyverse)
library(broom)
library(glmnet)


n <- dim(dataset)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.6))  #Training split
train <- dataset[id,]

id1 <- setdiff(1:n, id)         # Setting different data b/w Trainig and test

set.seed(12345) 
id2 <- sample(id1, floor(n*0.4))
test<- dataset[id2,]
dim(train)
dim(test)

# Scale the data
train<-scale(train)
exclude_x_columns = c("subject.","sex","age","test_time","motor_UPDRS","total_UPDRS")

x_train= subset(train, select = -c(subject.,sex,age,test_time,motor_UPDRS,total_UPDRS))
y_train=train[,5]

#lamda Value
lambdas <- 10^seq(3, -2, by = -.1)

#Ridge Model
fit <- glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
summary(fit)

cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
plot(cv_fit)
plot(fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

y_predicted <- predict(fit, s = opt_lambda, newx = x_train)

# Sum of Squares Total and Error
sst <- sum((y_train - mean(y_train))^2)
sse <- sum((y_predicted - y_train)^2)

# R squared
rsq <- 1 - sse / sst
rsq

