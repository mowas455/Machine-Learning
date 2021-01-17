#libraries 
library(ggplot2)
library(glmnet)
library(psych)
library(tidyr)

# Read and split the data
data = read.csv("C:/Users/Mounish/Desktop/ML/Lab1 Block 1/tecator.csv")

# Split data into train and test set
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]

#=================================================================================================================

# 1.fit the linear model to the this data

fit<-lm(Fat~ .-ï..Sample-Protein-Moisture,data=train)
summary(fit)


lm_train_pred = predict(fit, train)
train_mse = mean((train$Fat-lm_train_pred)^2)
train_rmse = sqrt(mean((train$Fat-lm_train_pred)^2))

test_predictions_lm = predict(fit, test)
test_mse = mean((test$Fat-test_predictions_lm)^2)
test_rmse = sqrt(mean((test$Fat-test_predictions_lm)^2))

cat("Train error\nMSE: ", train_mse, "\nRMSE: ", train_rmse,"\n\nTest error\nMSE: ", test_mse, "\nRMSE: ", test_rmse)

#================================================================================================================
# 3.Fit The Lasso model 

covariates = scale( train[,2:101])
response = scale(train$Fat)

set.seed(12345)
model_lasso = glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
plot(model_lasso, xvar="lambda", label=T)

10^

#================================================================================================================
# 4.Presenting a plot that represent how DOF depend on the penalty parameter.

plot(model_lasso$lambda,model_lasso$df,col="red",
     ylab="Degrees of Freedom",
     xlab="Lambda")
#By the decreasing degrees of freedom , the lambda value of the model get decreased, but the 
#=================================================================================================================

#5.Fit the ridge model.

model_ridge = glmnet(as.matrix(covariates), response, alpha=0, family="gaussian")
plot(model_ridge, xvar="lambda", label=T)

#=========================================================================================================================

# 6.Optimal Lasso
set.seed(12345)
cv_model1<-cv.glmnet(as.matrix(covariates),response,family="gaussian" , lambda=10^seq(-5,5,length=500), alpha=1)

plot(cv_model1)

lambda_optimal<-cv_model1$lambda.1se
lambda_optimal

#Finding the optimal features

features<-as.matrix(coef(cv_model1))

optimal_features<-features[features!=0,]
length(optimal_features)

