# Lab 3 block 1 of 732A99/TDDE01 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)

index <- sample(1:4601)
tr <- spam[index[1:3000], ]
va <- spam[index[3001:3800], ]
trva <- spam[index[1:3800], ]
te <- spam[index[3801:4601], ]

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}



filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3


   # Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3 ? Why ?

# 2. What is the estimate of the generalization error of the filter returned ? err0, err1, err2 or err3 ? Why ?




#=======================================#===================================================================
#Answer
#Support Vector Machines
#svm is a supervised learning algorithm, which is widely used for classification algorithm.
#SVM is applicable for data that are linearly separable and for non-linear data it uses kernel
#method for classification.

# It classifies the two classes using hyperplane,The hyperplane should have the largest margin in a
#high dimensional space to separate a given data into a classes.THe margin between the two classes 
#represent the longest distance between the closest data point of the classes.

#Baesd on the number of the input features/variables, 
#the decision boundary can be a line (if we had only 2 features) or a hyperplane.
#A hyperplane is an (N-1)-dimensional subspace for an N-dimensional space. 

#SVM pick the decision boundary that maximizes the distance to the support vectors. 
#The decision boundary is drawn in a way that the distance to support vectors are maximized. 
#If the decision boundary is too close to the support vectors then, 
#it will be sensitive to noise and not generalize well.

#Error= (c)Classification Error + Margin Error(soft margin)
 
#margin help to increase the distance of the decision boundary to the support vectors

#Maximize the number of points that are correctly classified in the training set.

#C-parameter
# C is small, the penalty for misclassified data point is low so decision boundary with large margin is choosen
# at the expense of the great number of misclassifications
# c is large ,SVM tries to minimize the number of misclassifed samples and 
#results in decision boundary with smaller armgin.

#RBF





