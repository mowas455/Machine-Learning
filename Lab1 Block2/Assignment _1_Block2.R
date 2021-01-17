---
title: "Assignment-1(Ensemble Methods)"
author: "Mowniesh Asokan(mowas455)"
date: "11/23/2020"
output: pdf_document
---



## R Markdown

```{r }
##Condition-1 (x1<x2) 

set.seed(12345)
list1 = list()
for (i in 1:5) { # Indicate number of iterations with "i"
  x1 =  runif(100)
  x2 = runif(100)
  y<-as.factor(as.numeric(x1<x2))
  list1[[i]] = data.frame(x1,x2,y)
}


set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))
table(telabels)

#m- Denotes the miss classification error rate (_number indicates the number of trees)
#n- Denotes the accuracy

m_1<-c()
m_10<-c()
m_100<-c()
n_1<-c()
n_10<-c()
n_100<-c()
for (i in list1){
  set.seed(12345)
  
  n<-as.data.frame(i)
  
  rf1=randomForest(y~.,data=n,ntree=1,nodesize=25,keep.forest = TRUE)
  
  rf2=randomForest(y~.,data=n,ntree=10,nodesize=25,keep.forest = TRUE)
  
  rf3=randomForest(y~.,data=n,ntree=100,nodesize=25,keep.forest = TRUE)
  
  pred1<-predict(rf1,newdata = tedata)
  
  pred2<-predict(rf2,newdata = tedata)
  
  pred3<-predict(rf3,newdata = tedata)
  
  accuracy1<-mean(telabels == pred1)
  
  accuracy2<-mean(telabels == pred2)
  
  accuracy3<-mean(telabels == pred3)
  
  cm1 <- table(actual = telabels, fitted = pred1)
  
  mmce1 <- 1 - (sum(diag(cm1))/sum(cm1))

  cm2 <- table(actual = telabels, fitted = pred2)
  
  mmce2 <- 1 - (sum(diag(cm2))/sum(cm2))
  
  cm3 <- table(actual = telabels, fitted = pred3)
  
  mmce3 <- 1 - (sum(diag(cm3))/sum(cm3))
  
  
  n_1<-c(n_1,accuracy1)
  
  n_10<-c(n_10,accuracy2)
  
  n_100<-c(n_100,accuracy3)

  m_1<-c(m_1,mmce1)
  
  m_10<-c(m_10,mmce2)
  
  m_100<-c(m_100,mmce3)
  
}

trd<-data.frame(n_1,n_10,n_100,m_1,m_10,m_100)
 
cat("Mean of Miss classification using n_tree(1,10,100)",mean(trd$m_1),"\n")

cat("Variance of Miss classification using n_tree(1,10,100)",var(trd$m_1),"\n")

```