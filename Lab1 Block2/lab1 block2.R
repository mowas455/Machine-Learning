
#setwd("C:/Users/Mounish/Desktop/ML/Lab1 Block2")
RNGversion('3.5.1')
library(randomForest)

#Generation of Training Data

set.seed(12345)
x1<-runif(100)
x2<-runif(100)
trdata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
trlabels<-as.factor(y)


#Generation of test Data

set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
plot(x1,x2,col=(y+1))
table(telabels)

barplot(tedata3)

##Random Forest model fitting
set.seed(12345)

rf=randomForest(trlabels ~.,data=trdata,ntree=1,nodesize=25,keep.forest = TRUE)

pred<-predict(rf,newdata = tedata)

accuracy<-mean(telabels == pred)
accuracy

cm=table(telabels,as.factor(pred))
cm

mmce <- 1 - (sum(diag(cm))/sum(cm))
mmce


#####Random Forest function  for ntree=1,10,100

randforest<-function(train,label)
{
  set.seed(12345)
  
  rnf1=randomForest(label ~.,data=train,ntree=1,nodesize=25,keep.forest = TRUE)
  
  rnf2=randomForest(label ~.,data=train,ntree=10,nodesize=25,keep.forest = TRUE)

  rnf3=randomForest(label ~.,data=train,ntree=100,nodesize=25,keep.forest = TRUE)
  
  pred1<-predict(rnf1,newdata = tedata)
  
  pred2<-predict(rnf2,newdata = tedata)
  
  pred3<-predict(rnf3,newdata = tedata)
  
  accuracy1<-mean(telabels == pred1)
  accuracy2<-mean(telabels == pred2)
  accuracy3<-mean(telabels == pred3)
  
 print(accuracy1)
 print(accuracy2)
 print(accuracy3)
}
randforest(train=trdata,label = trlabels)



#### iterate to generate a dataset

set.seed(12345)
list1 = list()
for (i in 1:5) { # Indicate number of iterations with "i"
  x1 =  runif(100)
  x2 = runif(100)
  y<-as.factor(as.numeric(x1<x2))
  list1[[i]] = data.frame(x1,x2,y)
}
#list1
#list1[[1]]$y





a=(list1[[1]])
a$y
is.type(a)
rfv=randomForest(y~.,data=a,ntree=1,nodesize=25,keep.forest = TRUE)
rfv

n<-data.frame()
pred<-c()

for (i in list1){
  set.seed(12345)
  
  n<-as.data.frame(i)
  
  rf=randomForest(y~.,data=n,ntree=1,nodesize=25,keep.forest = TRUE)
  
  pred<-predict(rf,newdata = n)
  
  accuracy1<-mean((n$y) == pred)

  #print(pred)
  
#  print(n)
  
  print(accuracy1)
  }

#++++==================================================================================================================
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
trd  
#=================================================================================
#Condition- 2 (x1<0.5)

set.seed(12345)
list2 = list()
for (i in 1:5) { # Indicate number of iterations with "i"
  x1 =  runif(100)
  x2 = runif(100)
  y<-as.factor(as.numeric(x1<0.5))
  list2[[i]] = data.frame(x1,x2,y)
}


set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata2<-cbind(x1,x2)
y<-as.numeric((x1<0.5))
telabels2<-as.factor(y)
plot(x1,x2,col=(y+1))
table(telabels2)

#list2

m2_1<-c()
m2_10<-c()
m2_100<-c()
n2_1<-c()
n2_10<-c()
n2_100<-c()
for (i in list2){
  set.seed(12345)
  
  n<-as.data.frame(i)
  
  rf12=randomForest(y~.,data=n,ntree=1,nodesize=25,keep.forest = TRUE)
  
  rf22=randomForest(y~.,data=n,ntree=10,nodesize=25,keep.forest = TRUE)
  
  rf32=randomForest(y~.,data=n,ntree=100,nodesize=25,keep.forest = TRUE)
  
  pred1<-predict(rf12,newdata = tedata2)
  
  pred2<-predict(rf22,newdata = tedata2)
  
  pred3<-predict(rf32,newdata = tedata2)
  
  accuracy1<-mean(telabels2 == pred1)
  
  accuracy2<-mean(telabels2 == pred2)
  
  accuracy3<-mean(telabels2 == pred3)
  
  cm1 <- table(actual = telabels2, fitted = pred1)
  
  mmce1 <- 1 - (sum(diag(cm1))/sum(cm1))
  
  cm2 <- table(actual = telabels2, fitted = pred2)
  
  mmce2 <- 1 - (sum(diag(cm2))/sum(cm2))
  
  cm3 <- table(actual = telabels2, fitted = pred3)
  
  mmce3 <- 1 - (sum(diag(cm3))/sum(cm3))
  
  
  n2_1<-c(n2_1,accuracy1)
  
  n2_10<-c(n2_10,accuracy2)
  
  n2_100<-c(n2_100,accuracy3)
  
  m2_1<-c(m2_1,mmce1)
  
  m2_10<-c(m2_10,mmce2)
  
  m2_100<-c(m2_100,mmce3)
  
}

trd2<-data.frame(n2_1,n2_10,n2_100,m2_1,m2_10,m2_100)
trd2  

 
#==========================================================================================================
#Condition- 3 ((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))

set.seed(12345)
list3 = list()
for (i in 1:1000) { # Indicate number of iterations with "i"
  x1 =  runif(100)
  x2 = runif(100)
  y<-as.factor(as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)))
  list3[[i]] = data.frame(x1,x2,y)
}


set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata3<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5))
telabels3<-as.factor(y)
plot(x1,x2,col=(y+1))
table(telabels3)

#list3

m3_1<-c()
m3_10<-c()
m3_100<-c()
n3_1<-c()
n3_10<-c()
n3_100<-c()
for (i in list3){
  set.seed(12345)
  
  n<-as.data.frame(i)
  
  rf13=randomForest(y~.,data=n,ntree=1,nodesize=12,keep.forest = TRUE)
  
  rf23=randomForest(y~.,data=n,ntree=10,nodesize=12,keep.forest = TRUE)
  
  rf33=randomForest(y~.,data=n,ntree=100,nodesize=12,keep.forest = TRUE)
  
  pred1<-predict(rf13,newdata = tedata3)
  
  pred2<-predict(rf23,newdata = tedata3)
  
  pred3<-predict(rf33,newdata = tedata3)
  
  accuracy1<-mean(telabels3 == pred1)
  
  accuracy2<-mean(telabels3 == pred2)
  
  accuracy3<-mean(telabels3 == pred3)
  
  cm1 <- table(actual = telabels3, fitted = pred1)
  
  mmce1 <- 1 - (sum(diag(cm1))/sum(cm1))
  
  cm2 <- table(actual = telabels3, fitted = pred2)
  
  mmce2 <- 1 - (sum(diag(cm2))/sum(cm2))
  
  cm3 <- table(actual = telabels3, fitted = pred3)
  
  mmce3 <- 1 - (sum(diag(cm3))/sum(cm3))
  
  
  n3_1<-c(n3_1,accuracy1)
  
  n3_10<-c(n3_10,accuracy2)
  
  n3_100<-c(n3_100,accuracy3)
  
  m3_1<-c(m3_1,mmce1)
  
  m3_10<-c(m3_10,mmce2)
  
  m3_100<-c(m3_100,mmce3)
  
}

trd3<-data.frame(n3_1,n3_10,n3_100,m3_1,m3_10,m3_100)
dim(trd3)  
trd2
trd

mean(trd3$m3_100)

var(trd3$m3_1)

summary(trd3)

hist(trd3$m3_10,breaks = 30)
?hist
#=========================================================================================================
#1. For the 1000 Datasets,the accuracy and miss classification error in the random forest model is varying 
# by number of trees we given for the model.
#By observing the model, mean of miss classification error rate(mmce) is high when ntree=1,but when the tree get 
# increased to 10 and 100 mmce value of these dataset is get decreased.

pairplot(trd3)

1-0.075465 


barplot(tedata3)
hist(tedata2)

lin=
plot(trd3$m3_1)
library(ggplot2)

# stacked bar chart
ggplot(trd3$, 
       aes(x =c(1,10,100), 
           fill = drv)) + 
  geom_bar(position = "stack")
