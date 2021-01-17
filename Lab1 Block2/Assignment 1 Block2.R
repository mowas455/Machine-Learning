setwd("C:/Users/Mounish/Desktop/ML/Lab1 Block2")
RNGversion('3.5.1')
library(randomForest)

#Generation of Training Data

set.seed(12345)
x1<-runif(100)
x2<-runif(100)
trdata1<-cbind(x1,x2)
y<-as.numeric(x1<x2)
trlabels1<-as.factor(y)

#trdata    #training data

#trlabels   #training labels(y)


#Generation of test Data

set.seed(1234)

x1<-runif(1000)
x2<-runif(1000)
tedata1<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels1<-as.factor(y)
plot(x1,x2,col=(y+1))
table(telabels1)
#tedata  #test data

#telabels  # test labels

##Random Forest model fitting
rf=randomForest(trlabels1 ~.,data=trdata1,ntree=1,nodesize=25,keep.forest = TRUE)

pred<-predict(rf,newdata = tedata1)

cm=table(telabels1,as.factor(pred))
cm

mmce <- 1 - (sum(diag(cm))/sum(cm))
mmce

#===================================================================
#ntree=1
 
times<-c(1:1000)

for(i in times){
  set.seed(12345)
  temp=trdata1[sample(nrow(trdata1), 100), ]
  
   rf11=randomForest(trlabels1 ~.,data=temp,ntree=1,nodesize=25,keep.forest = TRUE)
   
}


testpred11=predict(rf11,newdata = tedata1)

plot(testpred11)

accuracy11=mean(telabels1 == testpred11)
accuracy11

cm11=table(telabels1,as.factor(testpred11))
cm11

mmce11 <- 1 - (sum(diag(cm11))/sum(cm11))
mmce11

##ntree=10

for(i in times){
  set.seed(12345)
  e<-trdata1[sample(nrow(trdata1), 100), ]
  
  rf12=randomForest(trlabels1 ~.,data=e,ntree=10,nodesize=25,keep.forest = TRUE)
  
 }

testpred12=predict(rf12,newdata = tedata1)

accuracy12=mean(telabels1 == testpred12)
accuracy12

cm12=table(telabels1,as.factor(testpred12))
cm12

mmce12 <- 1 - (sum(diag(cm12))/sum(cm12))
mmce12

##ntree=100
for(i in times){
  set.seed(12345)
  temp<-trdata1[sample(nrow(trdata1), 100), ]
  
  rf13=randomForest(trlabels1 ~.,data=temp,ntree=100,nodesize=25,keep.forest = TRUE)
  
}

testpred13=predict(rf13,newdata = tedata1)

accuracy13=mean(telabels1 == testpred13)
accuracy13

cm13=table(telabels1,as.factor(testpred13))
cm13

mmce13 <- 1 - (sum(diag(cm13))/sum(cm13))
mmce13



#===================================
  
set.seed(12345)
x1<-runif(100)
x2<-runif(100)
trdata2<-cbind(x1,x2)
y<-as.numeric(x1<0.5)
trlabels2<-as.factor(y)
trlabels1
trlabels2
trlabels3


set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata2<-cbind(x1,x2)
y<-as.numeric(x1<0.5) 
telabels2<-as.factor(y)
plot(x1,x2,col=(y+1))
x1

##Random Forest model fitting

#ntree=1

times<-c(1:1000)

for(i in times){
  set.seed(12345)
  temp<-trdata2[sample(nrow(trdata2), 100), ]
  
  rf21=randomForest(trlabels2 ~.,data=temp,ntree=1,nodesize=25,keep.forest = TRUE)
  
}

testpred21=predict(rf21,newdata = tedata2)

accuracy21=mean(telabels2 == testpred21)
accuracy21

cm21=table(telabels2,as.factor(testpred21))
cm21

mmce21 <- 1 - (sum(diag(cm21))/sum(cm21))
mmce21

##ntree=10

for(i in times){
  set.seed(12345)
  temp<-trdata2[sample(nrow(trdata2), 100), ]
  
  rf22=randomForest(trlabels2 ~.,data=temp,ntree=10,nodesize=25,keep.forest = TRUE)
  
}

testpred22=predict(rf22,newdata = tedata2)

accuracy22=mean(telabels2 == testpred22)
accuracy22

cm22=table(telabels2,as.factor(testpred22))
cm22

mmce22 <- 1 - (sum(diag(cm22))/sum(cm22))
mmce22

##ntree=100
for(i in times){
  set.seed(12345)
  temp<-trdata2[sample(nrow(trdata2), 100), ]
  
  rf23=randomForest(trlabels2 ~.,data=temp,ntree=100,nodesize=25,keep.forest = TRUE)
  
}

testpred23=predict(rf23,newdata = tedata2)

accuracy23=mean(telabels2 == testpred23)
accuracy23

cm23=table(telabels2,as.factor(testpred23))
cm23

mmce23 <- 1 - (sum(diag(cm23))/sum(cm23))
mmce23


plot(x1,x2,col=(testpred23))





#================================================================================================================================
set.seed(12345)
x1<-runif(100)
x2<-runif(100)
trdata3<-cbind(x1,x2)
y<-as.numeric(((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)) )
trlabels3<-as.factor(y)
trlabels1
trlabels3


set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata3<-cbind(x1,x2)
y<-as.numeric(((x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5)))
telabels3<-as.factor(y)
plot(x1,x2,col=(y+1))
