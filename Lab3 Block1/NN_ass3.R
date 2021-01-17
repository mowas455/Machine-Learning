library(neuralnet)
library(ggplot2)

set.seed(1234567890)
points<-runif(500,min=0,max=10)

#aaplying sin fucntion
data<-data.frame(points,Sin=sin(points))
train<-data[1:25,]
test<-data[26:500,]
winit<-seq(-1,1,0.1)

##Neural Network Model
nn<-neuralnet(Sin~points,train,hidden=3,startweights = winit)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn,test), col="red", cex=1)


#2
#Generating points from uniform dsitribution
points<-runif(500,min=0,max=20)

#aaplying sin fucntion
data_2<-data.frame(points,Sin=sin(points))
plot(train, cex=2,xlim=c(0,20))
points(data_2, col = "blue", cex=1)
points(data_2[,1],predict(nn,data_2), col="red", cex=1)


#3
points_3<-runif(500,0,10)
data_3<-data.frame(points_3,Sin=sin(points_3))
nn_2<-neuralnet(points_3~Sin,data_3,hidden=c(1,1),startweights = winit)
pred_2<-predict(nn_2,data_3)
plot(data_3, cex=2)
points(data_3, col = "blue", cex=1)
points(pred_2,data_3$Sin ,col="red", cex=1,xlim=c(0,20))



logl <- function(theta,x,y){
  y <- y
  x <- as.matrix(x)
  beta <- theta[1:ncol(x)]
  loglik <- sum(-y*log(1 + exp(-(x%%beta))) - (1-y)*log(1 + exp(x%%beta)))
  return(-loglik)
}

train$age_100 = train$AGE/100
train_temp_x = select(train,SEX,age_100)
train_temp_y = select(train,default_payment)
w = c(0,1,0)
logl(w,train_temp_x,train_temp_y)


