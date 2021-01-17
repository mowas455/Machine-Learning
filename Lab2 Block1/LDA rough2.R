setwd("C:/Users/Mounish/Desktop/ML/Lab2 Block1")
library (ggplot2)

###TReading data
data=iris

ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width)) +geom_point(aes(color = factor(Species))) 

x0=c(data$Sepal.Length)
y0=c(data$Sepal.Width)
z=c(data$Species)
df = data.frame(x1=x0,x2=y0)
y=as.factor(z)

LDA <- function(x, y) {
  group1_index = which( y == 1 )
  group2_index = which( y == 2 )
  group3_index = which( y == 3 )
  #priors:
  prior_group1 = length(group1_index) / length(y)
  prior_group2 = length(group2_index) / length(y)
  prior_group3 = length(group3_index) / length(y)
  print("Prior probabilities of groups:")
  print(c(prior_group1, prior_group2,prior_group3))
  #means:
  mean_group1 = colMeans(x[group1_index, ])
  mean_group2 = colMeans(x[group2_index, ])
  mean_group3 = colMeans(x[group3_index, ])
  
  print("Group means:")
  print(rbind(mean_group1, mean_group2,mean_group3))
  
  #Covariances
  cv1<-cov(x[group1_index, ])
  cv2<-cov(x[group2_index, ])
  cv3<-cov(x[group3_index, ])
  print("covariance matrix of group 1")
  print(cv1)
  
  print("covariance matrix of group 2")
  print(cv2)
  
  print("covariance matrix of group 3")
  print(cv3)
  
  pooled_cv<-((length(group1_index)*cv1)+(length(group2_index)*cv2)+(length(group3_index)*cv3))/length(y)
  print("Pooled covariance matrix of groups")
  print(pooled_cv)

  }
LDA(df,y)








#==============================================================================================================================
n<-which(y==1)
h<-cov(df[n,])
n1<-which(y==2)
h1<-cov(df[n1,])
n2<-which(y==3)
h2<-cov(df[n2,])
nm<-h+h1+h2
nm
cov(df)




Sigma_XX = (cov(data_B)*(nrow(data_B)-1) + cov(data_M)*(nrow(data_M)-1))/(nrow(data)-2)
solve(Sigma_XX) %*% (mean_M - mean_B)