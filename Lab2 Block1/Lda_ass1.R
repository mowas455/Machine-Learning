library(ggplot2)
data=iris
x0=c(data$Sepal.Length)
y0=c(data$Sepal.Width)
z=c(data$Species)
x = data.frame(x1=x0,x2=y0)
y=as.factor(z)
### 1.
# scatter plot for original data with target

  ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width))+
    geom_point(aes(color = factor(Species))) 
### 2a.
# Grouping the Targets
  group1_index = which( y == 1 )
  group2_index = which( y == 2 )
  group3_index = which( y == 3 )
  
# priors:
  prior_group1 = length(group1_index) / length(y)
  prior_group2 = length(group2_index) / length(y)
  prior_group3 = length(group3_index) / length(y)
  print("Prior probabilities of groups:")
  print(c(prior_group1, prior_group2,prior_group3))
  
# means:
  mean_group1 = as.matrix(colMeans(x[group1_index, ]))
  mean_group2 = as.matrix(colMeans(x[group2_index, ]))
  mean_group3 = as.matrix(colMeans(x[group3_index, ]))
  
  #print("Group means:")
  print(rbind(mean_group1, mean_group2,mean_group3))
  
# Co-variances Matrix
  cv1<-cov(x[group1_index, ])
  cv2<-cov(x[group2_index, ])
  cv3<-cov(x[group3_index, ])
  print("covariance matrix of group 1")
  print(cv1)
  
  print("covariance matrix of group 2")
  print(cv2)
  
  print("covariance matrix of group 3")
  print(cv3)

### 2b.  
# Pooled Co-variance Matrix
  pooled_cv<-as.matrix((length(group1_index)*cv1)+(length(group2_index)*cv2)+(length(group3_index)*cv3))/length(y)
  print("Pooled covariance matrix of groups")
  print(pooled_cv)

### 2d.  
# Discriminant function
 
 disc_fn<-function(v,p_cv,m_g,p_g)
 {
   v<-as.matrix(v)
   p_cv<-solve(p_cv)
   d1<-((v%*%p_cv)%*%(m_g))
   d2<-(0.5*t(m_g))%*%(p_cv)%*%(m_g)
   t_d<-d1-(as.numeric(d2))+log(p_g)
   return(t_d)
 }

# Bind the Discriminant values in the data.frame
  d_val1<- disc_fn(x,pooled_cv,mean_group1,prior_group1)
  d_val2<- disc_fn(x,pooled_cv,mean_group2,prior_group2)
  d_val3<- disc_fn(x,pooled_cv,mean_group3,prior_group3)

  disc_val<-cbind(d_val1,d_val2,d_val3)
  dis<-as.data.frame(disc_val)
  
  colnames(dis)<-c("setosa","versicolor","virginica")
  head(dis)

# Found the target value of this data by max value occurs on the row
  m<-colnames(dis)[max.col(dis, ties.method = "first")]
#https://statisticsglobe.com/return-column-name-of-largest-value-for-each-row-in-r
  dis$y<-m # add those values as y in the same dataframe



### 2e.
# Decision boundary
# m1<-(-0.5*t(mean_group1))%*%(cv1)%*%(mean_group1)
# m1+log(prior_group1)

  decision_boundary<-function(x,y,z)
  {
   m<-(-0.5*t(x))%*%(y)%*%(x)
   w_oi<-m+log(z)
   w_i<-y%*%x
   cat("w_oi",w_oi,"\n","w_i",w_i,"\n")
  }
  dc1<- decision_boundary(mean_group1,cv1,prior_group1)   #decision_bndy value of group 1
  dc2<- decision_boundary(mean_group2,cv2,prior_group2)   #decision_bndy value of group 2
  dc3<- decision_boundary(mean_group3,cv3,prior_group3)   #decision_bndy value of group 3


### 3.
# plot that showing the prediction by the builted LDA function
  
 ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width)) +
   geom_point(aes(color = factor(dis$y))) 

# confusion matrix

  table("Prediction"=as.factor(dis$y), "Reference"=y)

#miss-classification rate
  missclassrate=function(y,y_i)
  {
   n=length(y)
   v<-1-(sum(diag(table(y,y_i)))/n)
   return(v)
  }
  missclassrate(as.factor(dis$y),y)
  
# LDA Model
 library(MASS)

 fit_lda <- lda(y~., data = x)
 coef(fit_lda)
 pred_lda <- predict(fit_lda, x)
 vn<-data.frame(original = y, pred = pred_lda$class)
 table(vn$pred,vn$original)
 
 ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width)) +
   geom_point(aes(color = factor(vn$pred))) 

### 4.Sample the data using rmvnorm function
 
 library(mvtnorm)
  bvn1 <- rmvnorm(50, mean = mean_group1, sigma = cv1 )
  bvn2 <- rmvnorm(50, mean = mean_group2, sigma = cv2 )
  bvn3 <- rmvnorm(50, mean = mean_group3, sigma = cv3 )

  bvn<-rbind(bvn1,bvn2,bvn3)
  bvn<-as.data.frame(bvn)
  bvn$y<-y
  
  # sample the data
  sample_data<-bvn[sample(nrow(bvn), 150), ]
  
  colnames(bvn)<-c("sepal.length","sepal.width","species")
  

  ggplot(data =bvn, aes(x = sepal.length,y = sepal.width)) +geom_point(aes(color = factor(y))) 

 # ggplot(data =sample_data, aes(x = sepal.length,y = sepal.width)) +geom_point(aes(color = factor(sample_data$species))) 

### 5.Logistic Regression Model
 
 library(nnet)
 irisModel<-multinom(y~x1+x2 ,data  =  x)
  m<-summary(irisModel)

  pred2<-predict(irisModel,x)
  pred2

  ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width)) +
    geom_point(aes(color = factor(pred2)))

  table("Prediction"=as.factor(pred2), "Reference"=y)
  
  
#====================================================
#Prediction for new data

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
smean_group1 = as.matrix(colMeans(sample_data[group1_index, ]))
smean_group2 = as.matrix(colMeans(sample_data[group2_index, ]))
smean_group3 = as.matrix(colMeans(sample_data[group3_index, ]))

#print("Group means:")
#print(rbind(smean_group1, mean_group2,mean_group3))

#Covariance Matrix
scv1<-cov(sample_data[group1_index, ])
scv2<-cov(sample_data[group2_index, ])
scv3<-cov(sample_data[group3_index, ])
# print("covariance matrix of group 1")
# print(cv1)
# 
# print("covariance matrix of group 2")
# print(cv2)
# 
# print("covariance matrix of group 3")
# print(cv3)

#Pooled Covariance Matrix
spooled_cv<-as.matrix((length(group1_index)*scv1)+(length(group2_index)*scv2)+(length(group3_index)*scv3))/length(y)
# print("Pooled covariance matrix of groups")
# print(pooled_cv)

sd_val1<- disc_fn(sample_data,spooled_cv,smean_group1,prior_group1)
sd_val2<- disc_fn(sample_data,spooled_cv,smean_group2,prior_group2)
sd_val3<- disc_fn(sample_data,spooled_cv,smean_group3,prior_group3)

sdisc_val<-cbind(sd_val1,sd_val2,sd_val3)
sdis<-as.data.frame(sdisc_val)
sdis
colnames(sdis)<-c("setosa","versicolor","virginica")

m<-colnames(sdis)[max.col(sdis, ties.method = "first")]
sdis$z<-m
#dis
table("Prediction"=as.factor(sdis$z), "Reference"=y)
mean_group1
smean_group1


ggplot(data = sample_data, aes(x = V1,y = V2)) +geom_point(aes(color = factor(sdis$z))) 
