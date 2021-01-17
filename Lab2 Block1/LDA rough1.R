setwd("C:/Users/Mounish/Desktop/ML/Lab2 Block1")
library (ggplot2)

###TReading data
data=iris

ggplot(data = data, aes(x = Sepal.Length,y = Sepal.Width)) +geom_point(aes(color = factor(Species))) 

#1.
# It is a linear data, so we cannot use this data without separating.That the sepal lengths
#and widths are likely better suited as potential features two separate between the three flower classes 

#For this data Lda is used as classfier and also dimensionality reduction 
#can works to maximize the seperation of the known categories

#About LDA#
# Actualiy  LDA tries to reduce dimensions of the feature set while retaining the information 
#that discriminates output classes. LDA tries to find a decision boundary around each cluster of a class.
###The goal of a LDA is often to project a feature space 
#(a dataset n-dimensional samples) into a smaller subspace k (where k???n???1),
# 
# x = c(.4,.55,.65,.9,.1,.35,.5,.15,.2,.85)
# y = c(.85,.95,.8,.87,.5,.55,.5,.2,.1,.3)
# z = c(1,1,1,1,1,0,0,1,0,0)

x=c(data$Sepal.Length)
y=c(data$Sepal.Width)
z=c(data$Species)


###
df = data.frame(x1=x,x2=y,y=as.factor(z))

df
p<-as.numeric(df$y==1)
p
m1 = apply(df[df$y=="1",1:2],2,mean)
m2 = apply(df[df$y=="2",1:2],2,mean)
m3 = apply(df[df$y=="3",1:2],2,mean)
m<-m1+m2+m3
sig<-cov(df[,1:2])
sig
n<-diag(sig)
n
omega = solve(n)%*%(m1-m)
omega
plot(omega)






 
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
  
  #discriminant coefficients:
  x[group1_index, ] = sweep(x[group1_index, ], 2, mean_group1, "-")
  x[group2_index, ] = sweep(x[group2_index, ], 2, mean_group2, "-")
  x[group3_index, ] = sweep(x[group2_index, ], 2, mean_group3, "-")
  
  sigma = solve(cov(x))
  disc_coeff1 = sigma %*% mean_group1 
  disc_coeff2 = sigma %*% mean_group2 
  disc_coeff3 = sigma %*% mean_group3 
  
  print("Coefficients of linear discriminants:")
  print(rbind(disc_coeff1,disc_coeff2,disc_coeff3))
}
d1= data.frame(x1=x,x2=y)
t1=as.factor(z)
LDA(x=d1,y=t1)


#cov(d1)
library(MASS)
lda.fit = lda(t1 ~ ., as.data.frame(d1))
pred<-predict(lda.fit,d1)
lda.fit

plot(lda.fit)
plot(pred)
pred
#=========================================
library (dplyr)
data_B <- data %>% filter(Species == 1) %>% dplyr::select(-Species) %>% as.matrix()
data_M <- data %>% filter(Species == 2) %>% dplyr::select(-Species) %>% as.matrix()
data_B
data_M

class<-which(y==1)
v

#========================
# ty.lda <- function(x, groups){
#   x.lda <- lda(groups ~ ., as.data.frame(x))
#   
#   gr <- length(unique(groups))   ## groups might be factors or numeric
#   v <- ncol(x) ## variables
#   m <- x.lda$means ## group means
#   
#   w <- array(NA, dim = c(v, v, gr))
#   
#   for(i in 1:gr){
#     tmp <- scale(subset(x, groups == unique(groups)[i]), scale = FALSE)
#     w[,,i] <- t(tmp) %*% tmp
#   }
#   
#   W <- w[,,1]
#   for(i in 2:gr)
#     W <- W + w[,,i]
#   
#   V <- W/(nrow(x) - gr)
#   iV <- solve(V)
#   
#   class.funs <- matrix(NA, nrow = v + 1, ncol = gr)
#   colnames(class.funs) <- paste("group", 1:gr, sep=".")
#   rownames(class.funs) <- c("constant", paste("var", 1:v, sep = "."))
#   
#   for(i in 1:gr) {
#     class.funs[1, i] <- -0.5 * t(m[i,]) %*% iV %*% (m[i,])
#     class.funs[2:(v+1) ,i] <- iV %*% (m[i,])
#   }
#   x.lda$class.funs <- class.funs
#   
#   return(x.lda)
# }
# ty.lda(d1,t1)
