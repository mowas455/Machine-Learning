
data <- read.csv("C:/Users/Mounish/Desktop/ML/Lab2 Block1/communities.csv")

# 1.
# Scale the data
data[, -c(101)] = scale(data[, -c(101)])

# Calculate the mean of every feature and compute the centered data matrix:
mean_vector <- c()
for(i in 1:101){
  mean_vector[i] <- mean(data[,i])
}
centered_data <- matrix(0, nrow = nrow(data), ncol= ncol(data))
for(i in 1:ncol(data)){
  for(j in 1:nrow(data)){
    centered_data[j,i] <- data[j,i]-mean_vector[i]
  }
}

#compute the covariance matrix
covar_matrix <- t(centered_data) %*% centered_data * (1/nrow(data))

#calculate eigenvalues & eigenvector

eigenvalues <- eigen(covar_matrix)$values

eigenvector <- eigen(covar_matrix)$vector

# Calculate how many features are needed to explain 95% of the variance of the data
cumul <- 0
i <- 0
while(cumul<95){
  i <- i+1
  cumul <- cumul + eigenvalues[i]
}
cat(i,"features are needed to explain 95% of the variance in the data")
proportion_firstPCs <- sum(eigenvalues[1:2])


# 2.

# Repeat PCA analysis by using prcomp() function and make the score plot of the first principle component.
res <- prcomp(data)
lambda <- res$sdev^2
lambda

# lambda
sprintf("%2.3f", lambda/sum(lambda)*100)
screeplot(res, main="Variance explained by principal components")

# Plot the first principal component
U <- res$rotation
plot(U[,1], main ="Traceplot, PC1")

# 5 largest contributing factors
U_abs <- abs(res$rotation)
top_features <- tail(order(U_abs[,1]), 5)
plot(U_abs[,1], main = "Traceplot by absolute value, PC1")

# Plot of the PC scores in the coordinates (PC1, PC2) in which the color of
#the points is given by ViolentCrimesPerPop.

library(ggplot2)
df <- data.frame(res$x[,1], res$x[,2], data[,101])
colnames(df) <- c("PC1", "PC2", "ViolentCrimesPerPop")
ggplot(df, aes(x=PC1, y=PC2, color=ViolentCrimesPerPop))+
  geom_point()+
  labs(title="Score plot of PC1 and PC2 with respect to ViolentCrimesPerPop")


# 3.

# Assume a second order polynomial regression model in 
#which ViolentCrimesPerPop is target and PC1 is the feature.

Pr_comp1<- res$x[,1]
regress <- lm(data[,101]~poly(Pr_comp1, degree = 2))
summary(regress)


# Sscatterplot of the target versus the feature and present also the predicted values in this plot.

df <- data.frame(cbind(Pr_comp1,data[,101]))

colnames(df) <- c("PC1", "ViolentCrimesPerPop")
plot2 <- ggplot(data = df, aes(x= PC1, y=ViolentCrimesPerPop))+
  geom_point(col="red")+
  geom_point(aes(x= Pr_comp1 , y= regress$fitted.values), col = "dark green")
plot2

library(boot)

# Parametric bootstrap to estimate the confidence and prediction bands from the model from step 3 and add these bands into the plot from step 3.

df_ordered <- df[order(df$PC1),] # this is data2 in the slides. Ordered data with respect to the feature PC1
#generate the data from a our data and model

rng <- function(df, regress){
  data1=data.frame(ViolentCrimesPerPop = df$ViolentCrimesPerPop, PC1 = df$PC1)
  n <- length(data[,101])
  data1$ViolentCrimesPerPop <- rnorm(n, predict(regress, newdata=data1), sd(regress$residuals))
  return(data1)
}




# Predict from the newly generated data

f1 <- function(data1){
  res <- lm(data1$ViolentCrimesPerPop~poly(data1$PC1, degree = 2), data=data1)
  crime_predict <- predict(res, newdata =df_ordered)
  return(crime_predict)
}

# bootstrap

bootstrap <- boot(df_ordered, statistic = f1, R=1000, mle = regress, ran.gen = rng, sim="parametric")
e=envelope(bootstrap) #compute confidence bands: confidence in regression
fit <- lm(df_ordered$ViolentCrimesPerPop~poly(df_ordered$PC1, degree = 2), data = df_ordered)
crime_pred <- predict(fit)
plot(df_ordered$PC1, df_ordered$ViolentCrimesPerPop, pch = 21, bg="orange")
points(df_ordered$PC1, crime_pred, type="l")


# Enveloppe is calculated from the predicted model by adding and substracing away from the fit.
points(df_ordered$PC1, crime_pred + e$point[2,], type="l", col="blue")
points(df_ordered$PC1, crime_pred - e$point[1,], type="l", col="blue")


# Prediction bands: confidence in future data points
mle <- lm(df_ordered$ViolentCrimesPerPop~poly(df_ordered$PC1, degree = 2), data = df_ordered)

f2 <- function(data1){
  res <- lm(data1$ViolentCrimesPerPop~poly(data1$PC1, degree = 2), data = data1)
  crime <- predict(res, newdata = df_ordered)
  n <- length(df_ordered$ViolentCrimesPerPop)
  predicted_crimes <- rnorm(n, crime, sd(mle$residuals))
  return(predicted_crimes)
}
results <- boot(df_ordered, statistic = f2, R = 1000, mle = mle, ran.gen = rng, sim = "parametric")
e2 <- envelope(results, level = 0.95)


points(df_ordered$PC1, e2$point[2,], type="l", col="red")
points(df_ordered$PC1, e2$point[1,], type="l", col="red")
 
