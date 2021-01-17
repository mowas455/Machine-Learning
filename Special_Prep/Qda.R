data=iris

two.group.quadratic.classification <- function(data, grouping, newdata) {
  dat.split <- split(data, grouping)
  g1 <- as.data.frame(dat.split[1])
  g2 <- as.data.frame(dat.split[2])
  g1.means <- apply(g1, 2, mean)
  g2.means <- apply(g2, 2, mean)
  g1.covar <- cov(g1)
  g2.covar <- cov(g2)
  
  prediction <- apply(newdata, 1, function(y) {
    d2.y1 <- (y - g1.means) %*% solve(g1.covar) %*% (y - g1.means)
    d2.y2 <- (y - g2.means) %*% solve(g2.covar) %*% (y - g2.means)
    ifelse(d2.y1^2 > d2.y2^2, 2, 1)
  })
  
  class.table <- table(grouping, prediction, dnn = c('Actual Group','Predicted Group'))
  pred.errors <- sum(diag(t(apply(class.table, 2, rev)))) / dim(data)[1]
  results <- list('Prediction'=prediction, 'Table of Predictions'=class.table, 'Error Rate'=pred.errors)
  
  return(results)
}
two.group.quadratic.classification(data[,-5],as.factor(data[,5]),data[,-5])

# it works for binary classes but it need some alteration for multinomail classes



#logistic Regression from the scratch
#https://rpubs.com/junworks/Understanding-Logistic-Regression-from-Scratch