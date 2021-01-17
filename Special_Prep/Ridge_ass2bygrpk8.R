raw_data = read.csv("C:/Users/Mounish/Desktop/ML/Lab1 Block 1/parkinsons.csv")

train_test_split = function(data, proportion){
  proportion_size = floor(proportion*nrow(data))
  train = data[sample(seq_len(nrow(data)), size = proportion_size),]
  test = data[-sample(seq_len(nrow(data)), size = proportion_size),]
  return(list(train=train,test=test))
}
exclude_columns = c("subject.","sex","age","test_time","total_UPDRS")#,"motor_UPDRS")
# Excludes from scaling
raw_data[,!names(raw_data) %in% exclude_columns] = apply(
  raw_data[,!names(raw_data) %in% exclude_columns],
  2,scale)


dataset = train_test_split(raw_data, 0.6)

exclude_x_columns = c("subject.","sex","age","test_time","motor_UPDRS","total_UPDRS")
x_train = dataset$train[,!names(dataset$train) %in% exclude_x_columns]
y_train = dataset$train[,names(dataset$train) == "motor_UPDRS"]
x_test = dataset$test[,!names(dataset$test) %in% exclude_x_columns]
y_test = dataset$test[,names(dataset$test) == "motor_UPDRS"]

loglikelihood = function(x, y, w, dispersion){
  n = dim(x)[1] #for every datapoint
  base = n*log(1/(abs(dispersion)*sqrt(2*pi)))
  exponent = -sum((y - t(t(w)%*%t(x)))**2) / (2*(dispersion**2))
  return(base + exponent)
}


ridge = function(par, x, y, lambda){
  m = dim(x)[2] #for every parameter
  w = as.matrix(par[1:(length(par)-1)])
  dispersion = par[length(par)]
  #Avoids 0 value without need for optimizer restriction
  if(isTRUE(all.equal(dispersion,0))){dispersion = dispersion + 0.0000001}
  exponent = - lambda * sum( w**2) / (2 * (dispersion**2))
  base = m*log(sqrt(lambda) / (2 * (dispersion**2)))
  reg = base + exponent
  return(loglikelihood(x, y, w, dispersion) + reg)
}


ridgeOpt = function(lambda, x, y){
  x=as.matrix(x)
  y=as.matrix(y)
  #Random Parameter initilization
  w = as.matrix(rnorm(dim(x)[2], mean = 0, sd = 0.01))
  sigma = runif(1, 0.001, 1)
  result = optim(par = c(w,sigma),
                 ridge,
                 x=x,
                 y=y,
                 lambda=lambda,
                 method = "BFGS",
                 control=list(fnscale=-1) #Maximizing instead of minimizing
  )
  return(result)
}

ridge_fit = ridgeOpt(1000, x_train, y_train)
ridge_fit

D = function(lambda, params, x, y){
  w = as.matrix(params)
  x = as.matrix(x)
  y = as.matrix(y)
  df = x %*% solve(t(x) %*% x + (lambda * diag(length(w)))) %*% t(x)
  return(sum(diag(df)))
}
D(lambda = 0.2,
  params = ridge_fit$par[1:(length(ridge_fit$par)-1)],
  x = x_train,
  y = y_train)
get_mean_square_error = function(params, x, y){
  w = as.matrix(params)
  x = as.matrix(x)
  y = as.matrix(y)
  mean_err = mean((y - t(t(w)%*%t(x)))**2)
  return(mean_err)
}

evaluate_performance = function(lambdas, x_train, y_train, x_test, y_test){
  train_results = c()
  test_results = c()
  for(i in 1:length(lambdas)){
    ridge_fit = ridgeOpt(lambdas[i], x_train, y_train)
    #Evaluating on train
    train_results = c(train_results,get_mean_square_error(
      params = ridge_fit$par[1:(length(ridge_fit$par)-1)],
      x_train,
      y_train))
    #Evaluating on test
    test_results = c(test_results, get_mean_square_error(
      params = ridge_fit$par[1:(length(ridge_fit$par)-1)],
      x_test,
      y_test))
  }
  result_frame = data.frame(train_results=train_results, test_results=test_results)
  rownames(result_frame) = lambdas
  return(result_frame)
}
lambdas = c(1, 100, 1000)
evaluate_performance(lambdas, x_train, y_train, x_test, y_test)
AIC = function(lambda, par, x, y){
  # AIC = -2(log-likelihood) + 2K
  loglik = ridge(par, x, y, lambda)
  params = par[1:(length(par)-1)]
  freedom = D(lambda, params, x, y)
  return(-2*loglik + 2*freedom)
}
evaluate_AIC = function(lambdas, x, y){
  AIC_results=c()
  for(i in 1:length(lambdas)){
    ridge_fit = ridgeOpt(lambdas[i], x, y)
    AIC_results=c(AIC_results,AIC(lambda=lambdas[i], par=ridge_fit$par, x=x, y=y))
  }
  result_frame = data.frame(AIC_SCORE=AIC_results)
  rownames(result_frame) = lambdas
  return(result_frame)
}
complete_x = rbind(x_train, x_test)
complete_y = c(y_train, y_test)
evaluate_AIC(lambdas = lambdas, x = complete_x, y = complete_y)
