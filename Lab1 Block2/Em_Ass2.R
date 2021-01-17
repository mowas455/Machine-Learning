set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)

# plots
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")


bern_em = function(K){
  max_it <- 100 # max number of EM iterations
  min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
  N=1000 # number of training points
  D=10 # number of dimensions
  x <- matrix(nrow=N, ncol=D) # training data
  # Producing the training data
  for(n in 1:N) {
    k <- sample(1:3,1,prob=true_pi)
    for(d in 1:D) {
      x[n,d] <- rbinom(1,1,true_mu[k,d])
    }
  }
  
  
  K= K # number of guessed components
  z <- matrix(nrow=N, ncol=K) # fractional component assignments
  pi <- vector(length = K) # mixing coefficients
  mu <- matrix(nrow=K, ncol=D) # conditional distributions
  llik <- vector(length = max_it) # log likelihood of the EM iterations
  # Random initialization of the parameters
  pi <- runif(K,0.49,0.51)
  pi <- pi / sum(pi)
  for(k in 1:K) {
    mu[k,] <- runif(D,0.49,0.51)
  }
  #pi # Random vector of size K proportions
  #mu # random matrix of size KxD
  for(it in 1:max_it) {
    # E-step: Computation of the fractional component assignments
    # Your code here
    p_x = c()
    #For every K
    for(k in 1:K){
      p_x_n = c()
      #For every datapoint
      for(n in 1:N){
        berk_k_n = (mu[k,]**x[n,]) * ((1-mu[k,])**(1-x[n,]))
        p_x_n = cbind(p_x_n,pi[k]*prod(berk_k_n))
      }
      p_x = cbind(p_x, colSums(p_x_n))
    }
    z = (pi*p_x)/rowSums(pi*p_x)
    #Log likelihood computation.
    # Your code here
    components = c()
    for(n in 1:N){
      for(k in 1:K){
        likelihood = log(pi[k]) + sum((x[n,] * log(mu[k,])) + ((1-x[n,]) * log(1-mu[k,])))
        proportion = z[n,k]
        components = c(components, proportion * likelihood)
      }
    }
    llik[it] = sum(components)
    #cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    flush.console()
    # Stop if the lok likelihood has not changed significantly
    # Your code here
    if(isTRUE(abs(llik[it] - llik[it-1])< min_change)){break}
    #M-step: ML parameter estimation from the data and fractional component assignments
    ## adjust parameters to fit points assigned to them
    # Your code here
    #for all k do this
    
    
    for(k in 1:K){
      pi[k] = sum(z[,k]/N)
    }
    # for all k, i do this
    for(k in 1:K){
      for(i in 1:D){
        mu[k,i] = sum(z[,k]*x[,i])/sum(z[,k])
      }
    }
  }
  #pi
  #mu
  plot(mu[1,], type="o", col="blue", ylim=c(0,1),main=paste("K=",as.character(K)))
  for(i in 1:K){
    points(mu[i,], type="o")
  }
  plot(llik[1:it], type="o")
}
bern_em(2)
bern_em(3)
bern_em(4)
