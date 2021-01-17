set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
 
K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu

for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  
  
  # E???s t e p : Computat ion o f t h e f r a c t i o n a l component a s s ig nme n t s
  bp <- exp(x %???% log ( t (mu) ) + (1-x ) %???% log (1-t (mu) ) )
  pm <- matrix( rep( pi ,N) , nrow=N, ncol=K)
  px <- bp??? pm
  z <- px / rowSums ( px )  

  #Log l i k e l i h o o d compu ta t ion .
  lhood <??? x %???% log ( t (mu) ) + (1???x ) %???% log(1???t (mu) ) 

  for (n in 1 :N){
    for ( k in 1 :K){
      llik[it] <??? llik [it] + z [n,k] ??? lhood [n,k] + log (pi[k])
    }
  }
  cat ('iteration:', it , 'loglikelihood:', llik[it],"\n")
  flush.console()
  ifelse( it>1 & llik[it] ??? llik[it???1]< minchange,
                stop ( "EM is converged " ) , llik[it] )
  
# M???s t e p : ML parame ter e s t im a t i o n from t h e d a t a and
#  f r a c t i o n a lcomponent a s sig nm e n t s
  
  pi <??? colSums (z)/N

  mu <??? t (z) %???% x/colSums (z)  

}
pi
mu
p lot ( l l i k [ 1 : i t ] , type="o" )  
