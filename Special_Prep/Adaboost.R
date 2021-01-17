#For Bagging
#https://rpubs.com/nurakawa/bagging

# Adaboost
irisdata=iris
labels=as.numeric(irisdata$Species=="setosa")
head(irisdata)

# logistic regression
glmt1 <- glm(labels~irisdata$Sepal.Length, family=binomial)
coefficients(summary(glmt1))

# To check the accuracy
fit1 <- fitted(glmt1)
accuracytable=table(fit1>0.5, labels)
accuracy=(accuracytable[1, 1]+accuracytable[2, 2])/length(labels)
accuracy

## step 1 of the algorithm
X=irisdata$Sepal.Length # the training set is composed by X and y
y=labels # in this case, y=0/1 and not, as before, y=-1/1


## step 2 of the algorithm
T=10 # number of steps
m=length(y) # number of cases
classifier=matrix(data = NA, nrow = length(y), ncol = T) # where classifiers are to be stored
epsilon=0 # initialise epsilon
alpha=0 # initialise alpha
for (t in seq(1:T)){
  
  # construct distribution: D_{t}(i)=D_{t-1}(i)*F_i/Z
  if (t==1){
    D=rep(1/m, m) # particular case, D_1
  } else
  {
    for (i in seq(1:length(y))){
      if (h[i]==y[i]) {F[i]=exp(-alpha[t-1])   } else {F[i]=exp(alpha[t-1])} # F_i(for t-1)
      D[i]=D[i]*F[i] # D_{t}(i)=D_{t-1}(i)*F_i/Z
    }}
  D=D/sum(D) # normalisation
  
  # build up classifier
  
  ### is this required? Should weights be treated as integers?
  # D=as.integer(D*10000) 
  ### is this required? Should weights be treated as integers?
  h=glm(y~X, family=binomial, weights=D)
  h = as.numeric(fitted(h)>0.5)
  classifier[,t]=h
  ### required if weights converted to integers
  # D=D/sum(D) 
  ### required if weights converted to integers
  
  
  # compute error associated with classifier and alpha
  epsilon[t]=sum(D[!(y==h)])
  alpha[t]=0.5*log((1-epsilon[t])/epsilon[t]) # alpha_{t}
  
}


### step 3 of the algorithm
finalclassifier=matrix(data = 0, nrow = length(y), ncol = 1)
classifier[classifier==0]=-1
labels[labels==0]=-1
for (t in seq(1:T)){
  finalclassifier=finalclassifier+alpha[t]*classifier[, t]
}

finalclassifier=sign(finalclassifier)

accuracytable=table(finalclassifier, labels)
accuracy2=(accuracytable[1, 1]+accuracytable[2, 2])/length(labels)
accuracy2