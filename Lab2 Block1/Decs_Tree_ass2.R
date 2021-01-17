library(tree)
library(rpart)

data<-read.csv("C:/Users/Mounish/Desktop/ML/Lab2 Block1/bank-full.csv",stringsAsFactors = TRUE,sep=";",header=TRUE)
data=data[,-12]
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]
str(train)

#Training the Model using train dataset
model1<-tree(y~.,train)
model2<-tree(y~.,train,minsize=7000)
model3<-tree(y~.,train,mindev=0.0005)

#Training the Model using validation dataset
model_val_1<-predict(model1,valid,type="class")
model_val_2<-predict(model2,valid,type="class")
model_val_3<-predict(model3,valid,type="class")

confm_val_1<-table(model_val_1,valid$y)
confm_val_2<-table(model_val_2,valid$y)
confm_val_3<-table(model_val_3,valid$y)

error_val_1<-1-(sum(diag(confm_val_1))/sum(confm_val_1))
error_val_2<-1-(sum(diag(confm_val_2))/sum(confm_val_2))
error_val_3<-1-(sum(diag(confm_val_3))/sum(confm_val_3))

errors_validation<-data.frame(cbind(error_val_1,error_val_2,error_val_3))

#errors_train
misclass_tr_a<-misclass.tree(model1,detail=FALSE)
misclass_tr_b<-misclass.tree(model2,detail=FALSE)
misclass_tr_c<-misclass.tree(model3,detail=FALSE)

error1<-misclass_tr_a/nrow(train)
error2<-misclass_tr_b/nrow(train)
error3<-misclass_tr_c/nrow(train)

errors_tr<-as.data.frame(cbind(error1,error2,error3))

errors_tr
errors_validation

train_dev<-c()
valid_dev<-c()
for(i in 2:50)
{
  prune_train<-prune.tree(model3,best=i)
  prune_pred<-predict(prune_train,newdata=na.tree.replace(valid),type="tree")
  train_dev[i]<-deviance(prune_train)
  valid_dev[i]<-deviance(prune_pred)
}
plot(2:50,train_dev[2:50],type ="b",col="blue",ylim = c(8000,12000),xlab="First 50 leaves" )
points(2:50,valid_dev[2:50],type="b",col="red")

best_node<-which.min(valid_dev)
best_node

as.character(summary(prune_train)$used)
final_opt_tree<-prune.tree(model3,best=best_node)
pred_optimal<-predict(final_opt_tree,newdata = test,type="class")
test_confm<-table(pred_optimal,test$y)

#confusion Matrix
test_confm
test_error<-1-sum(diag(test_confm))/sum(test_confm)

#misclassification error

test_error
loss_tree<-rpart(y~.,data=train,method="class",
                 parm=list(loss=matrix(c(0,1,5,0),byrow=TRUE,nrow=2)),
                 control = tree.control(nobs=nrow(train),mindev=0.0005))

pred_loss<-predict(loss_tree,test,type="class")
loss_confm<-table(pred_loss,test$y)
error_loss<-1-sum(diag(loss_confm))/sum(loss_confm)
error_loss
loss_confm

library(MASS)
library(e1071)

fit<-naiveBayes(y~.,data=train)
pred_naive<-predict(fit,newdata=test,type="raw")
pred_optimal_2<-predict(final_opt_tree,newdata = test)
classify_tree<-list()
classify_naive<-list()
naive_conmat<-list()
tree_conmat<-list()
tpr_naive<-c()
fpr_naive<-c()
tpr_tree<-c()
fpr_tree<-c()
pi<-seq(0.05,0.95,0.05)

for(i in 1:length(pi))
{
  classify_tree<-factor(ifelse(pred_optimal_2[,2]>pi[i],"yes","no"),levels = c("no","yes"))
  classify_naive<-factor(ifelse(as.data.frame(pred_naive[,2])>pi[i],"yes","no"),levels = c("no","yes"))
  naive_conmat<-table("true"=test$y,"predicted"=classify_naive)
  tree_conmat<-table("true"=test$y,"predicted"=classify_tree)
  #naive_conmat[i]<-list(table(test$y,classify_naive[[i]]))
  #tree_conmat[i]<-list(table(test$y,classify_naive[[i]]))
  tpr_naive[i]<-naive_conmat[2,2]/sum(naive_conmat[2,2],naive_conmat[2,1])
  fpr_naive[i]<-naive_conmat[1,2]/sum(naive_conmat[1,1],naive_conmat[1,2])
  tpr_tree[i]<-tree_conmat[2,2]/sum(tree_conmat[2,2],tree_conmat[2,1])
  fpr_tree[i]<-tree_conmat[1,2]/sum(tree_conmat[1,2],tree_conmat[1,1])
  #i=i+1
}
plot(fpr_tree,tpr_tree,type="l",col="red")
lines(fpr_naive,tpr_naive,type="l",col="blue")
legend('bottomright',legend=c('Optimal_tree','Naive_Bayes_Model'),lty=1:2,col=c("red","blue"))

