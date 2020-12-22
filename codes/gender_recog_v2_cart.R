#setwd("~/Downloads/IU/Spring 2017/Data Mining/Project")
starts=Sys.time()
dataset = read.csv("voice.csv");
library("rpart");

num_row = nrow(dataset);
num_col = ncol(dataset);

male_data = dataset[dataset[,num_col]=="male",]
female_data = dataset[dataset[,num_col]=="female",]

selection = sample(c(0,1), size = nrow(male_data), replace = T, prob = c(0.5,0.5))
male_train_data = male_data[selection==0,]
male_test_data = male_data[selection==1,]

selection = sample(c(0,1), size = nrow(female_data), replace = T, prob = c(0.5,0.5))
female_train_data = female_data[selection==0,]
female_test_data = female_data[selection==1,]

X = rbind(male_train_data[,1:(num_col-1)], female_train_data[,1:(num_col-1)])
Y = c( rep(1, length= nrow(male_train_data)), rep(-1, length= nrow(female_train_data)) )
N = length(Y)

#CART: Decision Tree

X=as.matrix(X);
df = data.frame(X)

fit = rpart(Y ~ X,method="class",minbucket=1,cp=0)	
# fit a decision tree with min terminal size 1 an no purity improvement requirement
plot(fit)	      	       		       # look at complex tree we built
post(fit,file="gender_tree.ps", title = "Gender Classification Tree")

#Prune the Tree at minimum Xerror level
pfit<- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
post(pfit, file = "Pruned_gender_tree.ps", 
     title = "Pruned Gender Classification Tree")

pred = predict(fit,df,type="class")		# test out the classifier on the training data
trainerrors = sum(pred != Y)# great result!!!  (almost 100%)
trainerrors = (trainerrors/nrow(X))*100
print(trainerrors);
confusion_matrix = matrix(0,nrow = 2, ncol = 2)
# True-Positive
for(t in 1:nrow(X)){
  if (Y[t] == -1 && pred[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  else if (Y[t] == -1 && pred[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  else if (Y[t] == +1 && pred[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  else if (Y[t] == +1 && pred[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix

# test out the classifier on the test data
Xtest = rbind(male_test_data[,1:(num_col-1)], female_test_data[,1:(num_col-1)])
Ytest = c( rep(1, length= nrow(male_test_data)), rep(-1, length= nrow(female_test_data)) )
X=as.matrix(Xtest);

df = data.frame(X)
pred2 = predict(fit,df,type="class")		# get predicted results from model learned above
nrow(df)
testerrors = sum(pred2 != Ytest)		
testerrors = testerrors/nrow(X) * 100 
print(testerrors)

confusion_matrix = matrix(0,nrow = 2, ncol = 2)
# True-Positive
for(t in 1:nrow(X)){
  if (Ytest[t] == -1 && pred2[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  else if (Ytest[t] == -1 && pred2[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  else if (Ytest[t] == +1 && pred2[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  else if (Ytest[t] == +1 && pred2[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix
print(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)

Sys.time()-starts