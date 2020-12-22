library(randomForest);
starts=Sys.time();
dataset = read.csv("voice.csv");

num_row = nrow(dataset);
num_col = ncol(dataset);
N = num_row

selection = sample(c(0,1), size = num_row, replace = T, prob = c(0.5,0.5))
train_data = dataset[selection==0,]
test_data = dataset[selection==1,]

# Generating Train Dataset and class
X = train_data[,1:(num_col-1)]
Y = train_data[,num_col]
Y_01 = rep(NaN, length=length(Y))
for(i in 1:length(Y)){
  Y_01[i]= ifelse(Y[i]=="male", 1, -1);
}

# Generating Test Dataset and class
Xtest = test_data[,1:(num_col-1)]
Ytest = test_data[,num_col]
Ytest_01 = rep(NaN, length=length(Ytest))

for(i in 1:length(Ytest)){
  Ytest_01[i]= ifelse(Ytest[i]=="male", 1, -1);
}

# Random Forest::
Y_01=as.factor(Y_01);
Ytest_01=as.factor(Ytest_01);
X = train_data[,1:(num_col-1)]
X = cbind(X,Y_01);
Xtest = test_data[,1:(num_col-1)]
Xtest = cbind(Xtest,Ytest_01);

# Building Random Forest
model = randomForest(Y_01 ~ . , data = X, ntree=2000)

# test on training data
pred = predict(model, newdata = X)
error_rate = sum(pred != Y_01);
error_rate
print((error_rate/nrow(X))*100)
confusion_matrix = matrix(0,nrow = 2, ncol = 2)
# True-Positive
for(t in 1:nrow(X)){
  if (Y_01[t] == -1 && pred[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  else if (Y_01[t] == -1 && pred[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  else if (Y_01[t] == +1 && pred[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  else if (Y_01[t] == +1 && pred[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix
# test on test data
pred2 = predict(model, newdata = Xtest)
error_rate = sum(pred2 != Ytest_01);
print((error_rate/nrow(Xtest))*100)

confusion_matrix = matrix(0,nrow = 2, ncol = 2)
# True-Positive
for(t in 1:nrow(Xtest)){
  if (Ytest_01[t] == -1 && pred2[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  else if (Ytest_01[t] == -1 && pred2[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  else if (Ytest_01[t] == +1 && pred2[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  else if (Ytest_01[t] == +1 && pred2[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix
print(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
Sys.time()-starts
