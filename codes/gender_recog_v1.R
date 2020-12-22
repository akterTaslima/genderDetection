library(randomForest);
library(rpart);

dataset = read.csv("voice.csv");
# plot(dataset)

num_row = nrow(dataset);
num_col = ncol(dataset);
N = num_row

Eigen_Constant = 10e-10
effective_feat = 0;

selection = sample(c(0,1), size = num_row, replace = T, prob = c(0.5,0.5))
train_data = dataset[selection==0,]
test_data = dataset[selection==1,]

#train_data=scale(train_data);
#test_data=scale(test_data);

X = train_data[,1:(num_col-1)]
X=scale(X);
Y = train_data[,num_col]
Y_01 = rep(NaN, length=length(Y))
for(i in 1:length(Y)){
  Y_01[i]= ifelse(Y[i]=="male", 2, 1);
}
# plot(train_data, col=Y_01+1)

#CART: Decision Tree
#X = scale(X)  # make columns 0 mean
X=as.matrix(X);
df = data.frame(X)
Y_01=as.factor(Y_01);
#X = cbind(X,Y_01);
fit = rpart(Y_01 ~ X,method="class",minbucket=1,cp=0)	
# fit a decision tree with min terminal size 1 an no purity improvement requirement
plot(fit)	      	       		       # look at complex tree we built

post(fit,file="gender_tree.ps")

pred = predict(fit,df,type="vector")		# test out the classifier on the training data
trainerrors = sum(pred != Y_01)			# great result!!!  (so what)
trainerrors

Xtest = test_data[,1:(num_col-1)]
Xtest=scale(Xtest);
Ytest = test_data[,num_col]
Ytest_01 = rep(NaN, length=length(Ytest))

for(i in 1:length(Ytest)){
  Ytest_01[i]= ifelse(Ytest[i]=="male", 2, 1);
}
Xtest=as.matrix(Xtest);
Ytest_01=as.factor(Ytest_01);
#Xtest = cbind(Xtest,Ytest_01);
X=Xtest;
df = data.frame(X)
pred2 = predict(fit,df, type = "vector")		# get predicted results from model learned above
testerrors = sum(pred2 != Ytest_01)			# about 1/2 right! (as you should expect)
testerrors


