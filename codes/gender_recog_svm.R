stt = Sys.time()
setwd("~/Downloads/IU/Spring 2017/Data Mining/Project")
library("quadprog");  # we do this by quadratic programming and need library

dataset = read.csv("voice.csv");

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

#svm
Eigen_Constant = 10e-10
effective_feat = 0;

X = rbind(male_train_data[,1:(num_col-1)], female_train_data[,1:(num_col-1)])
# male = +1, female= -1
Y = c( rep(1, length= nrow(male_train_data)), rep(-1, length= nrow(female_train_data)) )
N = length(Y)

# pairs(X, col=Y+3)


X = scale(X)  # make columns 0 mean
S = t(X) %*% X/N;  # the sample covariance matrix
svd =  svd(S);  # take the singular value decomposition S = UDU^t
d = svd$d;  # d is diag(D)  # only the first r should be different from 0.
U = svd$u;  # columns of U are the "loadings" or independent directions

for (i in 1:length(d)){
  if( d[i] >= Eigen_Constant)
    effective_feat = effective_feat + 1;
}

Z = X %*% U[,1:effective_feat];  # transform Y to a new lower dim matrix that removes the redundancy
#pairs(Z,col=Y+3)


# svm(linear, poly)
##################################################
confusion_matrix = matrix(0,nrow = 2, ncol = 2)
NumWeight = ncol(Z);

dim = NumWeight + 1 + N;
PosBias = NumWeight + 1;

D = diag(1, dim); # the penalty is the sum of square of all variables except b which gets only a small penalty to
# keep the D matrix positive definite
D[PosBias , PosBias] = .001;    # this is the kludge for b
d_svm = rep(0,dim);
A = matrix(0,2*N,dim);
A[1:N,1:NumWeight] = Z*Y;
A[1:N,PosBias] = Y;
A[1:N,(PosBias+1):dim] = diag(N);
A[(N+1):(2*N),(PosBias+1):dim] = diag(N);
b = rep(0,2*N)
b[1:N] = 1;
result = solve.QP(D,d_svm,t(A),b);  # painless QP result from package!!

what = result$solution[1:NumWeight];  # margin-maximizing w
bhat = result$solution[PosBias];    # margin-maximizing b

# test-on train
predict_cls = rep(NaN, length = N )

X = scale(X)
for(t in 1:N){
  Z = X[t,] %*% U[,1:effective_feat];
  # Z = as.matrix(X[t,]) %*% U[,1:effective_feat];
  #Z = as.matrix(X[t,])
  predict_cls[t] = sign(Z%*%what + bhat);
  
  # True-Positive
  if (Y[t] == -1 && predict_cls[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  # False-Positive
  else if (Y[t] == -1 && predict_cls[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  # True-Negative
  else if (Y[t] == +1 && predict_cls[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  # False-Negative
  else if (Y[t] == +1 && predict_cls[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix
(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)

Sys.time()-stt
stt = Sys.time()

# test-on test
X = rbind(male_test_data[,1:(num_col-1)], female_test_data[,1:(num_col-1)])
# male = +1, female= -1
Y = c( rep(1, length= nrow(male_test_data)), rep(-1, length= nrow(female_test_data)) )
N = length(Y)

predict_cls = rep(NaN, length = N )

X = scale(X)
for(t in 1:N){
  Z = X[t,] %*% U[,1:effective_feat];
  # Z = as.matrix(X[t,]) %*% U[,1:effective_feat];
  #Z = as.matrix(X[t,])
  predict_cls[t] = sign(Z%*%what + bhat);

  # True-Positive
  if (Y[t] == -1 && predict_cls[t] == -1){
    confusion_matrix[1,1] = confusion_matrix[1,1]+1;
  }
  # False-Positive
  else if (Y[t] == -1 && predict_cls[t] == +1){
    confusion_matrix[1,2] = confusion_matrix[1,2]+1;
  }
  # True-Negative
  else if (Y[t] == +1 && predict_cls[t] == -1){
    confusion_matrix[2,1] = confusion_matrix[2,1]+1;
  }
  # False-Negative
  else if (Y[t] == +1 && predict_cls[t] == +1){
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
  else{
    confusion_matrix[2,2] = confusion_matrix[2,2]+1;
  }
}

confusion_matrix
(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)

Sys.time()-stt