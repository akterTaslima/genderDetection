dataset = read.csv("voice.csv");
#dataset=as.matrix(dataset)
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

X = rbind(male_train_data[,1:(num_col-1)], female_train_data[,1:(num_col-1)], male_test_data[,1:(num_col-1)], female_test_data[,1:(num_col-1)])
Y = c( rep(1, length= nrow(male_train_data)), rep(-1, length= nrow(female_train_data)), rep(1, length= nrow(male_test_data)), rep(-1, length= nrow(female_test_data)) )
N = length(Y)

X = cbind(X, Y);

male_data= male_data[,1:num_col-1]
female_data = female_data[, 1:num_col-1]

# parallel coordinate plot : Male data
parcoord(male_data, col=rainbow(nrow(male_data)), var.label=FALSE, main = "Parallel Coord Plot for Male dataset")

# parallel coordinate plot : Female data
parcoord(female_data, col=rainbow(nrow(female_data)), var.label=FALSE, main = "Parallel Coord Plot for female dataset")

# parallel coordinate plot : Entire data
parcoord(X, col=rainbow(nrow(X)), var.label=FALSE, main = "Parallel Coord Plot for Full dataset")


# #pairs(dataset)
# #plot(dataset[,1], dataset[, 13]);
# plot(dataset[,1], dataset[,13], col=Y+3, xlab = "MeanFreq", ylab = "MeanFun");
# 
# 
# ## 'Spider' or 'Radar' plot: Male Dataset
# stars(male_data, locations = c(0, 0), radius = FALSE,
#       key.loc = c(0, 0), main = "Voice Recognition Male Dataset", lty = 2, col.lines = 1:nrow(male_data))
# 
# ## 'Spider' or 'Radar' plot: Female Dataset
# stars(female_data, locations = c(0, 0), radius = FALSE,
#       key.loc = c(0, 0), main = "Voice Recognition Female Dataset", lty = 2, col.lines = 1:nrow(female_data))
# 
# ## 'Spider' or 'Radar' plot: Full Dataset
# stars(X, locations = c(0, 0), radius = FALSE,
#       key.loc = c(0, 0), main = "Voice Recognition Entire Dataset", lty = 2, col.lines = 1:nrow(X))
