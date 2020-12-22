# plotting in R should make imaginative and practical use of plotting parameters including
# color, plot, symbol, symbol size, line width, line type.  Usually the goal is not
# about creating "eye candy" but rather making the nature of your data clear.  The
# various plotting parameters are cues the eye uses in *grouping* the data.   

n = 200;
x = runif(n);
y = runif(n);


plot(x,y,pch=1:25,col=1:n,cex=seq(.5,2,length=n));


l = 100;
n = 30;
X = matrix(runif(n*l,min=-1,max=1),nrow=n,ncol=l);
for (i in 1:n) X[i,] = cumsum(cumsum(X[i,]));
matplot(t(X),type='l',lty=1:n,col=1:n,lwd=seq(1,2,length=n))
