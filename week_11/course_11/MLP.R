#An Introduction To Deep Learning 
#Chapter 4 Code Examples 

#Clear the workspace 
rm(list = ls())

#Loading the required packages
require(monmlp)
require(Metrics)

#MultiLayer Perceptron Code
x <- as.matrix(seq(-10, 10, length = 100))
y <- logistic(x) + rnorm(100, sd = 0.2)   
#monmlp::logistic
#Computes the logistic sigmoid function. 
#Used as a hidden layer transfer function for nonlinear MLP or MONMLP models.
#
#Plotting Data
plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")

#Fitting Model
mlpModel <- monmlp.fit(x = x, y = y, hidden1 = 3, monotone = 1,
                       n.ensemble = 15, bag = TRUE)
#number of ensemble members to fit.
mlpModel <- monmlp.predict(x = x, weights = mlpModel)

#Plotting predicted value over actual values
for(i in 1:15){
  lines(x, attr(mlpModel, "ensemble")[[i]], col = "red")
}

cat ("MSE for Gradient Descent Trained Model: ", mse(y, mlpModel))
###????mlpModel和attr(mlpModel,"ensemble")的數值有什麼差別???
plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")
for(i in 1:15){
  lines(x, attr(mlpModel, "ensemble")[[i]], col = "red")
}
lines(x, mlpModel, lwd = 4, col = "blue")
#因為Fit an individual model or ensemble of MLP or MONMLP regression model的目的
#為了要minimize a least squares cost function，所以圖形化出來預測結果自然會
#和logistic(x)的曲線很接近

####################################################################
#Conjugate Gradient Trained NN
#install.packages('RSNSS')
require(RSNNS)

#About mlp function https://www.rdocumentation.org/packages/RSNNS/versions/0.4-10/topics/mlp
conGradMLP <- mlp(x = x, y = y, 
                  size = (2/3)*nrow(x)*2, 
                  maxit = 200, 
                  learnFunc = "SCG")
#size  	number of units in the hidden layer(s)
#maxit	maximum of iterations to learn
#learnFunc	the learning function to use

y_h <- predict(conGradMLP, x)

cat ("MSE for Conjugate Gradient Descent Trained Model: ", mse(y_h, y))

