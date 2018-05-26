#載入套件
library(neuralnet)

#整理資料
data <- iris
data$setosa <- ifelse(data$Species == "setosa", 1, 0)
data$versicolor <- ifelse(data$Species == "versicolor", 1, 0)
data$virginica <- ifelse(data$Species == "virginica", 1, 0)
# Conditional Element Selection
# ifelse(test, yes, no)
# ifelse returns a value with the same shape as test which is filled with elements selected from either yes or no
# depending on whether the element of test is TRUE or FALSE.
#訓練模型
f1 <- as.formula('setosa + versicolor + virginica  ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width')
bpn <- neuralnet(formula = f1, data = data, hidden = c(2,4),learningrate = 0.01,linear.output = F)
print(bpn)

#圖解BP
plot(bpn)
#Keeping a linear output in the final layer of a neural network is normally 
#used in regression settings only; in classification settings, 
#such as yours, the correct choice 
#is to apply the activation function to the output neuron(s) as well.