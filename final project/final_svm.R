#----------- SVM (not suitalbe)---------

# Import library  
library(e1071)



# Create x and y
x <- subset(input, select = -g_view)
y <- input[,1]
input

# Build model
model <- svm(g_view ~ ., data = input)

# Predict with the model
pred_result <- predict(model, x)

# View and visualize the result
table(pred_result, y)
View(table(pred_result, y))



#------------xgboost-------
library(xgboost)
library(Matrix)
input$"g_view" <- as.factor(input$"g_view")
for(i in 1:ncol(input)){
  input[,i] <- as.factor(input[,i])
}
train <- sparse.model.matrix(g_view ~ ., data = input)
